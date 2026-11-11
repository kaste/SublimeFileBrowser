"""Derived file-size summaries, kept separate from status rendering."""
from __future__ import annotations

import os
import stat
import threading
import traceback
from typing import NamedTuple

import sublime
from sublime_plugin import EventListener


_cache = None
_TAG = 'FileBrowser.status_summaries'
_SETTING = {
    'marked': 'dired_marked_paths',
    'copied': 'dired_to_copy',
    'cut': 'dired_to_move',
}


def update(view, *, refresh=False):
    global _cache
    if _cache is None:
        _cache = SummaryCache(
            sublime.load_settings('dired.sublime-settings'),
            sublime.set_timeout,
            lambda target: target.run_command('dired_render_status'),
        )
    _cache.update(view, refresh=refresh)


def cached_size(view, kind, paths):
    """Read a matching aggregate without scheduling work or touching storage."""
    return _cache.cached_size(view, kind, paths) if _cache is not None else None


class StatusViewListener(EventListener):
    def on_close(self, view):
        if _cache is not None:
            _cache.forget(view)


def plugin_unloaded():
    global _cache
    if _cache is not None:
        _cache.stop()
        _cache = None


class SummaryCache:
    """Main-thread dataset cache with one coalescing, dedicated I/O worker.

    Copied/cut datasets are global; marked datasets belong to individual views.
    A refresh generation invalidates sizes even if path membership is unchanged.
    Only the latest pending request for each dataset is retained. A blocked OS
    call cannot be cancelled, but never blocks rendering or starts more workers.
    """
    def __init__(self, settings, schedule, render, *, measure=None):
        self.settings = settings
        self.schedule = schedule
        self.render = render
        self.measure = measure or total_size
        self.views = {}
        self.view_settings = {}
        self.datasets = {}
        self.generation = 0
        self.pending = {}
        self.condition = threading.Condition()
        self.worker = None
        self.stopped = False
        self.update_scheduled = False
        self.settings.add_on_change(_TAG, self.queue_update)

    def update(self, view, *, refresh=False):
        if self.stopped or not view.is_valid() or not view.settings().get('dired_path'):
            return
        if view.id() not in self.views:
            self.views[view.id()] = view
            self.view_settings[view.id()] = view.settings()
            view.settings().add_on_change(_TAG, self.queue_update)
        for kind in _SETTING:
            key = (kind, view.id() if kind == 'marked' else None)
            paths, root = self.identity(view, kind)
            previous = self.datasets.get(key)
            if previous and (previous.paths, previous.root) == (paths, root) and not refresh:
                continue
            self.generation += 1
            dataset = Dataset(paths, root, self.generation, None if paths else 0)
            self.datasets[key] = dataset
            with self.condition:
                self.pending.pop(key, None)
                if paths:
                    self.pending[key] = dataset
                    if self.worker is None:
                        self.worker = threading.Thread(
                            target=self.work, name='FileBrowser status summaries', daemon=True
                        )
                        self.worker.start()
                    self.condition.notify()

    def cached_size(self, view, kind, paths):
        key = (kind, view.id() if kind == 'marked' else None)
        dataset = self.datasets.get(key)
        root = view.settings().get('dired_path') if kind == 'marked' else None
        if dataset and (dataset.paths, dataset.root) == (tuple(sorted(paths)), root):
            return dataset.size
        return None

    def forget(self, view):
        self.views.pop(view.id(), None)
        settings = self.view_settings.pop(view.id(), None)
        if settings is not None:
            settings.clear_on_change(_TAG)
        key = ('marked', view.id())
        self.datasets.pop(key, None)
        with self.condition:
            self.pending.pop(key, None)

    def stop(self):
        with self.condition:
            self.stopped = True
            self.pending.clear()
            self.condition.notify()
        self.settings.clear_on_change(_TAG)
        for settings in self.view_settings.values():
            settings.clear_on_change(_TAG)
        self.views.clear()
        self.view_settings.clear()
        self.datasets.clear()

    def queue_update(self):
        if not self.stopped and not self.update_scheduled:
            self.update_scheduled = True
            self.schedule(self.flush)

    def flush(self):
        self.update_scheduled = False
        if self.stopped:
            return
        for view in list(self.views.values()):
            if not view.is_valid():
                self.forget(view)
                continue
            self.update(view)
            self.repaint(view)

    def identity(self, view, kind):
        settings = view.settings() if kind == 'marked' else self.settings
        paths = tuple(sorted(settings.get(_SETTING[kind]) or []))
        root = view.settings().get('dired_path') if kind == 'marked' else None
        return paths, root

    def work(self):
        while True:
            with self.condition:
                self.condition.wait_for(lambda: self.stopped or self.pending)
                if self.stopped:
                    return
                key = next(iter(self.pending))
                dataset = self.pending.pop(key)
            try:
                size = self.measure(dataset.paths)
            except Exception:
                traceback.print_exc()
                size = None
            with self.condition:
                if self.stopped:
                    return
            self.schedule(
                lambda key=key, dataset=dataset, size=size: self.publish(key, dataset, size)
            )

    def publish(self, key, dataset, size):
        current = self.datasets.get(key)
        if self.stopped or current is None or current.generation != dataset.generation:
            return
        kind, vid = key
        if vid is None:
            if tuple(sorted(self.settings.get(_SETTING[kind]) or [])) != dataset.paths:
                return
            views = list(self.views.values())
        else:
            view = self.views.get(vid)
            if view is None or not view.is_valid():
                return
            if self.identity(view, kind) != (dataset.paths, dataset.root):
                return
            views = [view]
        self.datasets[key] = dataset._replace(size=size)
        for view in views:
            self.repaint(view)

    def repaint(self, view):
        settings = view.settings()
        if (view.is_valid() and settings.get('dired_path')
                and not settings.get('dired_rename_mode')):
            self.render(view)


class Dataset(NamedTuple):
    paths: tuple[str, ...]
    root: str | None
    generation: int
    size: int | None


def total_size(paths):
    """Return a complete file total, or None for directories/unavailable data."""
    total = 0
    for path in paths:
        try:
            info = os.stat(path)
        except OSError:
            return None
        if stat.S_ISDIR(info.st_mode):
            return None
        total += info.st_size
    return total
