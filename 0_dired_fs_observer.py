'''This module creates a file system observer, which
    1. collect all open/expanded paths from all FileBrowser views and
    2. waiting for any changes in these paths (create/remove/modify file), and in case of such change
    3. schedule a refresh for corresponding view(s)

Filename of this module starts with 0_ because we want it being loaded before other FileBrowser
modules, so we can check if auto-refresh is functional.
The reason why we cannot check functionality here, but have to in the other module, is because
dired.py module is refreshing existing views on start-up, thus before checking presence of
auto-refresh, we must be sure that Refresh command is functional (i.e. dired.py is loaded).
In the other words, the algorithm is after we sure that
    1) all views are loaded (see dired.plugin_loaded),
    2) auto-refresh is presented,
then, finally,
    3) we add callback on change of dired_autorefresh setting (so user can disable it globally).
That is why this module must be loaded first, but its presence is checked in the other module.
'''

from __future__ import annotations
from itertools import chain
import os
import queue
import stat
import threading
import time
import traceback

import sublime

try:  # unavailable dependencies shall not break basic functionality
    from watchdog.observers import Observer
    from watchdog.observers.api import ObservedWatch
    from watchdog.events import FileSystemEventHandler, DirModifiedEvent, FileModifiedEvent
except ImportError:
    Observer = None
    FileSystemEventHandler = object

flatten = chain.from_iterable

REFRESH_TIMEOUT  = 1.0   # seconds: settly down time before we call `refresh`
SCHEDULE_REFRESH = 700   # milliseconds: interval for checking REFRESH_TIMEOUT
observer = None


def plugin_loaded():
    global observer
    if Observer:
        observer = ObservePaths()


def plugin_unloaded():
    global observer
    if observer:
        print('FileBrowser: shutting down file watcher:', observer.observer)
        observer.stop()
        observer = None


def refresh(views, erase_settings=False):
    '''
    views
        list of integers which are view.id(), can be empty
    erase_settings
        boolean, can be True after change of global setting dired_autorefresh
    '''
    if not views and not erase_settings:
        def is_dired(view): return view.settings().get("dired_path")
    else:
        def is_dired(view): return False

    for w in sublime.windows():
        for v in w.views():
            if v.id() in views or is_dired(v):
                if erase_settings:
                    v.settings().erase('dired_autorefresh')
                v.run_command('dired_refresh')


class ObservePaths(object):
    def __init__(self):
        self.observer = Observer()
        self.event_handler = ReportEvent()
        self.watches: dict[str, ObservedWatch] = {}
        self.paths: dict[sublime.ViewId, list[str]] = {}
        # Handling an event can block, e.g. checking a path on an unreachable network
        # share, so neither the main nor Sublime's shared worker thread does it.
        self.events: queue.Queue[tuple[str, object] | None] = queue.Queue()
        self.observer.start()
        threading.Thread(
            target=self.process_events, name='FileBrowser file watcher', daemon=True
        ).start()

    def post(self, event: str, payload: object):
        '''receiving args from common.emit_event, on any thread'''
        self.events.put((event, payload))

    def stop(self):
        self.events.put(None)

    def process_events(self):
        while (item := self.events.get()) is not None:
            try:
                self.dired_event_handler(*item)
            except Exception:
                traceback.print_exc()
        self.observer.stop()
        self.observer.join()

    def dired_event_handler(self, event, payload):
        def stop_watch(vid: sublime.ViewId):
            self.paths.pop(vid, None)
            rewatch_all()

        def add_paths(vid: sublime.ViewId, paths):
            for path in paths:
                if path and not os.path.isdir(path):
                    print(f"error(add_paths): {path} is not a directory")
            paths = [without_trailing_sep(p) for p in paths if os.path.isdir(p)]

            old_paths = self.paths.get(vid, [])
            self.paths.update({
                vid: sorted(p for p in set(old_paths + paths))
            })
            rewatch_all()

        def set_paths(vid: sublime.ViewId, paths):
            for path in paths:
                if path and not os.path.isdir(path):
                    print(f"error(set_paths): {path} is not a directory")
            paths = [without_trailing_sep(p) for p in paths if os.path.isdir(p)]

            self.paths.update({
                vid: sorted(paths)
            })
            rewatch_all()

        def remove_path(vid: sublime.ViewId, path):
            if path and not os.path.isdir(path):
                print(f"error(remove_path): {path} is not a directory")
            path_without_sep = without_trailing_sep(path)
            path_with_sep = os.path.join(path_without_sep, '')
            self.paths.update({
                vid: sorted(
                    p
                    for p in self.paths.get(vid, [])
                    if (
                        p != path_without_sep
                        and not p.startswith(path_with_sep)
                    )
                )
            })
            rewatch_all()

        def rewatch_all():
            next_set = set(flatten(self.paths.values()))
            # watchdog stops watching a deleted directory for good, even if it is
            # recreated, and an emitter may also die from an error.  Then we try to
            # watch the path anew, which drops it below if it is gone.
            running = {
                emitter.watch.path
                for emitter in self.observer.emitters
                if emitter.is_alive() and emitter.should_keep_running()
            }

            for p in self.watches.keys() - (next_set & running):
                self.observer.unschedule(self.watches.pop(p))
                self.event_handler.forget(p)

            failed = set()
            for p in sorted(next_set - self.watches.keys()):
                try:
                    self.watches[p] = self.observer.schedule(self.event_handler, p)
                except Exception as e:
                    print(f"FileBrowser: can't watch {p}: {e}")
                    failed.add(p)
                else:
                    self.event_handler.remember(p)
            drop_paths(failed)

        def drop_paths(paths):
            paths = set(paths)
            if paths:
                self.paths = {
                    vid: [p for p in view_paths if p not in paths]
                    for vid, view_paths in self.paths.items()
                }

        def toggle_watch_all(watch):
            '''watch is boolean or None, global setting dired_autorefresh'''
            views = self.paths.keys()
            if not watch:
                self.paths = {}
            sublime.set_timeout(lambda: refresh(views, erase_settings=(not watch)), 1)

        def ignore_view(vid: sublime.ViewId):
            self.event_handler.ignored_views.add(vid)

        def unignore_view(vid: sublime.ViewId):
            self.event_handler.ignored_views.discard(vid)

        case = {
            'set_paths': lambda: set_paths(*payload),
            'add_paths': lambda: add_paths(*payload),
            'remove_path': lambda: remove_path(*payload),
            'view_closed': lambda: stop_watch(payload),
            'stop_watch': lambda: stop_watch(payload),
            'toggle_watch_all': lambda: toggle_watch_all(payload),
            'ignore_view': lambda: ignore_view(payload),
            'watch_view': lambda: unignore_view(payload),
        }
        case[event]()
        # hand out a copy, watchdog's thread iterates it while we mutate ours
        self.event_handler.paths = dict(self.paths)


class ReportEvent(FileSystemEventHandler):
    def __init__(self):
        self.paths: dict[sublime.ViewId, list[str]] = {}
        self.ignored_views: set[sublime.ViewId] = set()
        self.scheduled_views: dict[sublime.ViewId, float] = {}
        self.ticking = False
        # directory -> {file path -> signature}, see `signature`
        self.signatures: dict[str, dict[str, tuple]] = {}
        self.lock = threading.Lock()

    def on_any_event(self, event):
        '''
        File system event received from watchdog module,
        not to be confused with the events our views post via common.emit_event
        dir(event) = ['event_type', 'is_directory', 'key', 'src_path']
        '''
        if isinstance(event, DirModifiedEvent):
            # change of access time may cause modified event, which can be safely ignored
            # actual changes will fire the corresponding event types:
            # FileMovedEvent
            # DirMovedEvent
            # FileModifiedEvent
            # FileCreatedEvent
            # DirCreatedEvent
            # FileDeletedEvent
            # DirDeletedEvent
            # print('Ignore DirModified:', event.key)
            return

        if isinstance(event, FileModifiedEvent) and not self.changed(event.src_path):
            # Reading a file updates its access time (if enabled on NTFS), and watchdog
            # reports that as a modification too.  Ignore it if nothing we render changed.
            return

        src_path = event.src_path
        path = os.path.dirname(src_path)
        for vid, paths in self.paths.items():
            if vid in self.ignored_views:
                continue
            if any(candidate in paths for candidate in (src_path, path)):
                self.schedule_refresh(vid)

    def schedule_refresh(self, view):
        with self.lock:
            self.scheduled_views[view] = time.monotonic()
            if self.ticking:
                return
            self.ticking = True
        sublime.set_timeout(self.refresh_settled_views, SCHEDULE_REFRESH)

    def refresh_settled_views(self):
        now = time.monotonic()
        with self.lock:
            views = [
                v for v, past in self.scheduled_views.items()
                if (now - past) > REFRESH_TIMEOUT
            ]
            for v in views:
                del self.scheduled_views[v]
            ticking = self.ticking = bool(self.scheduled_views)

        if ticking:  # re-arm first, a failing refresh must not stop the timer
            sublime.set_timeout(self.refresh_settled_views, SCHEDULE_REFRESH)
        if views:
            refresh(views)

    def remember(self, directory: str):
        '''Record the signatures of the files in a freshly watched directory.'''
        known = {}
        try:
            with os.scandir(directory) as entries:
                for entry in entries:
                    try:
                        known[entry.path] = signature(entry.stat(follow_symlinks=False))
                    except OSError:
                        pass
        except OSError:
            pass
        with self.lock:
            self.signatures[directory] = known

    def forget(self, directory: str):
        with self.lock:
            self.signatures.pop(directory, None)

    def changed(self, path: str) -> bool:
        try:
            current = signature(os.stat(path, follow_symlinks=False))
        except OSError:
            return True
        with self.lock:
            known = self.signatures.get(os.path.dirname(path))
            if known is None:
                return True
            previous = known.get(path)
            known[path] = current
        return current != previous


def signature(st: os.stat_result) -> tuple:
    '''What a dired listing shows of a file: size, mtime, and whether it is hidden.'''
    hidden = getattr(st, 'st_file_attributes', 0) & stat.FILE_ATTRIBUTE_HIDDEN
    return st.st_size, st.st_mtime_ns, hidden


def without_trailing_sep(path: str) -> str:
    '''Strip the trailing separator, but not from roots, "C:" is the cwd of drive C.'''
    return os.path.normpath(path)
