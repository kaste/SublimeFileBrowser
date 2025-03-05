# coding: utf-8

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
import datetime
from itertools import chain
import os

import sublime

try:  # unavailable dependencies shall not break basic functionality
    import package_events
    from watchdog.observers import Observer
    from watchdog.events import FileSystemEventHandler, DirModifiedEvent
except ImportError:
    Observer = None
    FileSystemEventHandler = object

from .common import EVENT_TOPIC

flatten = chain.from_iterable

REFRESH_TIMEOUT  = 1000  # milliseconds: auto-refresh shall not happen more than once per REFRESH_TIMEOUT
SCHEDULE_REFRESH = 700   # milliseconds: time out for checking REFRESH_TIMEOUT
observer = None


def plugin_loaded():
    global observer
    observer = ObservePaths()


def plugin_unloaded():
    global observer
    if observer:
        print('FileBrowser: shutting down file watcher:', observer.observer)
        observer.observer.stop()
        package_events.unlisten(EVENT_TOPIC, observer.dired_event_handler)
        observer.observer.join()
        del observer


def time_out(past, now):
    return (now - past) > datetime.timedelta(milliseconds=REFRESH_TIMEOUT)


def refresh(views, erase_settings=False):
    '''
    views
        list of integers which are view.id(), can be empty
    erase_settings
        boolean, can be True after change of global setting dired_autorefresh
    '''
    if not views and not erase_settings:
        def is_dired(view): return view.settings() and view.settings().get("dired_path")
    else:
        def is_dired(view): return False

    for w in sublime.windows():
        for v in w.views():
            if v.id() in views or is_dired(v):
                if erase_settings:
                    v.settings().erase('dired_autorefresh')
                v.run_command('dired_refresh')


class ObservePaths(object):
    def __new__(cls):
        if Observer is None:
            return None
        return object.__new__(cls)

    def __init__(self):
        self.observer = Observer()
        self.event_handler = ReportEvent()
        self.watched_paths: set[str] = set()
        self.paths: dict[sublime.ViewId, list[str]] = {}
        self.observer.start()
        package_events.listen(EVENT_TOPIC, self.dired_event_handler)

    def dired_event_handler(self, package, event, payload):
        '''receiving args from common.emit_event'''
        def stop_watch(vid: sublime.ViewId):
            self.paths.pop(vid, None)
            rewatch_all()

        def add_paths(vid: sublime.ViewId, paths):
            for path in paths:
                if path and not os.path.isdir(path):
                    print("error(add_paths): {path} is not a directory")
            paths = [p.rstrip(os.sep) for p in paths if os.path.isdir(p)]

            old_paths = self.paths.get(vid, [])
            self.paths.update({
                vid: sorted(p for p in set(old_paths + paths))
            })
            rewatch_all()

        def set_paths(vid: sublime.ViewId, paths):
            for path in paths:
                if path and not os.path.isdir(path):
                    print("error(set_paths): {path} is not a directory")
            paths = [p.rstrip(os.sep) for p in paths if os.path.isdir(p)]

            self.paths.update({
                vid: sorted(paths)
            })
            rewatch_all()

        def remove_path(vid: sublime.ViewId, path):
            if path and not os.path.isdir(path):
                print("error(remove_path): {path} is not a directory")
            path_without_sep = path.rstrip(os.sep)
            path_with_sep = path_without_sep + os.sep
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
            if self.watched_paths == next_set:
                return

            self.observer.unschedule_all()
            for p in next_set:
                self.observer.schedule(self.event_handler, p)
            self.watched_paths = next_set

        def toggle_watch_all(watch):
            '''watch is boolean or None, global setting dired_autorefresh'''
            views = self.paths.keys()
            if not watch:
                self.paths = {}
            sublime.set_timeout(lambda: refresh(views, erase_settings=(not watch)), 1)

        def ignore_view(vid: sublime.ViewId):
            self.event_handler.ignore_views.add(vid)

        def unignore_view(vid: sublime.ViewId):
            self.event_handler.ignore_views.discard(vid)

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
        self.event_handler.paths = self.paths


class ReportEvent(FileSystemEventHandler):
    def __init__(self):
        self.paths = {}
        self.ignore_views: set[sublime.ViewId] = set()
        self.scheduled_views = {}

    def on_any_event(self, event):
        '''
        File system event received from watchdog module,
        not to be confused with package_events which we use for internal communication
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

        src_path = event.src_path
        path = os.path.dirname(src_path)
        for v, p in self.paths.items():
            if any(i in p for i in (src_path, path)) and v not in self.ignore_views:
                if not self.scheduled_views:
                    self.schedule_refresh(v, datetime.datetime.now())
                else:
                    self.scheduled_views.update({v: datetime.datetime.now()})

    def schedule_refresh(self, view=None, at=None):
        now = datetime.datetime.now()
        if view and at:
            if time_out(at, now):
                sublime.set_timeout(self.schedule_refresh, SCHEDULE_REFRESH)
                return
            else:
                self.scheduled_views.update({view: at})

        if not self.scheduled_views:
            return

        views = [v for v, t in self.scheduled_views.items() if time_out(t, now)]
        for v in views:
            self.scheduled_views.pop(v, None)
        if views:
            sublime.set_timeout(lambda: refresh(views), 1)
        sublime.set_timeout(self.schedule_refresh, SCHEDULE_REFRESH)
        return
