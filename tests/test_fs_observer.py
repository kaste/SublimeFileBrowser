import importlib
import os
import shutil
import tempfile
import threading
from types import SimpleNamespace
from unittest import TestCase, mock, skipIf

from unittesting import DeferrableTestCase

from FileBrowser.common import emit_event, listings, NATURAL_SORT

fs_observer = importlib.import_module('FileBrowser.0_dired_fs_observer')

if fs_observer.Observer:
    from watchdog.events import DirModifiedEvent, FileCreatedEvent, FileModifiedEvent, FileMovedEvent


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class RewatchTest(DeferrableTestCase):
    def test_drops_vanished_directories_of_other_views(self):
        a, b, c = self.make_dirs('a', 'b', 'c')
        self.send('set_paths', (1, [a + os.sep]))
        self.send('set_paths', (2, [b + os.sep]))
        emitter_for_a = self.emitters()[a]
        os.rmdir(a)
        yield lambda: not emitter_for_a.should_keep_running()

        self.send('set_paths', (2, [b + os.sep, c + os.sep]))

        self.assertEqual(self.watcher.paths, {1: [], 2: [b, c]})
        self.assertEqual(self.watched(), {b, c})

    def test_keeps_watching_other_paths_if_one_fails(self):
        a, b = self.make_dirs('a', 'b')
        schedule = self.watcher.observer.schedule

        def failing_schedule(handler, path):
            if path == a:
                raise FileNotFoundError(2, 'vanished', path)
            return schedule(handler, path)

        with mock.patch.object(self.watcher.observer, 'schedule', failing_schedule):
            self.send('set_paths', (1, [a + os.sep, b + os.sep]))

        self.assertEqual(self.watcher.paths, {1: [b]})
        self.assertEqual(self.watched(), {b})

    def test_only_reschedules_changed_paths(self):
        a, b, c = self.make_dirs('a', 'b', 'c')
        self.send('set_paths', (1, [a + os.sep, b + os.sep]))
        emitter_for_a = self.emitters()[a]

        self.send('set_paths', (1, [a + os.sep, c + os.sep]))

        self.assertEqual(self.watched(), {a, c})
        self.assertIs(self.emitters()[a], emitter_for_a)

    def test_stops_watching_closed_views(self):
        a, b = self.make_dirs('a', 'b')
        self.send('set_paths', (1, [a + os.sep]))
        self.send('set_paths', (2, [b + os.sep]))

        self.send('view_closed', 1)

        self.assertEqual(self.watched(), {b})

    def test_only_the_last_watch_release_discards_the_listing(self):
        a, b = self.make_dirs('a', 'b')
        self.send('set_paths', (1, [a, b]))
        self.send('set_paths', (2, [a]))

        def read(path):
            return listings.read(path, True, True, (), (), NATURAL_SORT)

        shared = read(a)
        self.assertEqual(read(b), [])
        self.send('view_closed', 1)
        self.assertIs(read(a), shared)
        open(os.path.join(b, 'after-close'), 'w').close()
        self.assertEqual([e.name for e in read(b)], ['after-close'])

        self.send('remove_path', (2, a))
        open(os.path.join(a, 'after-fold'), 'w').close()
        self.assertEqual([e.name for e in read(a)], ['after-fold'])

    def test_restarts_stopped_watches(self):
        a, = self.make_dirs('a')
        self.send('set_paths', (1, [a + os.sep]))
        stopped = self.emitters()[a]
        # what watchdog does when the watched directory gets deleted
        stopped.stop()
        stopped.join()

        self.send('set_paths', (1, [a + os.sep]))

        self.assertEqual(self.watched(), {a})
        self.assertIsNot(self.emitters()[a], stopped)
        self.assertTrue(self.emitters()[a].should_keep_running())

    def test_restarts_dead_watches(self):
        a, = self.make_dirs('a')
        self.send('set_paths', (1, [a + os.sep]))
        dead = self.emitters()[a]
        # what remains of an emitter whose thread died from an exception
        dead.is_alive = lambda: False

        self.send('set_paths', (1, [a + os.sep]))

        self.assertEqual(self.watched(), {a})
        self.assertIsNot(self.emitters()[a], dead)

    def test_only_checks_the_paths_of_the_sending_view(self):
        a, b = self.make_dirs('a', 'b')
        self.send('set_paths', (1, [a + os.sep]))

        with mock.patch.object(os.path, 'isdir', wraps=os.path.isdir) as isdir:
            self.send('set_paths', (2, [b + os.sep]))

        checked = {os.fsdecode(call.args[0]).rstrip(os.sep) for call in isdir.call_args_list}
        self.assertNotIn(a, checked)
        self.assertEqual(self.watched(), {a, b})

    def test_watches_root_directories(self):
        root = os.path.abspath(os.sep)

        self.send('set_paths', (1, [root]))

        self.assertEqual(self.watcher.paths, {1: [root]})
        self.assertEqual(self.watched(), {root})

    def test_schedules_a_refresh_for_new_files(self):
        a, = self.make_dirs('a')
        self.send('set_paths', (1, [a + os.sep]))

        open(os.path.join(a, 'new'), 'w').close()

        yield lambda: 1 in self.watcher.event_handler.scheduled_views

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)
        self.watcher = fs_observer.ObservePaths()
        self.addCleanup(self.watcher.stop)

    def make_dirs(self, *names):
        paths = [os.path.join(self.root, name) for name in names]
        for path in paths:
            os.mkdir(path)
        return paths

    def send(self, event, payload):
        self.watcher.dired_event_handler(event, payload)

    def emitters(self):
        return {emitter.watch.path: emitter for emitter in self.watcher.observer.emitters}

    def watched(self):
        self.assertEqual(set(self.watcher.watches), set(self.emitters()))
        return set(self.watcher.watches)


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class PollingRewatchTest(RewatchTest):
    '''The fallback on macOS without watchdog's compiled FSEvents module'''
    def setUp(self):
        from watchdog.observers.polling import PollingObserver
        patcher = mock.patch.object(fs_observer, 'Observer', PollingObserver)
        self.addCleanup(patcher.stop)
        patcher.start()
        super().setUp()


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class EmitEventTest(DeferrableTestCase):
    def test_handles_events_in_order_on_the_watcher_thread(self):
        threads = []
        handler = self.watcher.dired_event_handler

        def recording_handler(event, payload):
            threads.append(threading.current_thread().name)
            handler(event, payload)

        self.watcher.dired_event_handler = recording_handler
        emit_event('set_paths', (1, [self.a + os.sep]))
        emit_event('stop_watch', 1)
        emit_event('set_paths', (2, [self.a + os.sep]))
        yield lambda: len(threads) == 3

        self.assertEqual(set(threads), {'FileBrowser file watcher'})
        self.assertEqual(self.watcher.paths, {2: [self.a]})

    def test_survives_failing_handlers(self):
        emit_event('unknown_event', None)
        emit_event('set_paths', (1, [self.a + os.sep]))

        yield lambda: set(self.watcher.watches) == {self.a}

    def test_stops_the_observer_once_pending_events_are_handled(self):
        emit_event('set_paths', (1, [self.a + os.sep]))
        self.watcher.stop()

        yield lambda: not self.watcher.observer.is_alive()
        self.assertEqual(set(self.watcher.watches), {self.a})

    def setUp(self):
        root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        self.a = os.path.join(root, 'a')
        os.mkdir(self.a)
        self.watcher = fs_observer.ObservePaths()
        self.addCleanup(self.watcher.stop)
        patcher = mock.patch.object(fs_observer, 'observer', self.watcher)
        self.addCleanup(patcher.stop)
        patcher.start()


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class QueuedFilesystemEventTest(TestCase):
    def test_stalled_stat_releases_dispatcher_lock_and_shutdown_waits_for_work(self):
        entered = threading.Event()
        release = threading.Event()
        stat_threads = []
        timed_out = []

        def stalled_stat(*args, **kwargs):
            stat_threads.append(threading.current_thread().name)
            entered.set()
            if not release.wait(2):
                timed_out.append(True)
            return self.stat_result

        filesystem = SimpleNamespace(stat=stalled_stat, path=os.path)
        with mock.patch.object(fs_observer, 'os', filesystem):
            try:
                self.observer.event_queue.put((FileModifiedEvent(self.file), self.watch))
                self.assertTrue(entered.wait(1), 'event did not reach stat')

                acquired = self.observer._lock.acquire(timeout=0.1)
                if acquired:
                    self.observer._lock.release()
                self.assertTrue(acquired, 'watchdog still holds its lock across stat')
                self.assertEqual(stat_threads, ['FileBrowser file watcher'])

                self.watcher.stop()
                self.assertTrue(self.observer.is_alive())
                self.sublime.set_timeout.assert_not_called()
            finally:
                release.set()
                self.watcher.stop()
                self.observer.join(2)

        self.assertFalse(self.observer.is_alive(), 'observer did not stop')
        self.assertEqual(timed_out, [])
        self.assertEqual(list(self.watcher.event_handler.scheduled_views), [1])
        self.sublime.set_timeout.assert_called_once()

    def test_serializes_filesystem_events_with_ignore_and_watch_commands(self):
        handler = self.watcher.event_handler
        with mock.patch.object(handler, 'schedule_refresh', wraps=handler.schedule_refresh) as schedule:
            self.watcher.post('ignore_view', 1)
            handler.on_any_event(FileCreatedEvent(self.file))
            self.watcher.post('watch_view', 1)
            handler.on_any_event(FileCreatedEvent(self.file))
            self.stop_watcher()

            schedule.assert_called_once_with(1)
        self.sublime.set_timeout.assert_called_once()

    def test_events_queued_after_view_close_do_not_refresh(self):
        self.watcher.post('view_closed', 1)
        self.watcher.event_handler.on_any_event(FileCreatedEvent(self.file))
        self.stop_watcher()

        self.assertEqual(self.watcher.event_handler.paths, {})
        self.sublime.set_timeout.assert_not_called()

    def test_queued_access_time_only_changes_do_not_refresh(self):
        handler = self.watcher.event_handler
        handler.remember(os.path.dirname(self.file))
        st = self.stat_result
        os.utime(self.file, ns=(st.st_atime_ns - 10**12, st.st_mtime_ns))
        handler.on_any_event(FileModifiedEvent(self.file))
        self.stop_watcher()

        self.assertEqual(handler.scheduled_views, {})
        self.sublime.set_timeout.assert_not_called()

    def test_queued_event_burst_starts_only_one_timer(self):
        handler = self.watcher.event_handler
        for number in range(100):
            handler.on_any_event(FileCreatedEvent(self.file + str(number)))
        self.stop_watcher()

        self.assertEqual(list(handler.scheduled_views), [1])
        self.sublime.set_timeout.assert_called_once()

    def test_continues_after_a_filesystem_event_fails(self):
        handler = self.watcher.event_handler
        filesystem = SimpleNamespace(
            stat=mock.Mock(side_effect=RuntimeError('injected failure')), path=os.path
        )
        with mock.patch.object(fs_observer, 'os', filesystem), \
                mock.patch.object(fs_observer.traceback, 'print_exc') as report:
            handler.on_any_event(FileModifiedEvent(self.file))
            handler.on_any_event(FileCreatedEvent(self.file))
            self.stop_watcher()

            report.assert_called_once()
        self.assertEqual(list(handler.scheduled_views), [1])
        self.sublime.set_timeout.assert_called_once()

    def setUp(self):
        from watchdog.observers.api import BaseObserver

        root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        self.file = os.path.join(root, 'file.txt')
        with open(self.file, 'w') as f:
            f.write('content')
        self.stat_result = os.stat(self.file)
        # Exercise the real dispatcher lock, but never start a native emitter.
        self.observer = BaseObserver(mock.Mock())
        api_patcher = mock.patch.object(fs_observer, 'sublime')
        self.addCleanup(api_patcher.stop)
        self.sublime = api_patcher.start()
        with mock.patch.object(fs_observer, 'Observer', return_value=self.observer):
            self.watcher = fs_observer.ObservePaths()
        self.addCleanup(self.stop_watcher)
        self.watcher.paths = {1: [root]}
        self.watcher.event_handler.paths = {1: [root]}
        self.watch = self.observer.schedule(self.watcher.event_handler, root)

    def stop_watcher(self):
        self.watcher.stop()
        self.observer.join(2)
        self.assertFalse(self.observer.is_alive(), 'observer did not stop')


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class ReportEventTest(TestCase):
    def test_callback_enqueues_without_checking_files_or_scheduling_timers(self):
        event = FileModifiedEvent(self.file)
        with mock.patch.object(self.handler, 'changed', side_effect=AssertionError('stat check')):
            self.handler.on_any_event(event)

        self.handler.post.assert_called_once_with('filesystem_event', event)
        self.sublime.set_timeout.assert_not_called()

    def test_directory_modifications_are_not_queued(self):
        self.handler.on_any_event(DirModifiedEvent(self.root))

        self.handler.post.assert_not_called()
        self.sublime.set_timeout.assert_not_called()

    def test_ignores_access_time_only_change(self):
        st = os.stat(self.file)
        os.utime(self.file, ns=(st.st_atime_ns - 10**12, st.st_mtime_ns))

        self.handler.process_event(FileModifiedEvent(self.file))

        self.assertEqual(self.handler.scheduled_views, {})

    def test_move_refreshes_both_listings_but_leaves_other_directories_cached(self):
        with tempfile.TemporaryDirectory() as dest, tempfile.TemporaryDirectory() as other:
            def names(path):
                return [e.name for e in listings.read(path, True, True, (), (), NATURAL_SORT)]

            for path in (self.root, dest, other):
                names(path)
            target = os.path.join(dest, 'moved')
            os.rename(self.file, target)
            self.handler.paths[2] = [dest]
            self.handler.process_event(FileMovedEvent(self.file, target))

            self.assertEqual(set(self.handler.scheduled_views), {1, 2})
            self.assertEqual(names(self.root), [])
            self.assertEqual(names(dest), ['moved'])
            with mock.patch.object(os, 'scandir', wraps=os.scandir) as scan:
                self.assertEqual(names(other), [])
                scan.assert_not_called()

    def test_refreshes_on_content_change(self):
        with open(self.file, 'a') as f:
            f.write('more')

        self.handler.process_event(FileModifiedEvent(self.file))

        self.assertEqual(list(self.handler.scheduled_views), [1])

    def test_refreshes_modified_files_in_unknown_directories(self):
        self.handler.forget(self.root)

        self.handler.process_event(FileModifiedEvent(self.file))

        self.assertEqual(list(self.handler.scheduled_views), [1])

    def test_starts_one_timer_for_many_events(self):
        for name in 'abcde':
            self.handler.process_event(FileCreatedEvent(os.path.join(self.root, name)))

        self.assertEqual(self.sublime.set_timeout.call_count, 1)

    def test_refreshes_settled_views_and_stops_ticking(self):
        self.handler.process_event(FileCreatedEvent(os.path.join(self.root, 'new')))
        self.handler.scheduled_views[1] -= fs_observer.REFRESH_TIMEOUT + 1

        self.handler.refresh_settled_views()

        self.refresh.assert_called_once_with([1])
        self.assertFalse(self.handler.ticking)
        self.assertEqual(self.sublime.set_timeout.call_count, 1)

    def test_keeps_ticking_while_events_arrive(self):
        self.handler.process_event(FileCreatedEvent(os.path.join(self.root, 'new')))

        self.handler.refresh_settled_views()

        self.refresh.assert_not_called()
        self.assertTrue(self.handler.ticking)
        self.assertEqual(self.sublime.set_timeout.call_count, 2)

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)
        self.file = os.path.join(self.root, 'file.txt')
        with open(self.file, 'w') as f:
            f.write('content')

        self.sublime = self.patch('sublime')
        self.refresh = self.patch('refresh')
        self.handler = fs_observer.ReportEvent(mock.Mock())
        self.handler.paths = {1: [self.root]}
        self.handler.remember(self.root)

    def patch(self, name):
        patcher = mock.patch.object(fs_observer, name)
        self.addCleanup(patcher.stop)
        return patcher.start()
