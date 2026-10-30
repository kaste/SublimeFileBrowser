import importlib
import os
import shutil
import tempfile
from unittest import TestCase, mock, skipIf

fs_observer = importlib.import_module('FileBrowser.0_dired_fs_observer')

if fs_observer.Observer:
    from watchdog.events import FileCreatedEvent


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class RewatchTest(TestCase):
    def test_drops_vanished_directories_of_other_views(self):
        a, b, c = self.make_dirs('a', 'b', 'c')
        self.send('set_paths', (1, [a + os.sep]))
        self.send('set_paths', (2, [b + os.sep]))
        os.rmdir(a)

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
        self.watcher.dired_event_handler('FileBrowser', event, payload)

    def emitters(self):
        return {emitter.watch.path: emitter for emitter in self.watcher.observer.emitters}

    def watched(self):
        self.assertEqual(set(self.watcher.watches), set(self.emitters()))
        return set(self.watcher.watches)


@skipIf(fs_observer.Observer is None, 'watchdog is not importable')
class ReportEventTest(TestCase):
    def test_starts_one_timer_for_many_events(self):
        for name in 'abcde':
            self.handler.on_any_event(FileCreatedEvent(os.path.join(self.root, name)))

        self.assertEqual(self.sublime.set_timeout.call_count, 1)

    def test_refreshes_settled_views_and_stops_ticking(self):
        self.handler.on_any_event(FileCreatedEvent(os.path.join(self.root, 'new')))
        self.handler.scheduled_views[1] -= fs_observer.REFRESH_TIMEOUT + 1

        self.handler.refresh_settled_views()

        self.refresh.assert_called_once_with([1])
        self.assertFalse(self.handler.ticking)
        self.assertEqual(self.sublime.set_timeout.call_count, 1)

    def test_keeps_ticking_while_events_arrive(self):
        self.handler.on_any_event(FileCreatedEvent(os.path.join(self.root, 'new')))

        self.handler.refresh_settled_views()

        self.refresh.assert_not_called()
        self.assertTrue(self.handler.ticking)
        self.assertEqual(self.sublime.set_timeout.call_count, 2)

    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)

        self.sublime = self.patch('sublime')
        self.refresh = self.patch('refresh')
        self.handler = fs_observer.ReportEvent()
        self.handler.paths = {1: [self.root]}

    def patch(self, name):
        patcher = mock.patch.object(fs_observer, name)
        self.addCleanup(patcher.stop)
        return patcher.start()
