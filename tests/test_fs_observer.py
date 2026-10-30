import importlib
import os
import shutil
import tempfile
from unittest import TestCase, mock, skipIf

fs_observer = importlib.import_module('FileBrowser.0_dired_fs_observer')


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
