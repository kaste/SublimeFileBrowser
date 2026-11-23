import os
import queue
import threading
import tempfile
from unittest import TestCase, mock

import sublime
from unittesting import DeferrableViewTestCase

from FileBrowser import dired_file_operations, status
from FileBrowser.show import retarget_dired_view


class SummaryCacheTest(TestCase):
    def test_computes_once_per_dataset_and_render_reads_only_cached_data(self):
        paths = ['/audit/one', '/audit/two']
        view = self.make_view(1, dired_marked_paths=paths)
        threads = []

        def measure(snapshot):
            threads.append(threading.current_thread().name)
            return 2048

        self.measure.side_effect = measure
        self.cache.update(view)
        self.assertIsNone(self.cache.cached_size(view, 'marked', paths))
        self.publish_next()

        for _ in range(3):
            self.cache.update(view)
            self.assertEqual(self.cache.cached_size(view, 'marked', paths), 2048)
        self.measure.assert_called_once_with(tuple(paths))
        self.assertEqual(threads, ['FileBrowser status summaries'])
        self.render.assert_called_once_with(view)

    def test_clipboard_changes_update_all_views_and_share_one_computation(self):
        first, second = self.make_view(1), self.make_view(2)
        self.cache.update(first)
        self.cache.update(second)

        self.settings.set('dired_to_copy', ['/elsewhere/copied'])
        self.settings.set('dired_to_move', [])
        self.assertEqual(self.callbacks.qsize(), 1, 'settings updates must coalesce')
        self.publish_next()  # Snapshot the changed datasets on the main thread.
        self.publish_next()  # Publish the worker result.

        self.measure.assert_called_once_with(('/elsewhere/copied',))
        for view in (first, second):
            self.assertEqual(self.cache.cached_size(view, 'copied', ['/elsewhere/copied']), 2048)
        self.assertEqual(self.render.call_args_list[-2:], [mock.call(first), mock.call(second)])

    def test_clipboard_result_outlives_the_originating_view(self):
        self.settings['dired_to_copy'] = ['/elsewhere/copied']
        first = self.make_view(1)
        self.cache.update(first)
        completion = self.callbacks.get(timeout=2)
        self.cache.forget(first)

        completion()
        second = self.make_view(2)
        self.cache.update(second)

        self.assertEqual(self.cache.cached_size(second, 'copied', ['/elsewhere/copied']), 2048)
        self.measure.assert_called_once()

    def test_refresh_invalidates_same_paths_and_unrelated_settings_do_not(self):
        paths = ['/audit/file']
        view = self.make_view(1, dired_marked_paths=paths)
        self.measure.side_effect = [10, 20]
        self.cache.update(view)
        self.publish_next()

        view.settings().set('color_scheme', 'another scheme')
        self.publish_next()
        self.assertEqual(self.cache.cached_size(view, 'marked', paths), 10)
        self.measure.assert_called_once()

        self.cache.update(view, refresh=True)
        self.assertIsNone(self.cache.cached_size(view, 'marked', paths))
        self.publish_next()
        self.assertEqual(self.cache.cached_size(view, 'marked', paths), 20)
        self.assertEqual(self.measure.call_count, 2)

    def test_stalled_work_is_coalesced_and_stale_results_are_discarded(self):
        entered, release = threading.Event(), threading.Event()
        old, middle, latest = ['/audit/old'], ['/audit/middle'], ['/audit/latest']
        view = self.make_view(1, dired_marked_paths=old)

        def measure(paths):
            if paths == tuple(old):
                entered.set()
                if not release.wait(2):
                    raise AssertionError('test gate timed out')
                return 10
            return 30

        self.measure.side_effect = measure
        try:
            self.cache.update(view)
            self.assertTrue(entered.wait(1))
            view.settings().set('dired_marked_paths', middle)
            self.publish_next()
            view.settings().set('dired_marked_paths', latest)
            self.publish_next()
            self.assertIsNone(self.cache.cached_size(view, 'marked', latest))
            self.render.reset_mock()
        finally:
            release.set()

        self.publish_next()  # Old generation must not repaint or publish a size.
        self.render.assert_not_called()
        self.publish_next()
        self.assertEqual(self.cache.cached_size(view, 'marked', latest), 30)
        self.assertEqual(
            self.measure.call_args_list, [mock.call(tuple(old)), mock.call(tuple(latest))]
        )
        self.render.assert_called_once_with(view)

    def test_stop_does_not_wait_for_stalled_storage_or_publish_afterward(self):
        entered, release, finished = threading.Event(), threading.Event(), threading.Event()
        view = self.make_view(1, dired_marked_paths=['/audit/file'])

        def measure(paths):
            entered.set()
            release.wait(2)
            finished.set()
            return 10

        self.measure.side_effect = measure
        try:
            self.cache.update(view)
            self.assertTrue(entered.wait(1))
            self.cache.stop()
            self.assertFalse(finished.is_set(), 'stop waited for storage')
        finally:
            release.set()
            self.cache.worker.join(2)
        self.assertFalse(self.cache.worker.is_alive())
        self.assertTrue(self.callbacks.empty())
        self.render.assert_not_called()

    def test_clearing_marks_discards_an_already_queued_completion(self):
        view = self.make_view(1, dired_marked_paths=['/audit/file'])
        self.cache.update(view)
        old_completion = self.callbacks.get(timeout=2)
        view.settings().set('dired_marked_paths', [])
        self.publish_next()
        self.render.reset_mock()

        old_completion()

        self.assertEqual(self.cache.cached_size(view, 'marked', []), 0)
        self.render.assert_not_called()

    def test_retarget_discards_completion_even_before_settings_callback_runs(self):
        paths = ['/audit/file']
        view = self.make_view(1, dired_marked_paths=paths)
        self.cache.update(view)
        old_completion = self.callbacks.get(timeout=2)
        view.settings().set('dired_path', '/other/')

        old_completion()

        self.assertIsNone(self.cache.cached_size(view, 'marked', paths))
        self.render.assert_not_called()

    def test_closed_view_and_unloaded_cache_reject_completions(self):
        for unload in (False, True):
            with self.subTest(unload=unload):
                view = self.make_view(1, dired_marked_paths=['/audit/file'])
                self.cache.update(view)
                old_completion = self.callbacks.get(timeout=2)
                if unload:
                    self.cache.stop()
                    self.assertEqual(self.settings.callbacks, {})
                else:
                    self.cache.forget(view)
                self.assertEqual(view.settings().callbacks, {})

                old_completion()

                self.render.assert_not_called()

    def test_completed_totals_do_not_replace_rename_instructions(self):
        view = self.make_view(1, dired_marked_paths=['/audit/file'], dired_rename_mode=True)
        self.cache.update(view)
        self.publish_next()
        self.render.assert_not_called()

        view.settings().set('dired_rename_mode', False)
        self.publish_next()
        self.render.assert_called_once_with(view)
        self.assertEqual(self.cache.cached_size(view, 'marked', ['/audit/file']), 2048)

    def test_missing_files_and_directories_have_unknown_sizes_without_pruning(self):
        with tempfile.TemporaryDirectory() as root:
            missing = os.path.join(root, 'missing')
            self.measure.side_effect = status.total_size
            view = self.make_view(1, dired_marked_paths=[missing])
            self.cache.update(view)
            self.publish_next()
            self.assertIsNone(self.cache.cached_size(view, 'marked', [missing]))
            self.assertEqual(view.settings().get('dired_marked_paths'), [missing])

            self.settings.set('dired_to_move', [root + os.sep])
            self.publish_next()
            self.publish_next()
            self.assertIsNone(self.cache.cached_size(view, 'cut', [root + os.sep]))
            self.assertEqual(self.settings.get('dired_to_move'), [root + os.sep])

    def setUp(self):
        self.settings = Settings()
        self.callbacks = queue.Queue()
        self.render = mock.Mock()
        self.measure = mock.Mock(return_value=2048)
        self.cache = status.SummaryCache(
            self.settings, self.callbacks.put, self.render, measure=self.measure
        )
        self.addCleanup(self.cache.stop)

    def publish_next(self):
        self.callbacks.get(timeout=2)()

    def make_view(self, vid, **settings):
        view = mock.Mock()
        view.id.return_value = vid
        view.is_valid.return_value = True
        view.settings.return_value = Settings(dired_path='/audit/', **settings)
        return view


class StatusIntegrationTest(DeferrableViewTestCase):
    def test_mark_and_refresh_commands_update_sizes_without_render_probes(self):
        self.view.run_command('dired_mark', {'markall': True, 'move_cursor': False})
        yield lambda: 'marked(2; 3kB)' in self.view.get_status('__FileBrowser__')

        with open(self.first, 'ab') as f:
            f.write(b'x' * 1024)
        self.view.run_command('dired_rescan')
        yield lambda: 'marked(2; 4kB)' in self.view.get_status('__FileBrowser__')

        self.view.run_command('dired_mark', {'mark': False, 'markall': True})
        self.assertNotIn('marked(', self.view.get_status('__FileBrowser__'))

    def test_summary_completion_does_not_overwrite_operation_progress(self):
        operation = object.__new__(dired_file_operations.call_SystemAgnosticFileOperation)
        operation.view = self.view
        thread = mock.Mock()
        thread.name = 'copy'
        with mock.patch.object(sublime, 'set_timeout'):
            operation.progress_bar([thread])
        try:
            self.view.run_command('dired_mark', {'markall': True, 'move_cursor': False})
            yield lambda: 'marked(2; 3kB)' in self.view.get_status('__FileBrowser__')

            self.assertIn('Writing copy', self.view.get_status('__0FileBrowser__'))
        finally:
            self.view.erase_status('__0FileBrowser__')

    def test_copy_cut_and_clear_commands_update_the_shared_summaries(self):
        settings = sublime.load_settings('dired.sublime-settings')
        keys = ('dired_to_copy', 'dired_to_move')
        original = {key: (settings.has(key), settings.get(key)) for key in keys}
        with mock.patch.object(sublime, 'save_settings'), \
                mock.patch.object(dired_file_operations, 'NT', False):
            try:
                for key in keys:
                    settings.set(key, [])
                self.view.run_command('dired_mark', {'markall': True, 'move_cursor': False})
                self.view.run_command('dired_copy_files')
                yield lambda: 'copied(2; 3kB)' in self.view.get_status('__FileBrowser__')
                self.assertNotIn('marked(', self.view.get_status('__FileBrowser__'))

                self.view.run_command('dired_mark', {'markall': True, 'move_cursor': False})
                self.view.run_command('dired_copy_files', {'cut': True})
                yield lambda: 'cut(2; 3kB)' in self.view.get_status('__FileBrowser__')
                self.assertNotIn('copied(', self.view.get_status('__FileBrowser__'))

                self.view.run_command('dired_clear_copy_cut_list', {'clear_system': False})
                self.assertNotIn('cut(', self.view.get_status('__FileBrowser__'))
                self.assertNotIn('copied(', self.view.get_status('__FileBrowser__'))
            finally:
                for key, (present, value) in original.items():
                    if present:
                        settings.set(key, value)
                    else:
                        settings.erase(key)

    def test_clear_unmarks_and_empties_the_copy_cut_list(self):
        settings = sublime.load_settings('dired.sublime-settings')
        keys = ('dired_to_copy', 'dired_to_move')
        original = {key: (settings.has(key), settings.get(key)) for key in keys}
        with mock.patch.object(sublime, 'save_settings'), \
                mock.patch.object(dired_file_operations, 'NT', False):
            try:
                for key in keys:
                    settings.set(key, [])
                self.view.run_command('dired_mark', {'markall': True, 'move_cursor': False})
                self.view.run_command('dired_copy_files')
                self.view.run_command('dired_mark', {'markall': True, 'move_cursor': False})
                yield lambda: all(
                    part in self.view.get_status('__FileBrowser__')
                    for part in ('marked(2', 'copied(2')
                )

                self.view.run_command('dired_clear')

                self.assertEqual(self.view.settings().get('dired_marked_paths'), [])
                self.assertEqual(settings.get('dired_to_copy'), [])
                self.assertEqual(settings.get('dired_to_move'), [])
                self.assertNotIn('marked(', self.view.get_status('__FileBrowser__'))
                self.assertNotIn('copied(', self.view.get_status('__FileBrowser__'))
            finally:
                for key, (present, value) in original.items():
                    if present:
                        settings.set(key, value)
                    else:
                        settings.erase(key)

    def setUp(self):
        super().setUp()
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = directory.name + os.sep
        self.first = os.path.join(self.root, 'one.txt')
        for path, size in ((self.first, 1024), (os.path.join(self.root, 'two.txt'), 2048)):
            with open(path, 'wb') as f:
                f.write(b'x' * size)
        self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
        self.view.settings().set('dired_autorefresh', False)
        self.view.settings().set('dired_filter', '')
        self.view.settings().set('dired_filter_extension', '')
        retarget_dired_view(self.view, self.root)
        yield lambda: self.first in self.view.settings().get('dired_index', [])


class Settings(dict):
    def __init__(self, **values):
        super().__init__(values)
        self.callbacks = {}

    def set(self, key, value):
        self[key] = value
        for callback in list(self.callbacks.values()):
            callback()

    def add_on_change(self, tag, callback):
        self.callbacks[tag] = callback

    def clear_on_change(self, tag):
        self.callbacks.pop(tag, None)
