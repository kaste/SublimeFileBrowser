import os
import tempfile
import unittest
from unittest import mock

from FileBrowser import common, dired
from FileBrowser.dired import existing_directories


class FuzzyMatchTest(unittest.TestCase):
    def test_highlight_indices(self):
        for term, name, expected in [
            ('', 'file', []),
            ('AB', 'cab', [1, 2]),
            ('ab', 'cab-ab-ab', [7, 8]),
            ('ab', 'ab-ab', [0, 1]),
            ('ab', 'a-b-b', [0, 4]),
            ('abc', 'a-bc-b', [0, 2, 3]),
            ('ab', 'a_b', []),
            ('.py', 'x.py.py', [4, 5, 6]),
            ('.py', 'x.py.PY', [4, 5, 6]),
            ('.py', 'x.py.p-y', [4, 5, 7]),
            ('ki', 'Kİ', [0, 1]),
            ('.txt', 'İ.txt', [1, 2, 3, 4]),
            ('sék', 'x-ſ-É-K', [2, 4, 6]),
            ('aΣb', 'a-ς-b', [0, 2, 4]),
            ('ß', 'ss', []),
            ('ab', 'a\nx-b', []),
        ]:
            with self.subTest(term=term, name=name):
                self.assertEqual(common.rx_fuzzy_match(term, name), expected)

    def test_callers_cannot_mutate_cached_indices(self):
        common.rx_fuzzy_match('ab', 'a-b').clear()
        self.assertEqual(common.rx_fuzzy_match('ab', 'a-b'), [0, 2])


class AutorefreshSettingsTest(unittest.TestCase):
    def test_unrelated_settings_changes_do_not_toggle_watches(self):
        self.values['dired_to_copy'] = ['/audit/file']
        self.callback()
        self.values['dired_show_hidden_files'] = False
        self.callback()

        self.emit_event.assert_not_called()

    def test_emits_once_per_value_change_including_setting_removal(self):
        for value in (False, True, None, False):
            if value is None:
                self.values.pop('dired_autorefresh')
            else:
                self.values['dired_autorefresh'] = value
            self.callback()
            self.callback()

        self.assertEqual(self.emit_event.call_args_list, [
            mock.call('toggle_watch_all', False),
            mock.call('toggle_watch_all', True),
            mock.call('toggle_watch_all', None),
            mock.call('toggle_watch_all', False),
        ])

    def test_reentrant_notification_does_not_repeat_the_toggle(self):
        def notify_again(*args):
            self.emit_event.side_effect = None
            self.callback()

        self.emit_event.side_effect = notify_again
        self.values['dired_autorefresh'] = False
        self.callback()

        self.emit_event.assert_called_once_with('toggle_watch_all', False)

    def setUp(self):
        self.values = {'dired_autorefresh': True}
        api_patcher = mock.patch.object(dired, 'sublime')
        self.addCleanup(api_patcher.stop)
        api = api_patcher.start()
        api.windows.return_value = []
        settings = api.load_settings.return_value
        settings.get.side_effect = self.values.get
        emit_patcher = mock.patch.object(dired, 'emit_event')
        self.addCleanup(emit_patcher.stop)
        self.emit_event = emit_patcher.start()

        dired.plugin_loaded()

        tag, self.callback = settings.add_on_change.call_args.args
        self.assertEqual(tag, 'dired_autorefresh')
        self.emit_event.assert_not_called()


class StatusTest(unittest.TestCase):
    def test_reports_counts_without_probing_or_pruning_paths(self):
        view_settings = {
            'dired_path': '/audit/',
            'dired_show_hidden_files': False,
            'dired_filter': 'needle',
            'dired_marked_paths': ['/unavailable/file', '/unavailable/folder/'],
            'dired_to_copy': ['/unavailable/copy'],
        }
        clipboard_settings = {
            'dired_to_copy': ['/unavailable/copy'],
            'dired_to_move': ['/missing/one', '/missing/two', '/missing/three'],
        }
        command = common.DiredBaseCommand()
        command.view = mock.Mock()
        command.view.settings.return_value.get.side_effect = view_settings.get
        command.view.window.return_value.folders.return_value = ['/audit']

        with mock.patch.object(common, 'sublime') as api, \
                mock.patch.object(common, 'os') as filesystem, \
                mock.patch.object(common, 'isdir', side_effect=AssertionError('isdir called')):
            settings = api.load_settings.return_value
            settings.get.side_effect = clipboard_settings.get
            filesystem.path.lexists.side_effect = AssertionError('lexists called')
            filesystem.path.getsize.side_effect = AssertionError('getsize called')
            filesystem.stat.side_effect = AssertionError('stat called')

            command.set_status()

            command.view.set_status.assert_called_once_with(
                '__FileBrowser__',
                '[?: Help, I: Disable Filter, u: Unmark All, ctrl+space: Clear Clipboard] '
                'Project root, Hidden: Off' + (', System: On' if common.NT else '') +
                ', marked(2), copied(1), cut(3)',
            )
            settings.set.assert_not_called()
            api.save_settings.assert_not_called()
            filesystem.assert_not_called()
            self.assertEqual(filesystem.mock_calls, [])


class CachedStatusTest(unittest.TestCase):
    def test_renders_cached_sizes_without_recomputing_them(self):
        command = common.DiredBaseCommand()
        command.view = mock.Mock()
        view_settings = {
            'dired_path': '/audit/',
            'dired_marked_paths': ['/audit/one', '/audit/two'],
        }
        clipboard_settings = {
            'dired_to_copy': ['/elsewhere/copy'],
            'dired_to_move': ['/elsewhere/cut'],
        }
        command.view.settings.return_value.get.side_effect = view_settings.get
        command.view.window.return_value.folders.return_value = []
        with mock.patch.object(common, 'sublime') as api, \
                mock.patch.object(common, 'status', create=True) as summaries, \
                mock.patch.object(common, 'os') as filesystem, \
                mock.patch.object(common, 'isdir', side_effect=AssertionError('isdir called')):
            api.load_settings.return_value.get.side_effect = clipboard_settings.get
            summaries.cached_size.side_effect = lambda view, kind, paths: {
                'marked': 2048, 'copied': 3 * 1024 * 1024, 'cut': 512,
            }[kind]

            for nt, show_system, indicator in (
                (False, False, ''),
                (False, True, ''),
                (True, False, ', System: Off'),
                (True, True, ', System: On'),
            ):
                with self.subTest(nt=nt, show_system=show_system), \
                        mock.patch.object(common, 'NT', nt):
                    view_settings['dired_show_system_files'] = show_system
                    command.set_status()
                    command.set_status()

                    self.assertEqual(command.view.set_status.call_args, mock.call(
                        '__FileBrowser__',
                        '[?: Help, u: Unmark All, ctrl+space: Clear Clipboard] '
                        'Hidden: On' + indicator +
                        ', marked(2; 2kB), copied(1; 3MB), cut(1; 512B)',
                    ))
            summaries.update.assert_not_called()
            self.assertEqual(filesystem.mock_calls, [])
            api.save_settings.assert_not_called()


class ExistingDirectoriesTest(unittest.TestCase):
    def test_discards_directory_renamed_since_last_refresh(self):
        with tempfile.TemporaryDirectory() as root:
            old_path = os.path.join(root, 'filebrowser_lib') + os.sep
            new_path = os.path.join(root, 'lib') + os.sep
            os.mkdir(old_path)

            os.rename(old_path, new_path)

            self.assertEqual(existing_directories([old_path]), [])

    def test_preserves_existing_directories_in_order_without_duplicates(self):
        with tempfile.TemporaryDirectory() as root:
            first = os.path.join(root, 'first') + os.sep
            second = os.path.join(root, 'second') + os.sep
            os.mkdir(first)
            os.mkdir(second)

            self.assertEqual(
                existing_directories([second, first, second]),
                [second, first],
            )
