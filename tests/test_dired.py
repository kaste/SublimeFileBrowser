import os
import tempfile
import unittest
from unittest import mock

from FileBrowser import common
from FileBrowser.dired import existing_directories


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
                '[?: Help, I: Disable Filter, u: Unmark All, ctrl+z: Clear Clipboard] '
                'Project root, Hidden: Off' + (', System: On' if common.NT else '') +
                ', marked(2), copied(1), cut(3)',
            )
            settings.set.assert_not_called()
            api.save_settings.assert_not_called()
            filesystem.assert_not_called()
            self.assertEqual(filesystem.mock_calls, [])


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
