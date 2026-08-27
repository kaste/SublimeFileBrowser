import os
import tempfile
import unittest
from unittest import mock

from FileBrowser.common import prune_missing_cut_items
from FileBrowser.dired import existing_directories


class PruneMissingCutItemsTest(unittest.TestCase):
    def test_discards_file_moved_by_an_external_application(self):
        with tempfile.TemporaryDirectory() as root:
            source = os.path.join(root, 'source.txt')
            destination = os.path.join(root, 'destination.txt')
            with open(source, 'w'):
                pass
            os.rename(source, destination)

            settings = mock.Mock()
            settings.get.return_value = [source]
            with mock.patch(
                'FileBrowser.common.sublime.load_settings',
                return_value=settings,
            ):
                with mock.patch(
                    'FileBrowser.common.sublime.save_settings'
                ) as save:
                    self.assertEqual(prune_missing_cut_items(), [])

            settings.set.assert_called_once_with('dired_to_move', [])
            save.assert_called_once_with('dired.sublime-settings')

    def test_preserves_existing_items_without_saving(self):
        with tempfile.TemporaryDirectory() as root:
            source = os.path.join(root, 'source.txt')
            with open(source, 'w'):
                pass

            settings = mock.Mock()
            settings.get.return_value = [source]
            with mock.patch(
                'FileBrowser.common.sublime.load_settings',
                return_value=settings,
            ):
                with mock.patch(
                    'FileBrowser.common.sublime.save_settings'
                ) as save:
                    self.assertEqual(prune_missing_cut_items(), [source])

            settings.set.assert_not_called()
            save.assert_not_called()


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


if __name__ == '__main__':
    unittest.main()
