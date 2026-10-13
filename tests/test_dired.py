import os
import tempfile
import unittest

import sublime
from unittesting import DeferrableTestCase

from FileBrowser.common import prune_missing_cut_items
from FileBrowser.dired import existing_directories


class PruneMissingCutItemsTest(DeferrableTestCase):
    def test_discards_file_moved_by_an_external_application(self):
        with tempfile.TemporaryDirectory() as root:
            source = os.path.join(root, 'source.txt')
            destination = os.path.join(root, 'destination.txt')
            with open(source, 'w'):
                pass
            self.settings.set('dired_to_move', [source])
            sublime.save_settings('dired.sublime-settings')
            yield lambda: self.saved_cut_items() == [source]

            os.rename(source, destination)

            self.assertEqual(prune_missing_cut_items(), [])
            self.assertEqual(self.settings.get('dired_to_move'), [])
            yield lambda: self.saved_cut_items() == []

    def test_preserves_existing_items_without_saving(self):
        with tempfile.TemporaryDirectory() as root:
            source = os.path.join(root, 'source.txt')
            with open(source, 'w'):
                pass
            self.settings.set('dired_to_move', [source])
            sublime.save_settings('dired.sublime-settings')
            yield lambda: self.saved_cut_items() == [source]
            modified = os.stat(self.settings_path).st_mtime_ns

            self.assertEqual(prune_missing_cut_items(), [source])
            self.assertEqual(self.settings.get('dired_to_move'), [source])
            yield 100

            self.assertEqual(self.saved_cut_items(), [source])
            self.assertEqual(os.stat(self.settings_path).st_mtime_ns, modified)

    def setUp(self):
        self.settings = sublime.load_settings('dired.sublime-settings')
        self.settings_path = os.path.join(
            sublime.packages_path(), 'User', 'dired.sublime-settings'
        )
        self.had_cut_items = self.settings.has('dired_to_move')
        self.original_cut_items = self.settings.get('dired_to_move')
        if os.path.exists(self.settings_path):
            with open(self.settings_path, 'rb') as settings_file:
                self.original_file = settings_file.read()
        else:
            self.original_file = None
        self.addCleanup(self.restore_settings)

    def saved_cut_items(self):
        if not os.path.exists(self.settings_path):
            return None
        with open(self.settings_path, encoding='utf-8') as settings_file:
            return sublime.decode_value(settings_file.read()).get('dired_to_move')

    def restore_settings(self):
        if self.had_cut_items:
            self.settings.set('dired_to_move', self.original_cut_items)
        else:
            self.settings.erase('dired_to_move')
        if self.original_file is not None:
            with open(self.settings_path, 'wb') as settings_file:
                settings_file.write(self.original_file)
        elif os.path.exists(self.settings_path):
            os.remove(self.settings_path)


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
