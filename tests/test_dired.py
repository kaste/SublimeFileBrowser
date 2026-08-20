import os
import tempfile
import unittest

from FileBrowser.dired import existing_directories


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
