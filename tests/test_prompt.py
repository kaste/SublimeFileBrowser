import os
import tempfile
import unittest
from unittest import mock

from FileBrowser.prompt import dired_prompt, PromptContext


class DirectoryPromptTest(unittest.TestCase):
    def test_resolves_relative_directory_before_calling_back(self):
        command, callback = self.make_command()

        command.on_done('.')

        callback.assert_called_once_with(os.path.abspath('.'), 'extra')

    def test_preserves_absolute_directory(self):
        command, callback = self.make_command()
        with tempfile.TemporaryDirectory() as root:
            command.on_done(root)

            callback.assert_called_once_with(os.path.abspath(root), 'extra')

    def test_reopens_prompt_for_missing_directory(self):
        command, callback = self.make_command()
        with tempfile.TemporaryDirectory() as root:
            missing = os.path.join(root, 'missing')
            with mock.patch('FileBrowser.prompt.sublime.error_message') as error:
                command.on_done(missing)

        callback.assert_not_called()
        error.assert_called_once()
        command.window.run_command.assert_called_once_with('dired_prompt')

    def make_command(self):
        callback = mock.Mock()
        command = dired_prompt(mock.Mock())
        command.ctx = PromptContext('Directory:', '.', callback, 'extra')
        return command, callback


if __name__ == '__main__':
    unittest.main()
