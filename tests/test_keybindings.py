import unittest

import sublime


class FileBrowserKeybindingsTest(unittest.TestCase):
    def test_space_uses_a_character_binding_for_toggle_and_move(self):
        binding = self.binding_for(' ')

        self.assertEqual(binding['command'], 'dired_mark')
        self.assertEqual(binding['args'], {'mark': 'toggle'})
        self.assertEqual(binding['context'], self.binding_for('m')['context'])
        self.assertFalse(any(binding['keys'] == ['space'] for binding in self.bindings))

    def test_ctrl_space_is_an_alias_for_unmark_all(self):
        binding = self.binding_for('ctrl+space')
        unmark = self.binding_for('u')

        self.assertEqual(binding['command'], 'dired_mark')
        self.assertEqual(binding['args'], {'mark': False, 'markall': True})
        self.assertEqual(binding['command'], unmark['command'])
        self.assertEqual(binding['args'], unmark['args'])
        self.assertEqual(binding['context'], unmark['context'])

    def test_arrows_use_file_aware_navigation(self):
        for key, forward in (('right', True), ('left', False)):
            matches = [
                binding for binding in self.bindings
                if binding['keys'] == [key]
                and binding['command'] == 'dired_move_horizontal'
            ]
            self.assertEqual(len(matches), 1)
            self.assertEqual(matches[0]['args'], {'forward': forward})
            self.assertEqual(matches[0]['context'], self.binding_for('m')['context'])

    def test_h_and_l_keep_explicit_directory_navigation(self):
        self.assertEqual(self.binding_for('h')['command'], 'dired_fold')
        self.assertEqual(self.binding_for('l')['command'], 'dired_expand')

    def setUp(self):
        self.bindings = sublime.decode_value(
            sublime.load_resource('Packages/FileBrowser/Default.sublime-keymap')
        )

    def binding_for(self, key):
        matches = [binding for binding in self.bindings if binding['keys'] == [key]]
        self.assertEqual(len(matches), 1)
        return matches[0]
