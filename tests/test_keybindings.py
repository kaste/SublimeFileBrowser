import unittest

import sublime


class FileBrowserKeybindingsTest(unittest.TestCase):
    def test_space_uses_a_character_binding_for_toggle_and_move(self):
        binding = self.binding_for(' ')

        self.assertEqual(binding['command'], 'dired_mark')
        self.assertEqual(binding['args'], {'mark': 'toggle'})
        self.assertEqual(binding['context'], self.binding_for('m')['context'])
        self.assertFalse(any(binding['keys'] == ['space'] for binding in self.bindings))

    def test_ctrl_space_clears_marks_and_the_copy_cut_list(self):
        binding = self.binding_for('ctrl+space')

        self.assertEqual(binding['command'], 'dired_clear')
        self.assertNotIn('args', binding)
        self.assertEqual(binding['context'], self.binding_for('u')['context'])

    def test_undo_keys_are_not_bound_in_dired_views(self):
        for platform in ('Windows', 'Linux', 'OSX'):
            bindings = sublime.decode_value(sublime.load_resource(
                'Packages/FileBrowser/Default ({0}).sublime-keymap'.format(platform)
            ))
            for binding in self.bindings + bindings:
                self.assertNotIn(binding['keys'], (['ctrl+z'], ['super+z']), platform)

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
