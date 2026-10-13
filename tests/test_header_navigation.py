import os
import tempfile

import sublime
from unittesting import DeferrableViewTestCase

from FileBrowser.common import PARENT_SYM
from FileBrowser.show import retarget_dired_view


class HeaderNavigationTest(DeferrableViewTestCase):
    def test_up_from_parent_link_jumps_to_bof_for_copying_the_header_path(self):
        self.select(sublime.Region(self.point_for(PARENT_SYM)))

        self.view.run_command('dired_next_line', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(0)]
        self.view.run_command('expand_selection', {'to': 'line'})
        yield lambda: not self.view.sel()[0].empty()

        self.assertIn(self.root, self.view.substr(self.view.sel()[0]))

    def test_up_without_parent_link_jumps_from_first_entry_to_bof(self):
        self.view.settings().set('dired_show_parent', False)
        self.view.run_command('dired_refresh')
        yield lambda: PARENT_SYM not in self.view.settings().get('dired_index')
        self.select(sublime.Region(self.point_for(self.folder) + 2))

        self.view.run_command('dired_next_line', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(0)]

    def test_down_from_header_returns_to_first_entry(self):
        self.select(sublime.Region(0))

        self.view.run_command('dired_next_line', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(self.point_for(PARENT_SYM))]

    def test_up_from_bof_stays_at_bof(self):
        self.select(sublime.Region(0))

        self.view.run_command('dired_next_line', {'forward': False})
        yield 0

        self.assertEqual(list(self.view.sel()), [sublime.Region(0)])

    def test_up_handles_header_and_item_cursors_independently(self):
        self.select(
            sublime.Region(self.point_for(PARENT_SYM)),
            sublime.Region(self.point_for(self.file) + 2),
        )

        self.view.run_command('dired_next_line', {'forward': False})
        expected = [sublime.Region(0), sublime.Region(self.point_for(self.folder) + 2)]
        yield lambda: list(self.view.sel()) == expected

    def test_left_from_first_entry_bol_jumps_to_header_eol(self):
        self.select(sublime.Region(self.point_for(PARENT_SYM)))
        end = self.view.line(0).b

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(end)]

    def test_header_arrows_move_normally(self):
        self.select(sublime.Region(0))
        index = self.view.settings().get('dired_index')

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(1)]
        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(0)]
        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield 0

        self.assertEqual(list(self.view.sel()), [sublime.Region(0)])
        self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_header_right_at_eol_jumps_to_first_entry_bol(self):
        end = self.view.line(0).b
        self.select(sublime.Region(end))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(self.point_for(PARENT_SYM))]

    def test_header_boundary_shortcuts_work_without_a_parent_link(self):
        self.view.settings().set('dired_show_parent', False)
        self.view.run_command('dired_refresh')
        yield lambda: PARENT_SYM not in self.view.settings().get('dired_index')
        bol = self.point_for(self.folder)
        end = self.view.line(0).b
        self.select(sublime.Region(bol))
        index = self.view.settings().get('dired_index')

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(end)]
        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(bol)]

        self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_header_boundary_shortcuts_preserve_other_cursors(self):
        end = self.view.line(0).b
        bol = self.point_for(PARENT_SYM)
        point = self.point_for(self.file) + 4
        self.select(sublime.Region(bol), sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        expected = [sublime.Region(end), sublime.Region(point - 1)]
        yield lambda: list(self.view.sel()) == expected

        self.select(sublime.Region(4), sublime.Region(end))
        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(5), sublime.Region(bol)]

    def test_header_arrows_collapse_path_selections_normally(self):
        end = self.view.line(0).b
        for forward, expected in ((True, end), (False, 0)):
            for selection in (sublime.Region(0, end), sublime.Region(end, 0)):
                self.select(selection)

                self.view.run_command('dired_move_horizontal', {'forward': forward})
                yield lambda: list(self.view.sel()) == [sublime.Region(expected)]

    def setUp(self):
        super().setUp()
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = directory.name + os.sep
        self.folder = os.path.join(self.root, 'folder') + os.sep
        self.file = os.path.join(self.root, 'file.txt')
        os.mkdir(self.folder)
        with open(self.file, 'w'):
            pass
        self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
        self.view.settings().set('dired_autorefresh', False)
        self.view.settings().set('dired_header', True)
        self.view.settings().set('dired_show_parent', True)
        self.view.settings().set('dired_show_stats', False)
        self.view.settings().set('dired_filter', '')
        self.view.settings().set('dired_filter_extension', '')
        retarget_dired_view(self.view, self.root)
        yield lambda: self.file in self.view.settings().get('dired_index', [])
        self.assertEqual(self.view.settings().get('dired_index')[:2], ['', ''])

    def point_for(self, path):
        row = self.view.settings().get('dired_index').index(path)
        return self.view.text_point(row, 0)

    def select(self, *regions):
        self.view.sel().clear()
        self.view.sel().add_all(regions)
