import os
import tempfile

import sublime
from unittesting import DeferrableViewTestCase

from FileBrowser.show import retarget_dired_view


class HorizontalNavigationTest(DeferrableViewTestCase):
    def test_arrows_move_within_a_file_name(self):
        point = self.point_for(self.first) + 4
        self.select(sublime.Region(point))
        index = self.view.settings().get('dired_index')

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(point + 1)]
        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(point)]

        self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_arrows_collapse_filename_selections_normally(self):
        start = self.point_for(self.first) + 2
        end = start + len('first.txt')
        for forward, expected in ((True, end), (False, start)):
            for selection in (sublime.Region(start, end), sublime.Region(end, start)):
                self.select(selection)
                self.view.run_command('dired_move_horizontal', {'forward': forward})
                yield lambda: list(self.view.sel()) == [sublime.Region(expected)]

    def test_arrows_move_multiple_file_cursors(self):
        points = [self.point_for(path) + 4 for path in (self.first, self.second)]
        self.select(*(sublime.Region(point) for point in points))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(point + 1) for point in points]
        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(point) for point in points]

    def test_right_stops_at_file_eol_instead_of_jumping_to_the_next_line(self):
        yield from self.expand_folder()
        index = self.view.settings().get('dired_index')
        for path in (self.first, self.second, self.child):
            end = self.view.line(self.point_for(path)).b
            self.select(sublime.Region(end - 1))

            self.view.run_command('dired_move_horizontal', {'forward': True})
            yield lambda: list(self.view.sel()) == [sublime.Region(end)]
            self.view.run_command('dired_move_horizontal', {'forward': True})
            yield 0

            self.assertEqual(list(self.view.sel()), [sublime.Region(end)])
            self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_right_holds_eol_cursors_while_other_cursors_move(self):
        end = self.view.line(self.point_for(self.first)).b
        point = self.point_for(self.second) + 4
        self.select(sublime.Region(end), sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        expected = sorted([sublime.Region(end), sublime.Region(point + 1)])
        yield lambda: list(self.view.sel()) == expected

    def test_right_collapses_select_all_without_expanding_directories(self):
        index = self.view.settings().get('dired_index')
        expanded = self.view.settings().get('dired_expanded_paths')
        self.view.run_command('select_all')
        yield lambda: list(self.view.sel()) == [sublime.Region(0, self.view.size())]

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(self.view.size())]

        self.assertEqual(self.view.settings().get('dired_index'), index)
        self.assertEqual(self.view.settings().get('dired_expanded_paths'), expanded)

    def test_right_collapses_reverse_select_all_without_expanding_directories(self):
        index = self.view.settings().get('dired_index')
        self.select(sublime.Region(self.view.size(), 0))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(self.view.size())]

        self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_right_collapses_select_all_without_changing_existing_expansions(self):
        yield from self.expand_folder()
        index = self.view.settings().get('dired_index')
        expanded = self.view.settings().get('dired_expanded_paths')
        self.view.run_command('select_all')
        yield lambda: not self.view.sel()[0].empty()

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(self.view.size())]

        self.assertEqual(self.view.settings().get('dired_index'), index)
        self.assertEqual(self.view.settings().get('dired_expanded_paths'), expanded)

    def test_right_collapses_a_directory_name_selection_without_expanding(self):
        start = self.point_for(self.folder) + 2
        end = start + len('folder')
        self.select(sublime.Region(start, end))
        index = self.view.settings().get('dired_index')

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(end)]

        self.assertEqual(self.view.settings().get('dired_index'), index)
        self.assertNotIn(self.child, self.view.settings().get('dired_index'))

    def test_right_collapses_a_selection_ending_at_eol(self):
        start = self.point_for(self.first) + 2
        end = self.view.line(start).b
        self.select(sublime.Region(start, end))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(end)]

    def test_right_from_bol_or_the_prefix_jumps_to_the_current_item_name(self):
        yield from self.expand_folder()
        index = self.view.settings().get('dired_index')
        for path, prefix_length in ((self.first, 2), (self.folder, 2), (self.child, 3)):
            bol = self.point_for(path)
            for offset in range(prefix_length):
                self.select(sublime.Region(bol + offset))

                self.view.run_command('dired_move_horizontal', {'forward': True})
                yield lambda: list(self.view.sel()) == [sublime.Region(bol + prefix_length)]

                self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_right_handles_prefix_and_interior_cursors_together(self):
        yield from self.expand_folder()
        bol = self.point_for(self.child)
        point = self.point_for(self.first) + 4
        self.select(sublime.Region(bol), sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        expected = sorted([sublime.Region(bol + 3), sublime.Region(point + 1)])
        yield lambda: list(self.view.sel()) == expected

    def test_left_from_bol_moves_to_the_previous_item_name_without_folding(self):
        yield from self.expand_folder()
        index = self.view.settings().get('dired_index')
        for path, previous in (
            (self.first, self.child),
            (self.second, self.first),
            (self.child, self.folder),
            (self.folder, self.folder),
        ):
            self.select(sublime.Region(self.point_for(path)))
            prefix_length = 3 if previous == self.child else 2
            expected = self.point_for(previous) + prefix_length

            self.view.run_command('dired_move_horizontal', {'forward': False})
            yield lambda: list(self.view.sel()) == [sublime.Region(expected)]

            self.assertEqual(self.view.settings().get('dired_index'), index)

    def test_left_handles_bol_and_interior_cursors_together(self):
        yield from self.expand_folder()
        point = self.point_for(self.first) + 4
        self.select(sublime.Region(self.point_for(self.child)), sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        expected = sorted([
            sublime.Region(self.point_for(self.folder) + 2), sublime.Region(point - 1)
        ])
        yield lambda: list(self.view.sel()) == expected

        self.assertIn(self.child, self.view.settings().get('dired_index'))

    def test_marked_directory_does_not_prevent_file_cursor_movement(self):
        self.view.settings().set('dired_marked_paths', [self.folder])
        point = self.point_for(self.first) + 4
        self.select(sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: list(self.view.sel()) == [sublime.Region(point + 1)]

        self.assertNotIn(self.child, self.view.settings().get('dired_index'))
        self.assertEqual(self.view.settings().get('dired_marked_paths'), [self.folder])

    def test_left_on_a_nested_file_does_not_collapse_its_parent(self):
        yield from self.expand_folder()
        point = self.point_for(self.child) + 4
        self.select(sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(point - 1)]

        self.assertIn(self.child, self.view.settings().get('dired_index'))
        self.assertIn(self.folder, self.view.settings().get('dired_expanded_paths'))

    def test_arrows_still_expand_and_collapse_directories(self):
        yield from self.expand_folder()
        self.select(sublime.Region(self.point_for(self.folder) + 2))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: self.child not in self.view.settings().get('dired_index')

        self.assertNotIn(self.folder, self.view.settings().get('dired_expanded_paths'))

    def test_mixed_file_and_directory_cursors_still_expand_directories(self):
        self.select(
            sublime.Region(self.point_for(self.folder) + 2),
            sublime.Region(self.point_for(self.first) + 2),
        )

        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: self.child in self.view.settings().get('dired_index')

        self.assertIn(self.first, self.view.settings().get('dired_index'))
        self.assertIn(self.folder, self.view.settings().get('dired_expanded_paths'))

    def test_left_collapses_a_directory_name_selection_without_folding(self):
        yield from self.expand_folder()
        start = self.point_for(self.folder) + 2
        for selection in (sublime.Region(start, start + 3), sublime.Region(start + 3, start)):
            self.select(selection)

            self.view.run_command('dired_move_horizontal', {'forward': False})
            yield lambda: list(self.view.sel()) == [sublime.Region(start)]

            self.assertIn(self.child, self.view.settings().get('dired_index'))

    def test_left_moves_inside_directory_names_without_folding(self):
        yield from self.expand_folder()
        point = self.point_for(self.folder) + 4
        self.select(sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(point - 1)]

        self.assertIn(self.child, self.view.settings().get('dired_index'))

    def test_left_stops_at_a_root_file_name_without_entering_the_prefix(self):
        start = self.point_for(self.first) + 2
        self.select(sublime.Region(start + 1))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(start)]
        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield 0

        self.assertEqual(list(self.view.sel()), [sublime.Region(start)])
        self.assertIn(self.first, self.view.settings().get('dired_index'))

    def test_left_at_a_nested_file_name_start_collapses_its_parent(self):
        yield from self.expand_folder()
        # The child line has one indentation tab before the icon and space.
        start = self.point_for(self.child) + 3
        self.select(sublime.Region(start))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: self.child not in self.view.settings().get('dired_index')

        self.assertNotIn(self.folder, self.view.settings().get('dired_expanded_paths'))

    def test_left_preserves_boundary_cursors_when_other_cursors_can_move(self):
        yield from self.expand_folder()
        boundary = self.point_for(self.child) + 3
        point = self.point_for(self.first) + 4
        self.select(sublime.Region(boundary), sublime.Region(point))

        self.view.run_command('dired_move_horizontal', {'forward': False})
        expected = sorted([sublime.Region(boundary), sublime.Region(point - 1)])
        yield lambda: list(self.view.sel()) == expected

        self.assertIn(self.child, self.view.settings().get('dired_index'))

    def test_left_collapses_a_line_selection_without_folding(self):
        yield from self.expand_folder()
        self.select(sublime.Region(self.point_for(self.child) + 4))
        # This is the built-in command used by Ctrl+L.
        self.view.run_command('expand_selection', {'to': 'line'})
        yield lambda: not self.view.sel()[0].empty()
        start = self.view.sel()[0].begin()

        self.view.run_command('dired_move_horizontal', {'forward': False})
        yield lambda: list(self.view.sel()) == [sublime.Region(start)]

        self.assertIn(self.child, self.view.settings().get('dired_index'))

    def test_explicit_fold_still_collapses_from_a_nested_file(self):
        yield from self.expand_folder()
        self.select(sublime.Region(self.point_for(self.child) + 4))

        self.view.run_command('dired_fold')
        yield lambda: self.child not in self.view.settings().get('dired_index')

        self.assertNotIn(self.folder, self.view.settings().get('dired_expanded_paths'))

    def setUp(self):
        super().setUp()
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = directory.name + os.sep
        self.folder = os.path.join(self.root, 'folder') + os.sep
        self.first = os.path.join(self.root, 'first.txt')
        self.second = os.path.join(self.root, 'second.txt')
        self.child = os.path.join(self.folder, 'child.txt')
        os.mkdir(self.folder)
        for path in (self.first, self.second, self.child):
            with open(path, 'w'):
                pass
        self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
        self.view.settings().set('dired_autorefresh', False)
        self.view.settings().set('dired_header', False)
        self.view.settings().set('dired_show_parent', False)
        self.view.settings().set('dired_show_stats', False)
        self.view.settings().set('dired_filter', '')
        self.view.settings().set('dired_filter_extension', '')
        retarget_dired_view(self.view, self.root)
        yield lambda: self.first in (self.view.settings().get('dired_index') or [])

    def expand_folder(self):
        self.select(sublime.Region(self.point_for(self.folder) + 2))
        self.view.run_command('dired_move_horizontal', {'forward': True})
        yield lambda: self.child in self.view.settings().get('dired_index')

    def point_for(self, path):
        row = self.view.settings().get('dired_index').index(path)
        return self.view.text_point(row, 0)

    def select(self, *regions):
        self.view.sel().clear()
        for region in regions:
            self.view.sel().add(region)
