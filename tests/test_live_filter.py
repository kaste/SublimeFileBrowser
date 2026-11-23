import os
import importlib
import tempfile
from unittest import mock

import sublime
from unittesting import DeferrableViewTestCase

from FileBrowser.dired import DiredRefreshOnFocus
from FileBrowser.common import DiredBaseCommand, listings
from FileBrowser.dired_misc import DiredFilterPanelFocus, dired_fuzzy_search
from FileBrowser.show import retarget_dired_view


class LiveFilterTest(DeferrableViewTestCase):
    def test_one_off_listings_are_fresh_without_evicting_a_cached_walk(self):
        command = DiredBaseCommand()
        command.view = self.view
        with tempfile.TemporaryDirectory() as root, tempfile.TemporaryDirectory() as other:
            command.walkdir(root)
            self.assertEqual(command.list_directory(other), ([], ''))
            open(os.path.join(other, 'new'), 'w').close()
            self.assertEqual(command.list_directory(other), (['new'], ''))
            with mock.patch.object(os, 'scandir', wraps=os.scandir) as scan:
                command.walkdir(root)
                scan.assert_not_called()

    def test_first_hidden_toggle_reuses_the_unfiltered_scan(self):
        with tempfile.TemporaryDirectory() as root:
            hidden = os.path.join(root, '.hidden')
            shown = os.path.join(root, 'shown')
            for path in (hidden, shown):
                open(path, 'w').close()
            self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
            settings = self.view.settings()
            settings.set('dired_autorefresh', False)
            settings.set('dired_show_hidden_files', False)
            settings.set('dired_hidden_files_patterns', ['.*'])
            retarget_dired_view(self.view, root)
            self.assertNotIn(hidden, settings.get('dired_index'))
            with mock.patch.object(os, 'scandir', wraps=os.scandir) as scan:
                self.view.run_command('dired_toggle_hidden_files')
                self.assertIn(hidden, settings.get('dired_index'))
                self.view.run_command('dired_toggle_hidden_files')
                self.assertNotIn(hidden, settings.get('dired_index'))
                scan.assert_not_called()

    def test_recursive_search_borrows_cached_scans_without_admitting_misses(self):
        command = DiredBaseCommand()
        command.view = self.view
        self.view.settings().set('binary_file_patterns', [])
        with tempfile.TemporaryDirectory() as root:
            warm = os.path.join(root, 'warm')
            cold = os.path.join(root, 'cold')
            os.mkdir(warm)
            os.mkdir(cold)
            command.walkdir(root)
            command.walkdir(warm)
            with mock.patch.object(os, 'scandir', wraps=os.scandir) as scan:
                command.walkdir(root, auto_expand=3)
                scan.assert_called_once_with(cold)
            open(os.path.join(cold, 'new'), 'w').close()
            rows, err = command.walkdir(cold)
            self.assertFalse(err)
            self.assertEqual([e.name for e in rows], ['new'])

    def test_focus_only_refreshes_as_a_fallback_for_autorefresh(self):
        with tempfile.TemporaryDirectory() as root:
            self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
            settings = self.view.settings()
            settings.set('dired_autorefresh', False)
            retarget_dired_view(self.view, root)
            path = os.path.join(root, 'new')
            open(path, 'w').close()
            handler = DiredRefreshOnFocus()
            watcher = importlib.import_module('FileBrowser.0_dired_fs_observer')
            with mock.patch.object(watcher, 'observer', None):
                handler.on_activated(self.view)
                self.assertNotIn(path, settings.get('dired_index'))

            settings.set('dired_autorefresh', True)
            with mock.patch.object(watcher, 'observer', mock.Mock()):
                handler.on_activated(self.view)
                self.assertNotIn(path, settings.get('dired_index'))
            with mock.patch.object(watcher, 'observer', None):
                handler.on_activated(self.view)
                self.assertIn(path, settings.get('dired_index'))

    def test_focused_search_survives_watcher_refresh_and_thaws_on_blur(self):
        with tempfile.TemporaryDirectory() as root:
            child = os.path.join(root, 'project')
            os.mkdir(child)
            hit = os.path.join(child, 'venv')
            other = os.path.join(child, 'vendor')
            for path in (hit, other):
                open(path, 'w').close()
            self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
            settings = self.view.settings()
            settings.set('dired_autorefresh', False)
            retarget_dired_view(self.view, root)
            with mock.patch.object(sublime.View, 'window') as window:
                dired_fuzzy_search(self.view).run(None)
            done, change = window.return_value.show_input_panel.call_args.args[2:4]
            change('ven')
            self.assertIn(hit, settings.get('dired_index'))
            with mock.patch.object(DiredBaseCommand, 'walkdir') as walk:
                change('venv')
                self.assertNotIn(other, settings.get('dired_index'))
                change('ven')
                self.assertIn(other, settings.get('dired_index'))
                walk.assert_not_called()
            os.remove(hit)
            listings.invalidate(child)
            self.view.run_command('dired_refresh')  # debounced watcher refresh
            self.assertIn(hit, settings.get('dired_index'))
            change('venv')
            self.assertIn(hit, settings.get('dired_index'))

            panel = mock.Mock()
            panel.settings.return_value.get.return_value = self.view.id()
            focus = DiredFilterPanelFocus()
            with mock.patch.object(sublime, 'set_timeout', side_effect=lambda f: f()):
                focus.on_deactivated(panel)
            self.assertNotIn(hit, settings.get('dired_index'))
            focus.on_activated(panel)
            open(hit, 'w').close()
            listings.invalidate(child)
            change('venv')
            self.assertNotIn(hit, settings.get('dired_index'))
            done('venv')
            self.assertIn(hit, settings.get('dired_index'))

    def test_edits_reuse_listings_but_refresh_reads_the_filesystem(self):
        with tempfile.TemporaryDirectory() as root:
            first = os.path.join(root, 'apple')
            other = os.path.join(root, 'ant')
            new = os.path.join(root, 'apricot')
            for path in (first, other):
                open(path, 'w').close()
            settings = self.view.settings()
            self.view.assign_syntax('Packages/FileBrowser/dired.sublime-syntax')
            settings.set('dired_autorefresh', False)
            retarget_dired_view(self.view, root)
            yield lambda: first in settings.get('dired_index', [])
            with mock.patch.object(sublime.View, 'window') as window:
                dired_fuzzy_search(self.view).run(None)
            on_done, on_change = window.return_value.show_input_panel.call_args.args[2:4]

            on_change('a')
            self.assertIn(other, settings.get('dired_index'))
            with mock.patch.object(os, 'scandir', wraps=os.scandir) as scan:
                on_change('ap')
                scan.assert_not_called()
            self.assertIn(first, settings.get('dired_index'))
            self.assertNotIn(other, settings.get('dired_index'))

            open(new, 'w').close()
            self.view.run_command('dired_refresh')
            self.assertNotIn(new, settings.get('dired_index'))
            self.view.run_command('dired_rescan')
            self.assertIn(new, settings.get('dired_index'))
            self.assertNotIn(other, settings.get('dired_index'))

            on_done('ap')
            self.assertIn(new, settings.get('dired_index'))
            self.assertFalse(settings.get('dired_filter_live'))
