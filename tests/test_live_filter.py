import os
import importlib
import tempfile
from unittest import mock

import sublime
from unittesting import DeferrableViewTestCase

from FileBrowser.dired import DiredRefreshOnFocus
from FileBrowser.common import DiredBaseCommand
from FileBrowser.dired_misc import dired_fuzzy_search
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

            with mock.patch.object(os, 'scandir', wraps=os.scandir) as scan:
                on_done('ap')
                scan.assert_not_called()
            self.assertFalse(settings.get('dired_filter_live'))
