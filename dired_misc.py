'''This module contains miscellaneous commands for additional functionality.
    Suppose these things are useful, but not essential.
'''

import glob
from itertools import chain
import math
import operator as op
import os
import subprocess
import sys
import threading
from os.path import (
    dirname, isfile, isdir, exists, join, normpath,
    getsize, getctime, getatime, getmtime)
from datetime import datetime

import sublime
import sublime_plugin
from sublime import Region
from sublime_plugin import TextCommand, EventListener

from .common import (
    DiredBaseCommand, hijack_window, get_group, emit_event,
    MARK_OPTIONS, NT, OSX, PARENT_SYM, sort_nicely)


STARTUPINFO = None
if sys.platform == "win32":
    STARTUPINFO = subprocess.STARTUPINFO()
    STARTUPINFO.dwFlags |= subprocess.STARTF_USESHOWWINDOW


def convert_size(size):
    if not size:
        return '0 B'
    size_name = ("B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
    i = int(math.floor(math.log(size, 1024)))
    p = math.pow(1024, i)
    s = round(size / p, 2)
    return '%s %s' % (s, size_name[i])


def get_dates(path):
    try:
        created = datetime.fromtimestamp(getctime(path)).strftime('%d %b %Y, %H:%M:%S')
    except OSError as e:
        created = e
    try:
        accessed = datetime.fromtimestamp(getatime(path)).strftime('%d %b %Y, %H:%M:%S')
    except OSError as e:
        accessed = e
    try:
        modified = datetime.fromtimestamp(getmtime(path)).strftime('%d %b %Y, %H:%M:%S')
    except OSError as e:
        modified = e
    return created, accessed, modified


class dired_find_in_files(TextCommand, DiredBaseCommand):
    def run(self, edit):
        self.load_index()
        path = self.path
        if path == 'ThisPC\\':
            path  = ''
            items = self.get_marked() or self.get_selected()
        else:
            items = self.get_marked()
        where = ', '.join(join(path, p) for p in items) or path or ''
        args  = {"panel": "find_in_files", "where": where, "replace": "", "reverse": "false"}
        sublime.active_window().run_command("show_panel", args)


# HELP ##############################################################


PANEL_NAME = "dired_help"
HELP_VIEW_SETTINGS = {
    'rulers': [],
    'color_scheme': 'Packages/FileBrowser/dired.hidden-tmTheme',
    'syntax': 'Packages/FileBrowser/dired-help.sublime-syntax',
    'margin': 16,
    'line_numbers': False,
    'gutter': False,
    'fold_buttons': False,
    'draw_indent_guides': False,
    'word_wrap': False,
    'spell_check': False,
    'drag_text': False,
}

def ensure_panel(window: sublime.Window) -> sublime.View:
    panel = window.find_output_panel(PANEL_NAME)
    if panel:
        return panel
    panel = window.create_output_panel(PANEL_NAME)
    panel.set_read_only(True)
    return panel


def show_panel(window: sublime.Window) -> sublime.View:
    panel = ensure_panel(window)
    window.run_command("show_panel", {"panel": "output.{}".format(PANEL_NAME)})
    return panel


class dired_help(sublime_plugin.TextCommand):
    def run(self, edit):
        window = self.view.window()
        if not window:
            return
        panel = show_panel(window)
        for key, value in HELP_VIEW_SETTINGS.items():
            panel.settings().set(key, value)
        # load help content
        content = sublime.load_resource('Packages/FileBrowser/shortcuts.md')
        if not content:
            window.status_message("Could not load 'Packages/FileBrowser/shortcuts.md'.")
            return
        # normalize line endings
        content = content.replace('\r\n', '\n').replace('\r', '\n')
        panel.set_read_only(False)
        panel.run_command("select_all")
        panel.run_command("right_delete")
        panel.run_command("append", {"characters": content})
        panel.set_read_only(True)
        panel.sel().clear()
        panel.show(0)


# OTHER #############################################################

class dired_toggle_project_folder(TextCommand, DiredBaseCommand):
    def run(self, edit):
        path = self.path.rstrip(os.sep)
        data = self.view.window().project_data() or {}
        data['folders'] = data.get('folders', {})
        folders = [f for f in data['folders'] if f['path'] != path]
        if len(folders) == len(data['folders']):
            folders.insert(0, {'path': path})
        data['folders'] = folders
        self.view.window().set_project_data(data)
        self.view.window().run_command('dired_refresh')


class dired_only_one_project_folder(TextCommand, DiredBaseCommand):
    def run(self, edit):
        path = self.path.rstrip(os.sep)
        msg = (
            "Set '{0}' as only one project folder "
            "(will remove all other folders from project)?"
            .format(path)
        )
        if sublime.ok_cancel_dialog(msg):
            data = self.view.window().project_data() or {'folders': {}}
            data['folders'] = [{'path': path}]
            self.view.window().set_project_data(data)
            self.view.window().run_command('dired_refresh')


class dired_quick_look(TextCommand, DiredBaseCommand):
    """
    quick look current file in mac or open in default app on other OSs
    """
    def run(self, edit, preview=True, files=None):
        self.load_index()
        files = files or self.get_marked() or self.get_selected(parent=False)
        if not files:
            return sublime.status_message('Nothing chosen')
        if OSX and preview:
            cmd = ["qlmanage", "-p"]
            for filename in files:
                fqn = join(self.path, filename)
                cmd.append(fqn)
            subprocess.call(cmd)
        else:
            if OSX:
                launch = lambda f: subprocess.call(['open', f], cwd=dirname(f))
            elif NT:
                # the "" before filename is a trick for batch files and such
                launch = lambda f: subprocess.call('start "" "%s"' % f, shell=True, cwd=dirname(f))
            else:
                launch = lambda f: subprocess.call(['xdg-open', f], cwd=dirname(f))
            for filename in files:
                fqn = join(self.path, filename)
                launch(fqn)


class dired_open_external(TextCommand, DiredBaseCommand):
    """open dir/file in external file explorer"""
    def run(self, edit, fname=None):
        path = self.path
        if not fname:
            self.load_index()
            files = self.get_selected(parent=False)
            fname = join(path, files[0] if files else '')
        else:
            files = True
        p, f  = os.path.split(fname.rstrip(os.sep))

        if not exists(fname):
            return sublime.status_message('Directory doesn’t exist “%s”' % path)

        if NT and path == 'ThisPC\\':
            return subprocess.Popen('explorer /select,"%s"' % fname)

        if files:
            self.view.window().run_command("open_dir", {"dir": p, "file": f})
        else:
            self.view.window().run_command("open_dir", {"dir": path})


class dired_open_in_new_window(TextCommand, DiredBaseCommand):
    def run(self, edit, project_folder=False):
        if project_folder:
            files = project_folder
        else:
            self.load_index()
            files = self.get_marked(full=True) or self.get_selected(parent=False, full=True)

        if not files:
            return sublime.status_message('Nothing chosen')

        self.open_in_subl(files)

        def run_on_new_window():
            settings = sublime.load_settings('dired.sublime-settings')
            open_on_jump = settings.get('dired_open_on_jump', 'left')

            if open_on_jump:
                options = {"immediate": True, "project": True}

                if open_on_jump in ['left', 'right']:
                    options["other_group"] = open_on_jump

                sublime.active_window().run_command("dired", options)

        sublime.set_timeout(run_on_new_window, 200)

    def open_in_subl(self, files):
        executable_path = sublime.executable_path()
        if OSX:
            app_path = executable_path[:executable_path.rfind(".app/") + 5]
            executable_path = app_path + "Contents/SharedSupport/bin/subl"
        items = [executable_path, "-n"] + files
        subprocess.Popen(items, cwd=None if NT else self.path)


class dired_toggle_auto_refresh(TextCommand):
    def is_enabled(self):
        return self.view.score_selector(0, "text.dired") > 0

    def is_visible(self):
        return self.is_enabled()

    def description(self):
        msg = 'auto-refresh for this view'
        if self.view.settings().get('dired_autorefresh', True):
            return 'Disable ' + msg
        else:
            return 'Enable ' + msg

    def run(self, edit):
        s = self.view.settings()
        ar = s.get('dired_autorefresh', True)
        s.set('dired_autorefresh', not ar)
        self.view.run_command('dired_refresh')



# FILTER ############################################################

class dired_fuzzy_search(TextCommand, DiredBaseCommand):
    def is_enabled(self):
        return self.view.score_selector(0, "text.dired") > 0

    def run(self, edit):
        window = self.view.window()
        if not window:
            return

        current = self.view.settings().get('dired_filter') or ''
        enabled = self.view.settings().get('dired_filter_enabled', True)
        filter_extension = self.view.settings().get('dired_filter_extension', '')
        self.load_index()
        initial_sels = (self.get_selected(), [Region(r.a, r.b) for r in self.view.sel()])
        initial_viewport = self.view.viewport_position()
        initial_expanded_folders: list[str] = self.view.settings().get('dired_expanded_paths', [])
        auto_expand = 0
        auto_expand_trigger = ''

        def apply_filter(text: str):
            if text:
                self.view.settings().set('dired_filter', text.strip())
            else:
                self.view.settings().erase('dired_filter')
            self.view.run_command('dired_refresh', {'auto_expand': auto_expand})

        def on_done(text: str):
            self.view.settings().set('dired_filter_live', False)
            self.recreate_dired_expanded_paths_from_view()
            apply_filter(text)

        def on_change(text: str):
            nonlocal auto_expand, auto_expand_trigger

            text = text.strip()

            # If the current term still starts with the trigger that caused
            # auto expansion, keep the current auto_expand depth and just
            # re-apply the filter.
            if auto_expand_trigger and text.startswith(auto_expand_trigger):
                apply_filter(text)
                return

            # Reset auto expansion state whenever the term diverges from the
            # previous trigger (including backspacing below it).
            auto_expand = 0
            auto_expand_trigger = ''

            # First try without auto expansion.
            apply_filter(text)

            # If live fuzzy filtering yields no matches at the current depth,
            # enable auto expansion up to three levels and try again.
            if self.view.settings().get('dired_count', 0) == 0:
                auto_expand = 3
                auto_expand_trigger = text
                apply_filter(text)

        def on_cancel():
            self.view.settings().set('dired_filter_live', False)
            if current:
                self.view.settings().set('dired_filter', current)
            else:
                self.view.settings().erase('dired_filter')
            self.view.settings().set('dired_filter_enabled', enabled)
            self.view.settings().set('dired_filter_extension', filter_extension)
            self.view.settings().set('dired_expanded_paths', initial_expanded_folders)
            self.view.run_command('dired_refresh')
            # After a `dired_refresh` we need to refresh `self.index` by calling
            # `load_index()`.  Even when we intend to to "just" revert to the
            # exact same state from before, `dired_refresh` still calls `listdir`
            # et.al. and hence has a fresh view of the state of the filesystem.
            self.load_index()
            self.restore_sels(initial_sels)
            self.view.set_viewport_position(initial_viewport, False)

        self.view.settings().set('dired_filter_live', True)
        self.view.settings().set('dired_filter_enabled', True)
        if not enabled and filter_extension:
            self.view.settings().erase('dired_filter_extension')

        pv = window.show_input_panel('Filter:', current if enabled else "", on_done, on_change, on_cancel)
        pv.settings().set('dired_input_panel', True)
        pv.settings().set('dired_target_view_id', self.view.id())
        if enabled:
            pv.run_command('select_all')


class dired_toggle_filter(TextCommand, DiredBaseCommand):
    def is_enabled(self):
        return self.view.score_selector(0, "text.dired") > 0

    def run(self, edit):
        s = self.view.settings()
        flt = s.get('dired_filter')
        if not flt and not s.get('dired_filter_extension'):
            sublime.status_message('FileBrowser: No filter set')
            return
        enabled = s.get('dired_filter_enabled', True)
        s.set('dired_filter_enabled', not enabled)
        # Refresh and update highlight according to new state
        self.view.run_command('dired_refresh')
        state = 'On' if not enabled else 'Off'
        sublime.status_message('FileBrowser: Filter {}'.format(state))


class dired_filter_by_extension(TextCommand, DiredBaseCommand):
    def is_enabled(self):
        return self.view.score_selector(0, "text.dired") > 0

    def run(self, edit):
        # Determine filename under cursor
        self.load_index()
        names = self.get_selected(parent=False) or []
        if not names:
            sublime.status_message('FileBrowser: Nothing selected')
            return

        name = names[0]
        # Ignore directories and parent link
        if name.endswith(os.sep) or name == PARENT_SYM:
            sublime.status_message('FileBrowser: Not a file')
            return

        _, ext = os.path.splitext(name)
        if not ext:
            sublime.status_message('FileBrowser: No extension')
            return

        s = self.view.settings()
        enabled = s.get('dired_filter_enabled', True)
        current_ext = s.get('dired_filter_extension', '')
        if enabled and current_ext.lower() == ext.lower():
            s.erase('dired_filter_extension')
            sublime.status_message('FileBrowser: Cleared extension filter')
        else:
            s.set('dired_filter_extension', ext)
            if not enabled:
                s.erase('dired_filter')
            sublime.status_message('FileBrowser: Extension filter {}'.format(ext))
        s.set('dired_filter_enabled', True)
        s.set('dired_filter_live', False)
        self.view.run_command('dired_refresh')


class dired_toggle_stats(TextCommand, DiredBaseCommand):
    def is_enabled(self):
        return self.view.score_selector(0, "text.dired") > 0

    def run(self, edit):
        settings = self.view.settings()
        current = settings.get('dired_show_stats', False)
        settings.set('dired_show_stats', not current)
        state = 'On' if not current else 'Off'
        sublime.status_message('FileBrowser: Stats {}'.format(state))
        self.view.run_command('dired_refresh')


# EVENT LISTENERS ###################################################


class DiredHijackNewWindow(EventListener):
    def on_window_command(self, window, command_name, args):
        if command_name != "new_window":
            return
        hijack_window()


dired_to_get_closed = {}


class DiredHideEmptyGroup(EventListener):
    def on_pre_close(self, view):
        window = view.window()
        if (
            window
            and 'dired' in view.scope_name(0)
        ):
            group, _ = window.get_view_index(view)
            dired_to_get_closed[view.id()] = (window, group)

            other_group = get_group(window.num_groups(), group)
            for other_view in window.views_in_group(other_group):
                if other_view.settings().get("dired_preview_view"):
                    other_view.close()

    def on_close(self, view):
        window, group = dired_to_get_closed.pop(view.id(), (None, None))
        if (window, group) == (None, None):
            return

        emit_event('view_closed', view.id())
        group_is_empty = not window.views_in_group(group)
        if window.num_groups() == 2 and group_is_empty:
            window.set_layout({"cols": [0.0, 1.0], "rows": [0.0, 1.0], "cells": [[0, 0, 1, 1]]})


class DiredContextProvider(EventListener):
    def on_query_context(self, view, key, operator, operand, match_all):
        if key == "dired_simple_selection":
            if operator not in (sublime.OP_EQUAL, sublime.OP_NOT_EQUAL):
                print(
                    "Context '{key}' only supports operator 'equal' and 'not_equal'."
                    .format(key=key)
                )
                return False

            if operand not in (True, False):
                print(
                    "Context '{key}' only supports operand 'true' and 'false'."
                    .format(key=key)
                )
                return False

            return (op.eq if operator == sublime.OP_EQUAL else op.ne)(
                (
                    (all if match_all else any)(
                        not s.empty()
                        for s in view.sel()
                    )
                ),
                operand
            )

        return None


# TOOLS #############################################################

class dired_call_vcs(TextCommand):
    '''Command allows to call it from other module(s)'''
    def run(self, edit, path):
        self.view.run_command("dired_draw_vcs_marker")
        CallVCS(self.view, path)


class CallVCS(DiredBaseCommand):
    '''Magic'''
    def __init__(self, view, path):
        self.view = view
        self.vcs_state = {'awaiting': 2, 'changed_items': {}}
        for vcs in ['git', 'hg']:
            self.start(vcs, path)

    def start(self, vcs, path):
        '''launch threads'''
        command = self.view.settings().get(f'{vcs}_path', False)
        if command:
            threading.Thread(target=self.check, args=(vcs, command, path)).start()  # fan-out
        else:
            self.done()

    def check(self, vcs, command, path):
        '''target function for a thread; worker'''
        try:
            root, status = self.get_output(vcs, self.expand_command(vcs, command), path)
        except Exception:
            changed_items = {}
        else:
            changed_items = {
                self.parse_status_item(vcs, root, item)
                for item in status
                if item
            }
        finally:
            self.done(changed_items)

    def done(self, changed_items={}):
        sublime.set_timeout(lambda: self.sink_(changed_items))  # fan-in

    def sink_(self, changed_items):
        self.vcs_state['awaiting'] -= 1
        self.vcs_state['changed_items'].update(changed_items)
        if self.vcs_state['awaiting'] == 0:
            self.view.settings().set("vcs_changed_items", self.vcs_state.get('changed_items'))
            self.view.run_command("dired_draw_vcs_marker")

    def expand_command(self, vcs, command):
        '''check if user got wildcards or envvars in custom command'''
        if any(c in command for c in '~*?[]$%') and not isfile(command):
            match = glob.glob(os.path.expandvars(os.path.expanduser(command)))
            if match:
                return match[0]
            else:
                sublime.error_message(
                    'FileBrowser:\n'
                    'It seems like you use wildcards in\n\n"{0}_path": "{1}".\n\n'
                    'But the pattern cannot be found, please, fix it '
                    'or use absolute path without wildcards.'
                    .format(vcs, command)
                )
        return command

    def get_output(self, vcs, executable, path):
        '''call a vsc, getting its output if any'''
        if not executable:
            raise ValueError("executable is required")
        if not path:
            raise RuntimeError("path is required")

        args = {
            'git_status': ['--no-optional-locks', 'status', '--untracked-files=all', '-z'],
            'git_root':   ['rev-parse', '--show-toplevel'],
            'hg_status':  ['status'],
            'hg_root':    ['root'],
        }
        sep = {'hg': '\n', 'git': '\x00'}

        root = subprocess.run(
            [executable] + args[f'{vcs}_root'],
            cwd=path,
            capture_output=True,
            text=True,
            startupinfo=STARTUPINFO
        ).stdout.strip('\n')
        if not root:
            raise ValueError(f"'{path}' is not managed by '{vcs}'.")
        if NT:
            root = root.replace("/", "\\")

        status = subprocess.run(
            [executable] + args[f'{vcs}_status'],
            cwd=path,
            capture_output=True,
            text=True,
            startupinfo=STARTUPINFO
        ).stdout.split(sep[vcs])

        return (root, status)

    def parse_status_item(self, vcs, root, item):
        '''return tuple (fullpath, status)'''
        item = item[1:] if vcs == 'git' else item
        filename = item[2:]
        return (join(root, filename), item[0])


class dired_draw_vcs_marker(TextCommand, DiredBaseCommand):
    def run(self, edit):
        if not self.view.settings().has('dired_index'):
            return  # view was closed
        changed_items = self.view.settings().get("vcs_changed_items")
        if not changed_items:
            self.view.erase_regions('M')
            self.view.erase_regions('?')
            return

        modified, untracked = [], []
        files_regions = {
            f: r
            for f, r in zip(
                self.get_all(),
                self.view.split_by_newlines(Region(0, self.view.size()))
            )
        }
        colorblind = self.view.settings().get('vcs_color_blind', False)
        offset = 1 if not colorblind else 0
        for fn in changed_items.keys():
            full_fn = normpath(fn)
            for p in chain([full_fn], paths_upwards(full_fn)):
                r = files_regions.get(p, 0)
                if r:
                    icon   = self._get_name_point(r) - 2
                    r      = Region(icon, icon + offset)
                    status = changed_items[fn]
                    if status == 'M':
                        modified.append(r)
                    elif status == '?':
                        untracked.append(r)
                    break

        if colorblind:
            self.view.add_regions(
                'M',
                modified,
                'item.colorblind.dired',
                '',
                MARK_OPTIONS | sublime.DRAW_EMPTY_AS_OVERWRITE)
            self.view.add_regions(
                '?',
                untracked,
                'item.colorblind.dired',
                '',
                MARK_OPTIONS | sublime.DRAW_EMPTY)
        else:
            self.view.add_regions('M', modified, 'item.modified.dired', '', MARK_OPTIONS)
            self.view.add_regions('?', untracked, 'item.untracked.dired', '', MARK_OPTIONS)


def paths_upwards(path):
    while True:
        next_path = os.path.dirname(path)
        if next_path == '/' or next_path == path:
            return
        yield next_path + os.sep
        path = next_path
