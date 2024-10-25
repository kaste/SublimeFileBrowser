# coding: utf-8

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

SYNTAX_EXTENSION = '.sublime-syntax'

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


class DiredFindInFilesCommand(TextCommand, DiredBaseCommand):
    def run(self, edit):
        self.index = self.get_all()
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

class DiredHelpCommand(TextCommand):
    def run(self, edit):
        view = self.view.window().new_file()
        view.set_name("Browse: shortcuts")
        view.set_scratch(True)
        view.settings().set('rulers', [])
        view.settings().set('color_scheme', 'Packages/FileBrowser/dired.hidden-tmTheme')
        view.settings().set('syntax', 'Packages/FileBrowser/dired-help' + SYNTAX_EXTENSION)
        view.settings().set('margin', 16)
        view.settings().set('line_numbers', False)
        view.settings().set('gutter', False)
        view.settings().set('fold_buttons', False)
        view.settings().set('draw_indent_guides', False)
        view.settings().set('word_wrap', False)
        view.settings().set('spell_check', False)
        view.settings().set('drag_text', False)
        view.run_command('dired_show_help')
        sublime.active_window().focus_view(view)


class DiredShowHelpCommand(TextCommand):
    def run(self, edit):
        content = sublime.load_resource('Packages/FileBrowser/shortcuts.md')
        if not content:
            window = self.view.window()
            window.status_message("Could not load 'Packages/FileBrowser/shortcuts.md'.")
            return

        normed_content = (
            content
            .replace('\r\n', '\n')
            .replace('\r', '\n')
        )
        self.view.erase(edit, Region(0, self.view.size()))
        self.view.insert(edit, 0, normed_content)
        self.view.sel().clear()
        self.view.set_read_only(True)


# OTHER #############################################################

class DiredToggleProjectFolder(TextCommand, DiredBaseCommand):
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


class DiredOnlyOneProjectFolder(TextCommand, DiredBaseCommand):
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


class DiredQuickLookCommand(TextCommand, DiredBaseCommand):
    """
    quick look current file in mac or open in default app on other OSs
    """
    def run(self, edit, preview=True, files=None):
        self.index = self.get_all()
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


class DiredOpenExternalCommand(TextCommand, DiredBaseCommand):
    """open dir/file in external file explorer"""
    def run(self, edit, fname=None):
        path = self.path
        if not fname:
            self.index = self.get_all()
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


class DiredOpenInNewWindowCommand(TextCommand, DiredBaseCommand):
    def run(self, edit, project_folder=False):
        if project_folder:
            files = project_folder
        else:
            self.index = self.get_all()
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


class DiredToggleAutoRefresh(TextCommand):
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


class DiredPreviewDirectoryCommand(TextCommand, DiredBaseCommand):
    '''Show properties and content of directory in popup'''
    def run(self, edit, fqn=None, point=0):
        if not fqn:
            self.index = self.get_all()
            filenames = self.get_selected(full=True)
            if not filenames:
                return sublime.status_message('Nothing to preview')
            fqn = filenames[0]
            if not (isdir(fqn) or fqn == PARENT_SYM):
                return sublime.status_message('Something wrong')

        self.view.settings().set('dired_stop_preview_thread', False)
        self.preview_thread = threading.Thread(
            target=self.worker, args=(fqn if fqn != PARENT_SYM else self.get_path(),))
        self.preview_thread.start()
        width, height = self.view.viewport_extent()
        self.view.show_popup(
            'Loading...',
            0,
            point or self.view.sel()[0].begin(),
            width,
            height / 2,
            self.open_from_preview
        )

    def worker(self, path):
        self.preview_path = '📁 <a href="dir\v{0}">{0}</a>'.format(path)
        self.subdirs = self.files = self.size = 0
        self.errors = []
        self.open_dirs = []
        self.open_files = []
        self._created, self._accessed, self._modified = get_dates(path)

        def add_err(err):
            self.errors.append(str(err))

        for index, (root, dirs, files) in enumerate(os.walk(path, onerror=add_err)):
            self.subdirs += len(dirs)
            self.files += len(files)

            if not index:
                sort_nicely(dirs)
                sort_nicely(files)
                self.open_dirs = [
                    '📁 <a href="dir\v{0}{1}">{2}</a>'.format(join(root, d), os.sep, d)
                    for d in dirs
                ]
                self.open_files = []

            for f in files:
                fpath = join(root, f)
                if not index:
                    self.open_files.append('≡ <a href="file\v%s">%s</a>' % (fpath, f))
                try:
                    self.size += getsize(fpath)
                except OSError as e:
                    add_err(e)

            if (
                not self.view.is_popup_visible()
                or self.view.settings().get('dired_stop_preview_thread')
            ):
                return
            sublime.set_timeout_async(self.update_preview(), 1)
        sublime.set_timeout_async(self.update_preview(loading=False), 1)

    def update_preview(self, loading=True):
        le = len(self.errors)
        if le > 5:
            if loading:
                errors = '<br>%d errors<br><br>' % le
            else:
                errors = '<br><a href="errors\v">%s errors</a> (click to view)<br><br>' % le
        else:
            errors = (
                '<br>Errors:<br> {0}<br><br>'
                .format('<br> '.join(self.errors) if self.errors else '<br>')
            )
        items = self.open_dirs + self.open_files
        self.view.update_popup(
            '<br>{0}{1}<br><br>'
            'Files: {2}; directories: {3}<br>'
            'Size: {4} ({5} bytes)<br><br>'
            'Created:  {6}<br>'
            'Accessed: {7}<br>'
            'Modified: {8}<br>{9}{10}'.format(
                'Loading... ' if loading else '', self.preview_path,
                self.files, self.subdirs,
                convert_size(self.size), self.size,
                self._created, self._accessed, self._modified,
                errors,
                ' %s<br><br>' % '<br> '.join(items) if items else '')
        )

    def open_from_preview(self, payload):
        msg, path = payload.split('\v')

        def show_errors(_):
            self.view.update_popup(
                '<br><a href="back\v">←<br><br>'
                '</a>Errors:<br> %s<br>' % '<br> '.join(self.errors))

        def go_back(_):
            self.update_preview(loading=False)

        def open_dir(path):
            self.view.settings().set('dired_path', path)
            self.view.run_command('dired_refresh')

        def open_file(path):
            (self.view.window() or sublime.active_window()).open_file(path)

        case = {
            'dir': open_dir,
            'file': open_file,
            'errors': show_errors,
            'back': go_back
        }
        case[msg](path)


class DiredFilePropertiesCommand(TextCommand, DiredBaseCommand):
    '''Show properties of file in popup'''
    def run(self, edit, fqn=None, point=0):
        if not fqn:
            self.index = self.get_all()
            filenames = self.get_selected(full=True)
            if not filenames:
                return sublime.status_message('Nothing to preview')

        width, height = self.view.viewport_extent()
        self.view.show_popup(
            'Loading...',
            0,
            point or self.view.sel()[0].begin(),
            width,
            height / 2,
            self.open_from_preview
        )
        self.get_info(fqn)

    def get_info(self, path):
        self.preview_path = path
        self.parent = dirname(path)
        self.size = 0
        self.errors = []
        self._created, self._accessed, self._modified = get_dates(path)
        try:
            self.size += getsize(path)
        except OSError as e:
            self.errors.append(str(e))
        if not self.view.is_popup_visible():
            return
        sublime.set_timeout_async(self.update_preview, 1)

    def update_preview(self):
        self.view.update_popup(
            '<br>≡ <a href="file\v{0}">{0}</a><br><br>'
            'Size: {1} ({2} bytes)<br><br>'
            'Created:  {3}<br>'
            'Accessed: {4}<br>'
            'Modified: {5}<br>'
            '{6}'
            '{7}'
            '<a href="app\v{0}">Open in default app</a><br>'
            '<a href="external\v{0}">Open parent in Finder/Explorer</a><br><br>'.format(
                self.preview_path,
                convert_size(self.size), self.size,
                self._created, self._accessed, self._modified,
                '<br>Errors:<br> %s<br><br>' % '<br> '.join(self.errors) if self.errors else '<br>',
                ('<a href="ql\v%s">Open in Quick Look</a><br>' % self.preview_path) if OSX else '')
        )

    def open_from_preview(self, payload):
        msg, path = payload.split('\v')

        def open_file(path):
            (self.view.window() or sublime.active_window()).open_file(path)

        def app(path):
            self.view.update_popup('Please, wait…')
            sublime.set_timeout_async(
                self.view.run_command('dired_quick_look', {'preview': False, 'files': [path]}), 1)

        def external(path):
            self.view.update_popup('Please, wait…')
            sublime.set_timeout_async(
                self.view.run_command('dired_open_external', {'fname': path}), 1)

        def ql(path):
            self.view.update_popup('Please, wait…')
            sublime.set_timeout_async(
                self.view.run_command('dired_quick_look', {'files': [path]}), 1)

        case = {
            'file': open_file,
            'app': app,
            'external': external,
            'ql': ql
        }
        case[msg](path)


# EVENT LISTENERS ###################################################

class DiredHoverProperties(sublime_plugin.ViewEventListener, DiredBaseCommand):
    @classmethod
    def is_applicable(cls, settings):
        return settings.get('syntax') == 'Packages/FileBrowser/dired.sublime-syntax'

    def on_hover(self, point, hover_zone):
        self.view.hide_popup()
        self.view.settings().set('dired_stop_preview_thread', True)
        if hover_zone != sublime.HOVER_GUTTER:
            return
        self.index = self.get_all()
        line = self.view.line(point)
        path = self.get_fullpath_for(line)
        self.name_point = self._get_name_point(line)
        if 'file' in self.view.scope_name(line.a):
            self.view.run_command('dired_file_properties', {'fqn': path, 'point': self.name_point})
        else:
            width, height = self.view.viewport_extent()
            self.view.show_popup(
                '<a href="{0}">Click here to preview directory<br> {0}</a>' .format(path),
                0,
                point or self.view.sel()[0].begin(),
                width,
                height / 2,
                self.open_from_preview
            )

    def open_from_preview(self, path):
        self.view.run_command('dired_preview_directory', {'fqn': path, 'point': self.name_point})


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


def is_any_dired_in_group(window, group):
    syntax = 'Packages/FileBrowser/dired%s' % SYNTAX_EXTENSION
    return any(v.settings().get('syntax') == syntax for v in window.views_in_group(group))


class DiredMoveOpenOrNewFileToRightGroup(EventListener):
    def on_activated(self, view):
        '''
        Trick to prevent unexpected movements (e.g. when switching project in
        current window; or restart)
        Reason why the whole logic shall not be run on_activated, is
        user should be able to explicitly put any view in left group
        no matter what, e.g. using keybinding or drag&drop

        self.MOVE is boolean
        '''
        w = sublime.active_window()
        self.MOVE = w and is_any_dired_in_group(w, 0)

    def on_new(self, view):
        if not self.MOVE:
            return
        w = sublime.active_window()
        if w.num_groups() < 2:
            return
        if is_any_dired_in_group(w, 0):
            if w.active_group() == 0:
                # at this point views are exist, so we cannot avoid the use of
                # set_view_index, but ST2 return None if group has no views
                # ST3 return None if group has active image’s view
                avig1 = w.active_view_in_group(1)
                if avig1:
                    _group, active_view_index_in_other_group = w.get_view_index(avig1)
                    index = active_view_index_in_other_group + 1
                else:
                    index = 0
                sublime.set_timeout(lambda: w.set_view_index(view, 1, index), 1)

    def on_load(self, view):
        self.on_new(view)


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
                        not s.empty() and len(view.lines(s)) == 1
                        for s in view.sel()
                    )
                ),
                operand
            )

        return None


# TOOLS #############################################################

class DiredCallVcs(TextCommand):
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


class DiredDrawVcsMarkerCommand(TextCommand, DiredBaseCommand):
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
