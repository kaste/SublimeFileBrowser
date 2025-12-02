'''Main module; launch and navigation related stuff'''
from __future__ import annotations
from collections import defaultdict
import os
from os.path import basename, dirname, isdir, exists, join
import sys
from textwrap import dedent
from datetime import datetime
import sublime
from sublime import Region
from sublime_plugin import EventListener, WindowCommand, TextCommand

from .common import (
    DiredBaseCommand, ListingItem, calc_width, display_path, emit_event, get_group, hijack_window,
    set_proper_scheme, NT, PARENT_SYM, rx_fuzzy_match)
from . import prompt
from .show import show, create_dired_view, retarget_dired_view
from .jumping import jump_names


ICON    = '𝌆'
SEPARATOR = '\u200b'
STATS_PADDING = 2


def reuse_view():
    return sublime.load_settings('dired.sublime-settings').get('dired_reuse_view', False)


def plugin_loaded():
    if len(sublime.windows()) == 1 and len(sublime.windows()[0].views()) == 0:
        hijack_window()

    for w in sublime.windows():
        for v in w.views():
            if v.settings() and v.settings().get("dired_path"):
                v.run_command("dired_refresh")

    dfsobserver = 'FileBrowser.0_dired_fs_observer'
    if dfsobserver not in sys.modules or sys.modules[dfsobserver].Observer is None:
        print(dedent('''
            FileBrowser: watchdog module is not importable, hence auto-refresh will not work.
            You can still manually refresh a dired view with `[r]`.
             • If you installed via Package Control, make sure to restart Sublime Text often enough
               to give it a chance to install the required dependencies.
             • If you installed manually, then refer the Readme on how to install dependencies as well.
        '''))  # noqa: E501

    settings = sublime.load_settings('dired.sublime-settings')
    settings.add_on_change(
        'dired_autorefresh',
        lambda: emit_event('toggle_watch_all', settings.get('dired_autorefresh', None))
    )


def plugin_unloaded():
    sublime.load_settings('dired.sublime-settings').clear_on_change('dired_autorefresh')


class dired(WindowCommand):
    """
    Open a dired view.  This is the main entrypoint/constructor.

    If `immediate` is set: open a dired view immediately. Usually highlight the
    file of the current view, fallback to an open folder or the users home
    directory as a last resort.  If `project` is also set: set the root
    directory to the best matching open folder.  Otherwise the root will be
    the directory of the view's file.  (In other words, you get either a flat,
    if `project: false`, or nested listing, if it is `true`.)

    If `immediate` is *not* set: open an input box to let the user type in the
    directory they want.  The input box implements a completion helper using
    `<tab>` to make this easier.  The input box is typically filled with the
    name of the directory of the active view's file, or a fallback.  If you
    set `project`, it is instead pre-filled with the folder attached to the
    window.  In case there are multiple folders open, the user gets prompted
    by a quick panel to choose a folder from.  In case there are *no* folders,
    show some fallbacks.
    """
    def run(self, immediate=False, project=False, single_pane=False, other_group=False):
        if immediate:
            fpath = self._get_current_fpath()
            if not fpath:
                path, goto = self._fallback_path(), ''

            elif project:
                path = self._best_root_for_fpath(fpath)
                if path:
                    goto = os.path.relpath(fpath, path)
                else:
                    path, goto = os.path.split(fpath)

            else:
                path, goto = os.path.split(fpath)

            view = show(self.window, path, goto=goto, single_pane=single_pane, other_group=other_group)
            if view:
                view.settings().set('dired_sidebar_mode', bool(other_group))
            return

        if project:
            folders = self.window.folders()
            if len(folders) == 1:
                path = folders[0]
            else:
                self._show_folders_panel(folders, single_pane, other_group)
                return
        else:
            fpath = self._get_current_fpath()
            path = os.path.dirname(fpath) if fpath else self._fallback_path()

        prompt.start('Directory:', self.window, path, self._show, single_pane, other_group)

    def _get_current_fpath(self):
        view = self.window.active_view()
        return view.file_name() if view else None

    def _show_folders_panel(self, folders, single_pane, other_group):
        if not folders:
            fpath = self._get_current_fpath()
            if fpath:
                folders += [os.path.dirname(fpath)]
            folders += [os.path.expanduser('~')]

        names = [basename(f) for f in folders]
        longest_name = max([len(n) for n in names])
        for i, f in enumerate(folders):
            name     = names[i]
            offset   = ' ' * (longest_name - len(name) + 1)
            names[i] = '%s%s%s' % (name, offset, display_path(f))

        self.window.show_quick_panel(
            names,
            lambda i: self._show_folder(i, folders, single_pane, other_group),
            sublime.MONOSPACE_FONT
        )

    def _fallback_path(self) -> str:
        if view := self.window.active_view():
            if repo_path := view.settings().get("git_savvy.repo_path"):
                return repo_path

        if folders := self.window.folders():
            return folders[0]
        return os.path.expanduser('~')

    def _best_root_for_fpath(self, fpath):
        folders = self.window.folders()
        for f in folders:
            # e.g. ['/a', '/aa'], to open '/aa/f' we need '/aa/'
            if fpath.startswith(''.join([f, os.sep])):
                return f

    def _show_folder(self, index, folders, single_pane, other_group):
        if index != -1:
            path = folders[index]
            view = show(self.window, path, single_pane=single_pane, other_group=other_group)
            if view:
                view.settings().set('dired_sidebar_mode', bool(other_group))

    def _show(self, path, single_pane, other_group):
        view = show(self.window, path, single_pane=single_pane, other_group=other_group)
        if view:
            view.settings().set('dired_sidebar_mode', bool(other_group))


class dired_refresh(TextCommand, DiredBaseCommand):
    """
    Populates or repopulates a dired view.

    self.index is a representation of view lines
               list contains full path of each item in a view, except
               header ['', ''] and parent_dir [PARENT_SYM]
    self.index shall be updated according to view modifications (refresh,
    expand single directory, fold) and stored in view settings as 'dired_index'

    The main reason for index is access speed to item path because we can
        self.index[self.view.rowcol(region.a)[0]]
    to get full path, instead of grinding with substr thru entire view
    substr is slow: https://github.com/SublimeTextIssues/Core/issues/882
    """
    sels: tuple[list[str] | None, list[Region] | None] | None

    def run(self, edit, goto='', to_expand=None, toggle=None, reset_sels=None, auto_expand=0):
        """
        goto
            Optional filename to put the cursor on; used only from "dired_up"

        to_expand
            List of absolute directory paths (with trailing os.sep) that shall
            be expanded.

        toggle
            If true, marked/selected directories shall switch state,
            i.e. expand/collapse

        reset_sels
            If True, previous selections & marks shan’t be restored
        """
        # after restart ST, callback seems to disappear, so reset callback on each refresh
        # for more reliability
        self.view.settings().clear_on_change('color_scheme')
        self.view.settings().add_on_change('color_scheme', lambda: set_proper_scheme(self.view))

        path = self.path
        if path == 'ThisPC\\':
            path = ''
        if path and not exists(path):
            if sublime.ok_cancel_dialog(
                (
                    'FileBrowser:\n\n'
                    'Directory does not exist:\n\n'
                    '\t{0}\n\nTry to go up?'.format(path)
                ),
                'Go'
            ):
                self.view.run_command('dired_up')
            return

        # Track expanded dirs via stored setting to survive filtering cycles
        stored_expanded = self.view.settings().get('dired_expanded_paths') or []
        # Normalize to list of strings
        if not isinstance(stored_expanded, list):
            stored_expanded = []
        self.expanded = expanded = list(dict.fromkeys(stored_expanded)) if not reset_sels else []
        self.show_hidden = self.view.settings().get('dired_show_hidden_files', True)
        self.goto = goto
        if os.sep in goto:
            to_expand = self.expand_goto(to_expand)

        if not reset_sels:
            self.index = self.get_all()
            self.sels = (self.get_selected(), list(self.view.sel()))
        else:
            # When resetting, clear selections and initialize index
            self.index = []
            self.sels = None
        self.populate_view(edit, path, expanded, to_expand, toggle, auto_expand)

        if self.view.settings().get('dired_autorefresh', True):
            emit_event(
                'set_paths',
                (self.view.id(), self.expanded + ([path] if path else []))
            )
        else:
            emit_event('stop_watch', self.view.id())

    def expand_goto(self, to_expand):
        '''e.g. self.goto = "a/b/c/d/", then to put cursor onto d, it should be
        to_expand = ["a/", "a/b/", "a/b/c/"] (items order in list dont matter)
        '''
        to_expand = to_expand or []
        goto = self.goto
        while len(goto.split(os.sep)) > 2:
            parent = dirname(goto) + os.sep
            to_expand.append(parent)
            goto = parent.rstrip(os.sep)
        return to_expand

    def populate_view(self, edit, path, expanded, to_expand, toggle, auto_expand=0):
        '''Called when we know that some directories were (or/and need to be) expanded'''
        root = path
        # `expanded` already contains absolute paths collected from settings
        if toggle and to_expand:
            merged = list(set(expanded + to_expand))
            expanded = [e for e in merged if not (e in expanded and e in to_expand)]
        else:
            expanded.extend(to_expand or [])
        self.expanded = expanded

        # Phase 1: build the tree
        entries, err = self.walkdir(root, expanded=set(expanded), auto_expand=auto_expand)
        if err:
            self.view.run_command("dired_up")
            self.view.set_read_only(False)
            self.view.insert(
                edit,
                self.view.line(self.view.sel()[0]).b,
                '\t<%s>' % err
            )
            self.view.set_read_only(True)
            return

        # Phase 2: filter bottom-up
        s = self.view.settings()
        enabled = s.get('dired_filter_enabled', True)
        ext = s.get('dired_filter_extension', '').lower()
        flt = s.get('dired_filter', '').strip()

        filtered_entries: list[ListingItem] = []
        for e in reversed(entries):
            ok_name = not enabled or not flt or rx_fuzzy_match(flt, e.name)
            if e.is_dir:
                children_present = (
                    filtered_entries and filtered_entries[-1].full_path.startswith(e.full_path))
                ok_ext = not enabled or not ext
                keep_dir = ok_ext and ok_name or children_present
                if keep_dir:
                    filtered_entries.append(e)
            else:
                ok_ext = not enabled or not ext or os.path.splitext(e.name)[1].lower() == ext
                keep_file = ok_ext and ok_name
                if keep_file:
                    filtered_entries.append(e)

        # Phase 3: render and build index
        show_stats = s.get('dired_show_stats', False)
        tab_setting = self.view.settings().get('tab_size')
        try:
            tab_size = int(tab_setting) if tab_setting else 4
        except (TypeError, ValueError):
            tab_size = 4

        rendered, new_index = format_items(reversed(filtered_entries), show_stats, tab_size)
        max_name_width = max((display_len for _, display_len, _, _ in rendered), default=0)
        max_size_width = max((len(size_part) for _, _, size_part, _ in rendered), default=0)
        lines = render_items(rendered, max_name_width, max_size_width, show_stats)

        if show_stats:
            s.set('dired_name_col_width', max_name_width)
            s.set('dired_size_col_width', max_size_width)
        else:
            s.erase('dired_name_col_width')
            s.erase('dired_size_col_width')

        self.set_status()
        self.index = new_index
        items = self.correcting_index(path, lines)
        self.write(edit, items)
        self.restore_selections(path)
        self.refresh_mark_highlights()
        self.refresh_clipboard_highlights()
        # Persist expanded paths for future refreshes
        self.view.settings().set('dired_expanded_paths', self.expanded)
        self.view.run_command('dired_call_vcs', {'path': path})

    def set_title(self, path):
        '''Update name of tab and return tuple of two elements
            text    list of two unicode obj (will be inserted before filenames
                    in view) or empty list
            header  boolean, value of dired_header setting
            '''
        header  = self.view.settings().get('dired_header', False)
        name    = jump_names().get(path or self.path)
        caption_base = "{0} → {1}".format(name, path) if name else path or self.path
        extras = ''
        # If filters are active, show them in the header, e.g. [*.py] [foo]
        if header:
            s = self.view.settings()
            if s.get('dired_filter_enabled', True):
                ext = s.get('dired_filter_extension') or ''
                if ext:
                    extras += "  [*{0}]".format(ext)
                flt = s.get('dired_filter') or ''
                if flt:
                    extras += "  [{0}]".format(flt)
        caption_full = caption_base + extras
        if header:
            text = [caption_full, '—' * len(caption_full)]
        else:
            text = []
        if not path:
            title = '%s %s' % (ICON, name or 'This PC')
        else:
            norm_path = path.rstrip(os.sep)
            if self.view.settings().get('dired_show_full_path', False):
                title = '%s %s (%s)' % (ICON, name or basename(norm_path), norm_path)
            else:
                title = '%s %s' % (ICON, name or basename(norm_path))
        self.view.set_name(title)
        return (text, header)

    def write(self, edit, fileslist):
        '''apply changes to view'''
        self.view.set_read_only(False)
        self.view.replace(edit, Region(0, self.view.size()), '\n'.join(fileslist))
        self.view.set_read_only(True)

        fileregion = self.fileregion()
        count = len(self.view.lines(fileregion)) if fileregion else 0
        self.view.settings().set('dired_count', count)
        self.view.settings().set('dired_index', self.index)
        # Update filter highlights (if any)
        self.update_filter_highlight()

    def correcting_index(self, path, fileslist):
        '''Add leading elements to self.index (if any), we need conformity of
        elements in self.index and line numbers in view
        Return list of unicode objects that ready to be inserted in view
        '''
        text, header = self.set_title(path)
        if path and (not fileslist or self.show_parent()):
            text.append(PARENT_SYM)
            self.index = [PARENT_SYM] + self.index
        if header:
            self.index = ['', ''] + self.index
        return text + fileslist

    def restore_selections(self, path):
        '''Set cursor(s)'''
        if self.goto:
            if self.goto[~0] != os.sep:
                self.goto += (os.sep if isdir(join(path, self.goto)) else '')
            self.sels = ([self.goto.replace(path, '', 1)], None)
        self.restore_sels(self.sels)


# NAVIGATION #####################################################

class dired_delegate_call(TextCommand):
    def run(self, edit, command, args=None):
        if target_id := self.view.settings().get('dired_target_view_id'):
            target = sublime.View(target_id)
        else:
            window = self.view.window()
            if not window:
                return
            target = window.active_view()  # type: ignore[assignment]
            if not target:
                return
        target.run_command(command, args or {})


class dired_next_line(TextCommand, DiredBaseCommand):
    def run(self, edit, forward=None):
        self.move(forward)


class dired_move(TextCommand, DiredBaseCommand):
    def run(self, edit, to="bof"):
        self.move_to_extreme(to)


class dired_select(TextCommand, DiredBaseCommand):
    '''Common command for opening file/directory in existing view'''
    def run(self, edit, new_view=False, other_group=None, and_close=0):
        '''
        new_view     if True, open directory in new view, rather than existing one
        other_group  if True, create a new group (if need) and open file in this group
        and_close    if True, close FileBrowser view after file was open
        '''
        if other_group is None:
            other_group = bool(self.view.settings().get('dired_sidebar_mode', False))

        self.index = self.get_all()
        filenames = (self.get_selected(full=True) if not new_view else
                     self.get_marked(full=True) or self.get_selected(full=True))

        window = self.view.window()
        assert window
        if self.goto_directory(filenames, window, new_view):
            return

        dired_view = self.view
        if other_group:
            self.focus_other_group(window)
        else:
            groups = window.num_groups()
            if groups > 1:
                dired_group = window.active_group()
                group = get_group(groups, dired_group)
                for other_view in window.views_in_group(group):
                    if other_view.settings().get("dired_preview_view"):
                        other_view.close()
                window.focus_view(dired_view)

        last_created_view = None
        for fqn in filenames or []:
            last_created_view = self.open_item(fqn, window, new_view)

        if and_close:
            window.focus_view(dired_view)
            window.run_command("close")
            if last_created_view:
                window.focus_view(last_created_view)

    def goto_directory(self, filenames, window: sublime.Window, new_view: bool):
        '''If reuse view is turned on and the only item is a directory, refresh the existing view'''
        if new_view and reuse_view():
            return False
        fqn = filenames[0]
        if len(filenames) == 1 and isdir(fqn):
            s = self.view.settings()
            # Clear any persisted marks when navigating into a directory
            s.set('dired_marked_paths', [])
            self.refresh_mark_highlights()
            # Disable active filter when traversing into a subdirectory
            s.set('dired_filter_enabled', False)
            # Prepare history: replace current, cut forward, append destination on refresh
            self.history_push()
            remembered_child = s.get('dired_last_selection_by_dir', {}).get(fqn, '')
            show(window, fqn, reuse_view=self.view, goto=remembered_child)
            return True
        elif fqn == PARENT_SYM:
            window.run_command("dired_up")
            return True
        return False

    def open_item(self, fqn, window, new_view) -> sublime.View | None:
        if isdir(fqn):
            show(window, fqn, ignore_existing=new_view)
        elif exists(fqn):  # ignore 'item <error>'
            return window.open_file(fqn, sublime.FORCE_GROUP, group=-1)
        else:
            sublime.status_message(
                'File does not exist ({0})'.format((basename(fqn.rstrip(os.sep)) or fqn))
            )
        return None

    def focus_other_group(self, window):
        '''call it when preview open in other group'''
        target_group = self._other_group(window, window.active_group())
        # set_view_index and focus are not very reliable
        # just focus target_group should do what we want
        window.focus_group(target_group)

    def _other_group(self, w, nag):
        '''
        creates new group if need and return index of the group where files
        shall be opened
        '''
        groups = w.num_groups()
        if groups == 1:
            width = calc_width(self.view)
            w.set_layout({
                "cols": [0.0, width, 1.0],
                "rows": [0.0, 1.0],
                "cells": [[0, 0, 1, 1], [1, 0, 2, 1]]
            })
        group = get_group(groups, nag)
        return group


class dired_preview(dired_select):
    '''Open file as a preview, so focus remains in FileBrowser view'''
    def run(self, edit):
        self.index = self.get_all()
        filenames = self.get_selected(full=True)

        if not filenames:
            return sublime.status_message('Nothing to preview')

        fqn = filenames[0]

        if isdir(fqn) or fqn == PARENT_SYM:
            # Preview directories by opening a dired view for the selected
            # folder in the "other" group, toggling it on repeated use.
            window = self.view.window()
            if not window:
                return

            # Resolve target directory path
            target_path = fqn if fqn != PARENT_SYM else self.get_path()
            if not target_path:
                return

            dired_view = self.view
            will_create_preview_group = window.num_groups() == 1
            group = self._other_group(window, window.active_group())

            open_views = window.views_in_group(group)
            # Find an existing preview container in the other group (file or dired).
            preview_view = next(
                (v for v in open_views if v.settings().get("dired_preview_container")),
                None
            )

            # Toggle behavior: if the preview already shows this folder,
            # close that view instead of opening another one.
            if (
                preview_view
                and preview_view.settings().get('dired_path') == target_path
                and preview_view.score_selector(0, "text.dired") > 0
            ):
                window.focus_view(preview_view)
                preview_view.close()
                sublime.set_timeout(lambda: window.focus_view(dired_view))
                return

            # Otherwise, reuse the existing preview view if possible, or create one.
            close_empty_preview_group = (
                will_create_preview_group
                or (
                    open_views and all(
                        v.settings().get("dired_preview_view")
                        for v in open_views
                    )
                )
            )

            if preview_view is None:
                preview_view = create_dired_view(window)

            # Place the view in the target group
            window.set_view_index(preview_view, group, 0)

            # Mark as a preview view so the handler can manage empty panes
            preview_view.settings().set("dired_preview_view", True)
            preview_view.settings().set("dired_preview_container", True)
            preview_view.settings().set(
                "dired_close_empty_preview_pane", close_empty_preview_group)

            # Initialize/reuse as dired view rooted at target_path
            retarget_dired_view(preview_view, target_path)

            when_loaded(preview_view, lambda: window.focus_view(dired_view))
            return

        if exists(fqn):
            window = self.view.window()
            assert window
            dired_view = self.view
            will_create_preview_group = window.num_groups() == 1
            group = self._other_group(window, window.active_group())
            other_view = window.active_view_in_group(group)
            if (
                other_view
                and other_view.file_name() == fqn
                and other_view.settings().get("dired_preview_view")
            ):
                # We need to focus even when we close, otherwise Sublime
                # magically opens an empty/new view.  `set_timeout` for the
                # same reason.  TODO: Investigate in safe-mode.
                window.focus_view(other_view)
                other_view.close()
                sublime.set_timeout(lambda: window.focus_view(dired_view))
            else:
                open_views = window.views_in_group(group)
                close_empty_preview_group = (
                    will_create_preview_group
                    or (
                        open_views and all(
                            other_view.settings().get("dired_preview_view")
                            for other_view in open_views
                        )
                    )
                )
                other_view = window.open_file(fqn, sublime.FORCE_GROUP, group=group)
                other_view.settings().set("dired_preview_view", True)
                other_view.settings().set(
                    "dired_close_empty_preview_pane", close_empty_preview_group)
                for v in open_views:
                    if v != other_view and v.settings().get("dired_preview_view"):
                        v.close()
                when_loaded(other_view, lambda: window.focus_view(dired_view))
        else:
            sublime.status_message(
                'File does not exist ({0})'.format(basename(fqn.rstrip(os.sep)) or fqn))


views_yet_to_get_loaded = defaultdict(list)
preview_to_get_closed = {}


def when_loaded(view, handler):
    if view.is_loading():
        views_yet_to_get_loaded[view.id()].append(handler)
    else:
        handler()


class PreviewViewHandler(EventListener):
    def on_load(self, view):
        callbacks = views_yet_to_get_loaded.pop(view.id(), [])
        for fn in callbacks:
            sublime.set_timeout(fn)

    def on_modified_async(self, view):
        if view.settings().get("dired_preview_view"):
            view.settings().erase("dired_preview_view")

    def on_pre_close(self, view):
        window = view.window()
        if (
            window
            and view.settings().get("dired_close_empty_preview_pane")
        ):
            group, _ = window.get_view_index(view)
            preview_to_get_closed[view.id()] = (window, group)

    def on_close(self, view):
        window, group = preview_to_get_closed.pop(view.id(), (None, None))
        if (window, group) == (None, None):
            return

        if window.num_groups() == 2 and len(window.views_in_group(group)) == 0:
            window.set_layout({"cols": [0.0, 1.0], "rows": [0.0, 1.0], "cells": [[0, 0, 1, 1]]})


class dired_expand(TextCommand, DiredBaseCommand):
    '''Open directory(s) inline, aka treeview'''
    def run(self, edit, toggle=False):
        '''
        toggle  if True, state of directory(s) will be toggled (i.e. expand/collapse)
        '''
        self.index = self.get_all()
        # Marks should not influence expand; only actual selections count
        selected = self.get_selected(parent=False, full=True) or []

        paths = [p for p in selected if p.endswith(os.sep)]

        if len(paths) == 1:
            return self.expand_single_directory(edit, paths[0], toggle)
        elif paths:
            # working with several selections at once is very tricky, thus for reliability we should
            # recreate the entire tree, despite it is supposedly slower, but not really, because
            # one view.replace/insert() call is faster than multiple ones
            self.view.run_command('dired_refresh', {'to_expand': paths, 'toggle': toggle})
            return
        else:
            return sublime.status_message('Item cannot be expanded')

    def expand_single_directory(self, edit, path, toggle):
        '''Expand one directory is safe and fast, thus we do it here,
        but for many directories call refresh command'''
        # Capture current marks and selection (by path) for later restoration
        marked_paths = self.view.settings().get('dired_marked_paths') or []
        current_sel = list(self.view.sel())[0]

        # Use the index to find the exact line for `path`
        self.index = self.get_all()
        row_by_path = {p: i for i, p in enumerate(self.index) if p}
        row = row_by_path.get(path)
        if row is not None:
            pt = self.view.text_point(row, 0)
            line = self.view.line(pt)
        else:
            line = self.view.line(current_sel)
        self.sel = Region(line.a, line.a)
        line_content = self.view.substr(line)
        if line_content.lstrip().startswith('▾'):
            if toggle:
                self.view.run_command('dired_fold')
            return

        settings = self.view.settings()
        self.show_hidden = settings.get('dired_show_hidden_files', True)
        show_stats = settings.get('dired_show_stats', False)
        name_width_setting = settings.get('dired_name_col_width', 0)
        size_width_setting = settings.get('dired_size_col_width', 0)
        tab_setting = self.view.settings().get('tab_size')
        try:
            tab_size = int(tab_setting) if tab_setting else 4
        except (TypeError, ValueError):
            tab_size = 4
        indentation_level = self.view.indentation_level(line.a)

        entries, err = self.walkdir(path, depth=indentation_level + 1)
        _base_timestamp = None
        if SEPARATOR in line_content:
            _base_timestamp = line_content.split(SEPARATOR)[1].strip().split(' ', 1)[1].strip()
        root_entry = ListingItem(
            basename(path.rstrip(os.sep)),
            path,
            True,
            9999,  # size not shown for dirs
            _base_timestamp,
            indentation_level,
            expanded=True,
            note=err or ('empty' if not entries else '')
        )
        entries = self._filter_entries(entries, settings)
        entries = [root_entry] + entries
        rendered, new_index = format_items(entries, show_stats, tab_size)
        max_name_width = max((display_len for _, display_len, _, _ in rendered), default=0)
        max_size_width = max((len(size_part) for _, _, size_part, _ in rendered), default=0)
        if show_stats and (
            max_name_width > name_width_setting or max_size_width > size_width_setting
        ):
            expanded = set(settings.get('dired_expanded_paths') or [])
            expanded.add(path)
            settings.set('dired_expanded_paths', list(expanded))
            settings.set('dired_name_col_width', max(max_name_width, name_width_setting))
            settings.set('dired_size_col_width', max(max_size_width, size_width_setting))
            self.view.run_command('dired_refresh')
            return

        replacement = render_items(rendered, name_width_setting, size_width_setting, show_stats)

        # Compute insert point for splicing children into the index
        insert_at = 1 + self.view.rowcol(line.a)[0]
        # Note that we exclude the root/path at the front of new_index ("[1:]").
        self.index = self.index[:insert_at] + new_index[1:] + self.index[insert_at:]
        self.view.settings().set('dired_index', self.index)

        dired_count = self.view.settings().get('dired_count', 0)
        # - 1 for the root/path which was already in the index before
        self.view.settings().set('dired_count', dired_count + len(new_index) - 1)

        self.view.set_read_only(False)
        self.view.replace(edit, line, '\n'.join(replacement))
        self.view.set_read_only(True)

        # Persist expanded paths
        expanded = set(self.view.settings().get('dired_expanded_paths') or [])
        expanded.add(path)
        self.view.settings().set('dired_expanded_paths', list(expanded))

        # Re-expand subfolders previously expanded under this parent
        saved_sub_expansions = self.view.settings().get('dired_saved_sub_expansions') or {}
        sub_expanded = saved_sub_expansions.get(path) or []
        if sub_expanded:
            for sub in sorted(sub_expanded, key=lambda p: p.count(os.sep)):
                self.expand_single_directory(edit, sub, toggle=False)
            saved_sub_expansions.pop(path, None)
            self.view.settings().set('dired_saved_sub_expansions', saved_sub_expansions)

        # Restore marks
        self.view.settings().set('dired_marked_paths', marked_paths)
        self.refresh_mark_highlights()

        # Try to focus last selected child under this parent
        last_child = (self.view.settings().get('dired_last_child_by_parent') or {}).get(path)
        if last_child:
            child_rel = last_child.replace(self.path, '', 1)
            self.restore_sels(([child_rel], None))
        else:
            # Fallback to focusing the parent directory line
            self.restore_sels(([path.replace(self.path, '', 1)], [self.sel]))

        self.view.run_command("dired_draw_vcs_marker")
        self.update_filter_highlight()
        self.refresh_clipboard_highlights()
        if self.view.settings().get('dired_autorefresh', True):
            emit_event('add_paths', (self.view.id(), [path]))
        else:
            emit_event('stop_watch', self.view.id())


class dired_fold(TextCommand, DiredBaseCommand):
    '''
    This command used to fold/erase/shrink (whatever you like to call it) content
    of some [sub]directory (within current directory, see self.path).
    There are two cases when this command would be fired:
        1. User mean to collapse (key ←)
        2. User mean to expand   (key →)
    In first case we just erase region, however, we need to figure out which region to erase:
        (a) if cursor placed on directory item and next line(s) indented (representing content of
            the directory) — erase indented line(s);
        (b) next line is not indented, but the line of directory item is indented — erase directory
            item itself and all neighbours with the same indent;
        (c) cursor placed on file item which is indented — same as prev. (erase item and neighbours)
    In second case we need to decide if erasing needed or not:
        (a) if directory was expanded — do erase (as in 1.a), so then it’ll be filled again,
            basically it is like update/refresh;
        (b) directory was collapsed — do nothing.

    Very important, in case of actual modification of view, set valid dired_index setting
                    see DiredRefreshCommand docs for details
    '''
    sels: tuple[list[str] | None, list[Region] | None] | None

    def run(self, edit, update=None, index=None):
        '''
        update
            True when user mean to expand, i.e. no folding for collapsed directory even if indented
        index
            list returned by self.get_all(), kinda cache during DiredExpand.expand_single_directory

        Call self.fold method on each line (multiple selections/marks), restore marks and selections
        '''
        v = self.view
        self.update = update
        self.index  = index or self.get_all()
        # Marks should not influence collapse; only actual selections count
        sels = list(v.sel())

        last_child_by_parent = self.view.settings().get('dired_last_child_by_parent') or {}
        saved_sub_expansions = self.view.settings().get('dired_saved_sub_expansions') or {}
        expanded_paths = self.view.settings().get('dired_expanded_paths') or []

        base_path = self.get_path()
        parents = []
        lines = [v.line(s.a) for s in reversed(sels)]
        for line in lines:
            # Child path under the cursor before we modify the view
            child_full = self.get_fullpath_for(line)
            parent_line = self.fold(edit, line)
            if parent_line is not None:
                parent_rel = self.get_parent(parent_line, base_path)
                parents.append((parent_rel, Region(parent_line.a)))
                parent_full = base_path + parent_rel
                last_child_by_parent[parent_full] = child_full

                # Save which subfolders were expanded under this parent
                saved_sub_expansions[parent_full] = [
                    p for p in expanded_paths
                    if p.startswith(parent_full) and p != parent_full
                ]

        # If nothing was folded, keep selection and inform the user
        if not parents:
            return sublime.status_message('Nothing to collapse')

        # Persist mappings
        self.view.settings().set('dired_last_child_by_parent', last_child_by_parent)
        self.view.settings().set('dired_saved_sub_expansions', saved_sub_expansions)

        # Focus the parent directory
        names, line_starts = map(list, zip(*parents))
        sel_info = (names, line_starts)

        self.refresh_mark_highlights()
        self.restore_sels(sel_info)
        self.view.run_command("dired_draw_vcs_marker")
        self.refresh_clipboard_highlights()
        self.recreate_dired_expanded_paths_from_view()

    def fold(self, edit, line: Region) -> Region | None:
        '''
        line is a Region, on which folding is supposed to happen (or not).
        Returns the parent directory line region if a fold was performed, else None.
        '''
        line, indented_region = self.get_indented_region(line)
        if not indented_region:
            return None  # folding is not supposed to happen, so we exit
        self.apply_change_into_view(edit, line, indented_region)
        return line

    def get_indented_region(self, line):
        '''Return tuple:
            line
                Region which shall NOT be erased, can be equal to argument line or less if folding
                was called on indented file item or indented collapsed directory
            indented_region
                Region which shall be erased
        '''
        v = self.view
        eol = line.b - 1
        if 'error' in v.scope_name(eol):  # remove inline error, e.g. <empty>
            indented_region = v.extract_scope(eol)
            return (line, indented_region)

        current_region = v.indented_region(line.b)
        next_region    = v.indented_region(line.b + 2)

        is_dir     = 'directory' in v.scope_name(line.a)
        next_empty = next_region.empty()
        this_empty = current_region.empty()
        line_in_next = next_region.contains(line)
        this_in_next = next_region.contains(current_region)

        def __should_exit():
            collapsed_dir = self.update and (line_in_next or next_empty or this_in_next)
            item_in_root  = (not is_dir or next_empty) and this_empty
            return collapsed_dir or item_in_root

        if __should_exit():
            return (None, None)

        elif self.update or (is_dir and not next_empty and not line_in_next):
            indented_region = next_region

        elif not this_empty:
            indented_region = current_region
            line = v.line(indented_region.a - 2)

        else:
            return (None, None)

        return (line, indented_region)

    def apply_change_into_view(self, edit, line, indented_region):
        '''set count and index, track marks/selections, replace icon, erase indented_region'''
        v = self.view
        start_line = 1 + v.rowcol(line.a)[0]

        # do not set count & index on empty directory
        if not line.contains(indented_region):
            removed_count = len(v.lines(indented_region))
            dired_count = v.settings().get('dired_count', 0)
            v.settings().set('dired_count', int(dired_count) - removed_count)
            if indented_region.b == v.size():
                # MUST avoid new line at eof
                indented_region = Region(indented_region.a - 1, indented_region.b)

            end_line   = start_line + removed_count
            self.index = self.index[:start_line] + self.index[end_line:]
            v.settings().set('dired_index', self.index)

        name_point  = self._get_name_point(line)
        icon_region = Region(name_point - 2, name_point - 1)

        v.set_read_only(False)
        v.replace(edit, icon_region, '▸')
        v.erase(edit, indented_region)
        v.set_read_only(True)
        if self.view.settings().get('dired_autorefresh', True):
            emit_event('remove_path', (self.view.id(), self.index[start_line - 1]))
        else:
            emit_event('stop_watch', self.view.id())


class dired_up(TextCommand, DiredBaseCommand):
    def run(self, edit):
        path = self.path
        if path == 'ThisPC\\':
            self.view.run_command('dired_refresh')
            return

        remembered_selection = None
        if path:
            self.index = self.get_all()
            selections = self.get_selected(parent=False, full=False)
            if selections:
                remembered_selection = selections[0]

        parent = dirname(path.rstrip(os.sep))
        if not parent.endswith(os.sep):
            parent += os.sep
        if parent == path:
            if NT:
                parent = 'ThisPC\\'
            else:
                return

        s = self.view.settings()
        # Remember position
        if remembered_selection:
            data = s.get('dired_last_selection_by_dir', {})
            data[path] = remembered_selection
            s.set('dired_last_selection_by_dir', data)

        # Clear any persisted marks and disable active filter when navigating up
        s.set('dired_marked_paths', [])
        self.refresh_mark_highlights()
        s.set('dired_filter_enabled', False)

        view_for_reuse = (self.view if reuse_view() else None)
        goto = basename(path.rstrip(os.sep)) or path
        self.history_push()
        show(self.view.window(), parent, reuse_view=view_for_reuse, goto=goto)


class dired_goto(TextCommand, DiredBaseCommand):
    """
    Prompt for a new directory.
    """
    def run(self, edit):
        prompt.start('Goto:', self.view.window(), self.path, self.goto)

    def goto(self, path):
        show(self.view.window(), path, reuse_view=self.view)


# MARKING ###########################################################

class dired_history_back(TextCommand, DiredBaseCommand):
    """Navigate back to the previous directory state in history."""
    def run(self, edit):
        s = self.view.settings()
        history = s.get('dired_history', [])
        cursor = s.get('dired_history_cursor', 0)
        if cursor < 1:
            return sublime.status_message('No previous location')

        # Replace current entry with any edits before leaving
        history = self.history_replace()

        # Move back based on pre-snapshot position
        next_cursor = cursor - 1
        target = history[next_cursor]
        s.set('dired_history_cursor', next_cursor)
        self._apply_history_entry(target)


class dired_history_forward(TextCommand, DiredBaseCommand):
    """Navigate forward to the next directory state in history."""
    def run(self, edit):
        s = self.view.settings()
        history: list = s.get('dired_history', [])
        cursor = s.get('dired_history_cursor', 0)
        if cursor >= len(history) - 1:
            return sublime.status_message('No next location')

        # Replace current entry with any edits before leaving
        history = self.history_replace()

        # Move forward based on pre-snapshot position
        next_cursor = cursor + 1
        target = history[next_cursor]
        s.set('dired_history_cursor', next_cursor)
        self._apply_history_entry(target)


class dired_mark_extension(TextCommand, DiredBaseCommand):
    def run(self, edit):
        filergn = self.fileregion()
        if filergn.empty():
            return
        current_item = self.view.substr(self.view.line(self.view.sel()[0].a))
        if current_item.endswith(os.sep) or current_item == PARENT_SYM:
            ext = ''
        else:
            ext = current_item.split('.')[-1]
        window = self.view.window()
        assert window
        pv = window.show_input_panel('Extension:', ext, self.on_done, None, None)
        pv.run_command("select_all")

    def on_done(self, ext):
        ext = ext.strip()
        if not ext:
            return
        if not ext.startswith('.'):
            ext = '.' + ext
        # Mark all files matching the extension plus keep existing marks
        self.index = self.get_all()
        paths = set(self.view.settings().get('dired_marked_paths') or [])
        matches = [p for p in self.index if p and p != PARENT_SYM and p.endswith(ext)]
        paths.update(matches)
        self.view.settings().set('dired_marked_paths', sorted(paths))
        self.refresh_mark_highlights()
        self.set_status()


class dired_mark(TextCommand, DiredBaseCommand):
    """
    Marks or unmarks files.

    The mark can be set to True to mark a file, False to unmark a file, or 'toggle' to
    toggle the mark.

    By default only selected files are marked, but if markall is True all files are
    marked/unmarked and the selection is ignored.

    If there is no selection and mark is '*', the cursor is moved to the next line so
    successive files can be marked by repeating the mark key binding (e.g. 'm').
    """
    def run(self, edit, mark=True, markall=False, forward=True, move_cursor=True):
        assert mark in (True, False, 'toggle')

        # If there is no selection, move the cursor so the user can keep pressing 'm'
        # to mark or toggle successive files.
        should_move = (
            move_cursor
            and not markall
            and len(self.view.sel()) == 1
            and self.view.sel()[0].empty()
        )

        if should_move and not forward:
            self.move(forward)

        filergn = self.fileregion()
        if filergn.empty():
            return

        # Compute target paths
        self.index = self.get_all()
        if markall:
            targets = [p for p in self.index if p and p != PARENT_SYM]
        else:
            targets = self.get_selected(parent=False, full=True)

        paths = set(self.view.settings().get('dired_marked_paths') or [])

        if mark == 'toggle':
            for p in targets:
                if p in paths:
                    paths.discard(p)
                else:
                    paths.add(p)
        elif mark is True:
            paths.update(targets)
        else:  # mark is False
            if markall:
                paths.clear()
            else:
                for p in targets:
                    paths.discard(p)

        self.view.settings().set('dired_marked_paths', sorted(paths))
        self.refresh_mark_highlights()
        self.set_status()

        if should_move and forward:
            self.move(forward)


class dired_expand_all(TextCommand, DiredBaseCommand):
    def run(self, edit):
        # Expand all: behavior depends on filter state.
        # - If no filter is active, expand all visible directories.
        # - If a filter is active, expand one level deeper under currently
        #   expanded directories to surface matches from subfolders.
        # Ensure hidden-files preference is available for list operations
        s = self.view.settings()
        self.show_hidden = s.get('dired_show_hidden_files', True)
        enabled = s.get('dired_filter_enabled', True)
        filter_active = enabled and (s.get('dired_filter') or s.get('dired_filter_extension'))
        expanded_paths = s.get('dired_expanded_paths') or []
        expanded_set = set(expanded_paths)

        def is_dot_dir(path: str) -> bool:
            return os.path.basename(path.rstrip(os.sep)).startswith('.')

        if not filter_active:
            self.index = self.get_all()
            dir_paths = [
                p for p in self.index
                if p and p.endswith(os.sep)
                if p not in expanded_set
                if not is_dot_dir(p)
            ]
            if not dir_paths:
                return sublime.status_message('No directories to expand')
            self.view.run_command('dired_refresh', {'to_expand': dir_paths, 'toggle': False})
            return

        # Filter active: expand children of already expanded directories
        # If nothing is expanded yet, start from the current root path
        if not expanded_paths:
            expanded_paths = [self.path]
        to_expand = set()
        for parent in expanded_paths:
            names, _ = self.list_directory(parent)
            for nm in names:
                # Build child path and normalize to dir-with-trailing-sep
                if parent:
                    child = join(parent, nm)
                    if isdir(child):
                        child_dir = child.rstrip(os.sep) + os.sep
                    else:
                        continue
                else:
                    # ThisPC root on Windows: names like "C:"; synthesize a
                    # directory path with a trailing separator
                    child_dir = nm.rstrip(os.sep) + os.sep
                if not is_dot_dir(child_dir) and child_dir not in expanded_set:
                    to_expand.add(child_dir)

        if not to_expand:
            return sublime.status_message('No child directories to expand')

        self.view.run_command('dired_refresh', {'to_expand': sorted(to_expand), 'toggle': False})


class dired_collapse_all(TextCommand, DiredBaseCommand):
    def run(self, edit):
        s = self.view.settings()
        # Collapse the deepest expanded directories (one level up)
        expanded = s.get('dired_expanded_paths') or []
        if not expanded:
            return sublime.status_message('No directories to collapse')

        def depth(p: str) -> int:
            return p.rstrip(os.sep).count(os.sep)

        # Find the maximum depth among expanded paths
        max_depth = max((depth(p) for p in expanded), default=-1)

        # Directly update the persisted expanded set and trigger a full
        # refresh. This is robust in the presence of live filters and
        # view/index drift.
        new_expanded = [p for p in expanded if depth(p) < max_depth]
        if len(new_expanded) == len(expanded):
            return sublime.status_message('Nothing to collapse')
        s.set('dired_expanded_paths', new_expanded)
        self.view.run_command('dired_refresh')


# OTHER #############################################################

class dired_toggle_hidden_files(TextCommand):
    def run(self, edit):
        show = self.view.settings().get('dired_show_hidden_files', True)
        self.view.settings().set('dired_show_hidden_files', not show)
        self.view.run_command('dired_refresh')


# MOUSE INTERACTIONS #################################################

class dired_doubleclick(TextCommand, DiredBaseCommand):
    def run_(self, _, args):
        view = self.view
        s = view.settings()
        if s.get("dired_path"):
            if 'directory' in view.scope_name(view.sel()[0].a):
                view.run_command("dired_expand", {"toggle": True})
            else:
                view.run_command("dired_select", {"other_group": True})


def format_stats(entry: ListingItem, show_stats: bool, date_format='%d.%m.%Y %H:%M'):
    if not show_stats:
        return '', ''
    if entry.is_dir:
        size_part = 'DIR'
    elif entry.size is not None:
        size_part = f"{entry.size:,}".replace(",", ".")
    else:
        return '', ''
    if isinstance(entry.mtime, float):
        timestamp = datetime.fromtimestamp(entry.mtime).strftime(date_format)
    else:
        timestamp = entry.mtime or ''
    return size_part, timestamp


def format_items(items, show_stats, tab_size):
    rendered = []
    new_index = []
    for e in items:
        indent = '\t' * e.depth
        if e.is_dir:
            icon = '▾' if e.expanded else '▸'
            note = f'\t<{e.note}>' if e.note else ''
            base_line = f"{indent}{icon} {e.name}{os.sep}{note}"
        else:
            base_line = f"{indent}≡ {e.name}"
        display_len = len(base_line.expandtabs(tab_size))
        size_part, timestamp = format_stats(e, show_stats)
        rendered.append((base_line, display_len, size_part, timestamp))
        new_index.append(e.full_path)
    return rendered, new_index


def render_items(items, name_width, size_width, show_stats):
    lines = []
    for base_line, display_len, size_part, timestamp in items:
        if show_stats and size_part:
            padding = max(name_width - display_len + STATS_PADDING, STATS_PADDING)
            size_aligned = size_part.rjust(size_width)
            stats_segment = size_aligned if not timestamp else f"{size_aligned}  {timestamp}"
            lines.append(f"{base_line}{' ' * padding}{SEPARATOR}{stats_segment}")
        else:
            lines.append(base_line)
    return lines
