from __future__ import annotations
import os
import sublime

from .common import first, set_proper_scheme, calc_width, get_group
SYNTAX_EXTENSION = '.sublime-syntax'


def create_dired_view(window: sublime.Window) -> sublime.View:
    """Create and return a new, empty dired view for the given window."""
    view = window.new_file()
    dired_settings = sublime.load_settings('dired.sublime-settings')
    if not dired_settings.get('dired_enable_stock_sublime_history', False):
        view.settings().set('is_widget', True)
    view.settings().add_on_change('color_scheme', lambda: set_proper_scheme(view))
    view.set_syntax_file('Packages/FileBrowser/dired' + SYNTAX_EXTENSION)
    view.set_scratch(True)
    return view


def retarget_dired_view(view: sublime.View, path: str, *, goto: str = '', to_expand=None) -> None:
    """Reuse an existing dired view for a new root path."""
    if not path.endswith(os.sep):
        path += os.sep

    old_path = view.settings().get('dired_path', '')
    reset_sels = path != old_path

    view.settings().set('dired_path', path)
    view.settings().set('dired_rename_mode', False)
    view.run_command('dired_refresh', {'goto': goto, 'reset_sels': reset_sels, 'to_expand': to_expand})


def set_active_group(window, view, other_group):
    nag = window.active_group()
    if other_group:
        group = 0 if other_group == 'left' else 1
        groups = window.num_groups()
        if groups == 1:
            width = calc_width(view)
            cols = [0.0, width, 1.0] if other_group == 'left' else [0.0, 1-width, 1.0]
            window.set_layout({
                "cols": cols,
                "rows": [0.0, 1.0],
                "cells": [[0, 0, 1, 1], [1, 0, 2, 1]]
            })
        elif view:
            group = get_group(groups, nag)
        window.set_view_index(view, group, 0)
    else:
        group = nag

    # when other_group is left, we need move all views to right except FB view
    if nag == 0 and other_group == 'left' and group == 0:
        for v in reversed(window.views_in_group(nag)[1:]):
            window.set_view_index(v, 1, 0)

    return (nag, group)


def get_or_create_dired_view(view, window, ignore_existing, path, single_pane) -> sublime.View:
    if not view and not ignore_existing:
        # See if a view for this path already exists.
        same_path = lambda v: v.settings().get('dired_path') == path
        # See if any reusable view exists in case of single_pane argument
        any_path = lambda v: v.score_selector(0, "text.dired") > 0
        view = first(window.views(), any_path if single_pane else same_path)

    if not view:
        view = create_dired_view(window)

    return view


def show(
    window,
    path,
    *,
    reuse_view: sublime.View | None = None,
    ignore_existing: bool = False,
    single_pane: bool = False,
    goto: str = '',
    other_group: str = '',
    to_expand=None
):
    """
    Determines the correct view to use, creating one if necessary, and prepares it.
    """
    if other_group:
        prev_focus = window.active_view()
        # simulate 'toggle sidebar':
        if prev_focus and 'dired' in prev_focus.scope_name(0):
            window.run_command('close_file')
            return

    if not path.endswith(os.sep):
        path += os.sep

    view = get_or_create_dired_view(reuse_view, window, ignore_existing, path, single_pane)
    set_active_group(window, view, other_group)
    if other_group and prev_focus:
        window.focus_view(prev_focus)

    # forcibly shoot on_activated, because when view was created it didnot have any settings
    window.show_quick_panel(['a', 'b'], None)
    retarget_dired_view(view, path, goto=goto, to_expand=to_expand)
    window.run_command('hide_overlay')
    window.focus_view(view)
