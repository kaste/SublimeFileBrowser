from __future__ import annotations
import os
import sublime

from .common import first, set_proper_scheme, calc_width, get_group, history_push
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


def retarget_dired_view(
    view: sublime.View,
    path: str,
    *,
    goto: str = '',
    expanded=None,
) -> None:
    """Reuse an existing dired view for a new root path.

    When `expanded` is provided, it replaces the view's persisted expanded set
    before refresh.
    """
    if not path.endswith(os.sep):
        path += os.sep

    old_path = view.settings().get('dired_path', '')
    reset_sels = path != old_path

    view.settings().set('dired_path', path)
    view.settings().set('dired_rename_mode', False)
    if expanded is not None:
        view.settings().set('dired_expanded_paths', expanded)
    view.run_command('dired_refresh', {'goto': goto, 'reset_sels': reset_sels})


def get_or_create_group_for_dired(window, view, other_group):
    """Ensure a group exists for a side-by-side dired view.

    Returns (active_group, target_group).
    """
    ag = window.active_group()
    groups = window.num_groups()
    if groups == 1:
        group = 0 if other_group == 'left' else 1
        width = calc_width(view)
        cols = [0.0, width, 1.0] if other_group == 'left' else [0.0, 1 - width, 1.0]
        window.set_layout({
            "cols": cols,
            "rows": [0.0, 1.0],
            "cells": [[0, 0, 1, 1], [1, 0, 2, 1]]
        })
    else:
        group = get_group(groups, ag)
    return ag, group


def adjust_window_layout_for_dired(window, view, other_group):
    ag, group = get_or_create_group_for_dired(window, view, other_group)
    window.set_view_index(view, group, 0)
    # when other_group is left, we move all views to right except the dired view
    if ag == 0 and other_group == 'left' and group == 0:
        for v in reversed(window.views_in_group(ag)[1:]):
            window.set_view_index(v, 1, 0)


def get_or_create_dired_view(window, ignore_existing, path, single_pane) -> sublime.View:
    """Return a suitable dired view for `path`, creating one if necessary."""
    view = None
    if not ignore_existing:
        # See if a view for this path already exists.
        same_path = lambda v: v.settings().get('dired_path') == path
        # See if any reusable view exists in case of single_pane argument
        any_path = lambda v: v.score_selector(0, "text.dired") > 0
        view = first(window.views(), any_path if single_pane else same_path)

    return view or create_dired_view(window)


def show(
    window,
    path,
    *,
    reuse_view: sublime.View | None = None,
    ignore_existing: bool = False,
    single_pane: bool = False,
    goto: str = '',
    other_group: str = '',
    expanded=None,
    record_history: bool = False,
):
    """
    Determine the correct dired view to use, creating one if necessary, and prepare it.

    - `path`: absolute root directory for the view (trailing `os.sep` is added).
      The sentinel `ThisPC\\` represents the drive listing on Windows.
    - `goto`: optional target to focus after refresh; can be relative or
      absolute. If absolute, it must be under `path`. If it contains separators,
      parent directories are expanded (relative paths are rooted under `path`).
    - `expanded`: list of absolute directory paths (with trailing `os.sep`) to
      treat as the full expanded set for the view on refresh.
    - `record_history`: when True, push a history entry before retargeting an
      existing dired view (no-op for newly created views or unchanged roots).
    """
    if other_group:
        av = window.active_view()
        if av and 'dired' in av.scope_name(0):
            window.run_command('close_file')
            return None

    if not path.endswith(os.sep):
        path += os.sep

    view = reuse_view or get_or_create_dired_view(window, ignore_existing, path, single_pane)
    old_path = view.settings().get('dired_path')
    if record_history and old_path and old_path != path:
        history_push(view)
    if other_group:
        adjust_window_layout_for_dired(window, view, other_group)

    if single_pane:
        current_root = view.settings().get("dired_path")
        if current_root and path.startswith(current_root):
            current_index = view.settings().get("dired_index")
            if path in current_index:
                path, goto = current_root, os.path.join(path, goto)

    retarget_dired_view(view, path, goto=goto, expanded=expanded)
    window.focus_view(view)
    return view
