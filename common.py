'''Common stuff, used in other modules'''
from __future__ import annotations
import re
import os
import fnmatch
from itertools import chain, repeat
from typing import Iterable

import sublime
from sublime import Region
from os.path import isdir, join, basename

try:  # unavailable dependencies shall not break basic functionality
    import package_events
except ImportError:
    package_events = None

flatten = chain.from_iterable
PLATFORM = sublime.platform()
NT = PLATFORM == 'windows'
LIN = PLATFORM == 'linux'
OSX = PLATFORM == 'osx'

if NT:
    import ctypes


EVENT_TOPIC = 'FileBrowser'
MARK_OPTIONS = sublime.DRAW_NO_OUTLINE
RE_FILE = re.compile(r'^(\s*)([^\\//].*)$')
PARENT_SYM = "⠤"


def first(seq, pred):
    '''similar to built-in any() but return the object instead of boolean'''
    return next((item for item in seq if pred(item)), None)


def sort_nicely(names):
    """ Sort the given list in the way that humans expect.
    Source: http://www.codinghorror.com/blog/2007/12/sorting-for-humans-natural-sort-order.html
    """
    convert = lambda text: int(text) if text.isdigit() else text.lower()
    alphanum_key = lambda key: [convert(c) for c in re.split('([0-9]+)', key)]
    names.sort(key=alphanum_key)


def set_proper_scheme(view):
    '''
    this is callback, it is not meant to be called directly
        view.settings().add_on_change('color_scheme', lambda: set_proper_scheme(view))
    set once, right after view is created
    _note_, color_scheme must not be set directly, but in a setting file
    '''
    # Since we cannot create file with syntax, there is moment when view has no settings,
    # but it is activated, so some plugins (e.g. Color Highlighter) set wrong color scheme
    if view.settings().get('dired_rename_mode', False):
        dired_settings = sublime.load_settings('dired-rename-mode.sublime-settings')
    else:
        dired_settings = sublime.load_settings('dired.sublime-settings')

    color_scheme = dired_settings.get('color_scheme')
    if view.settings().get('color_scheme') == color_scheme:
        return

    if color_scheme:
        view.settings().set('color_scheme', color_scheme)
    else:
        view.settings().erase('color_scheme')


def calc_width(view):
    '''
    return float width, which must be
        0.0 < width < 1.0 (other values acceptable, but cause unfriendly layout)
    used in show.show() and "dired_select" command with other_group=True
    '''
    width = view.settings().get('dired_width', 0.3)
    if isinstance(width, float):
        width -= width // 1  # must be less than 1
    elif isinstance(width, int):  # assume it is pixels
        wport = view.viewport_extent()[0]
        width = 1 - round((wport - width) / wport, 2)
        if width >= 1:
            width = 0.9
    else:
        sublime.error_message(
            'FileBrowser:\n\ndired_width set to '
            'unacceptable type "{0}", please change it.\n\n'
            'Fallback to default 0.3 for now.'
            .format(type(width))
        )
        width = 0.3
    return width or 0.1  # avoid 0.0


def get_group(groups, nag):
    '''
    groups  amount of groups in window
    nag     number of active group
    return number of neighbour group
    '''
    if groups <= 4 and nag < 2:
        group = 1 if nag == 0 else 0
    elif groups == 4 and nag >= 2:
        group = 3 if nag == 2 else 2
    else:
        group = nag - 1
    return group


def relative_path(rpath):
    '''rpath is either list or empty string (if list, we need only first item);
    return either empty string or rpath[0] (or its parent), e.g.
        foo/bar/ → foo/bar/
        foo/bar  → foo/
    '''
    if rpath:
        rpath = rpath[0]
        if rpath[~0] != os.sep:
            rpath = os.path.split(rpath)[0] + os.sep
        if rpath == os.sep:
            rpath = ''
    return rpath


def hijack_window():
    '''Execute on loading plugin or on new window open;
    allow to open FileBrowser automatically
    '''
    settings = sublime.load_settings('dired.sublime-settings')
    command = settings.get("dired_hijack_new_window")
    if command:
        if command == "jump_list":
            sublime.set_timeout(lambda: sublime.windows()[-1].run_command("dired_jump_list"), 1)
        else:
            sublime.set_timeout(
                lambda: sublime.windows()[-1].run_command("dired", {"immediate": True}), 1)


def emit_event(event_type: str, payload: object):
    '''Notify our filesystem observer about changes in our views'''
    if package_events:
        package_events.notify(EVENT_TOPIC, event_type, payload)


class DiredBaseCommand:
    """
    Convenience functions for dired TextCommands
    """
    @property
    def path(self):
        return self.view.settings().get('dired_path')

    def get_path(self):
        path = self.path
        if path == 'ThisPC\\':
            path = ''
        return path

    def filecount(self):
        """
        Returns the number of files and directories in the view.
        """
        return self.view.settings().get('dired_count', 0)

    def move_to_extreme(self, extreme="bof"):
        """
        Moves the cursor to the beginning or end of file list.  Clears all sections.
        """
        files = self.fileregion(with_parent_link=True)
        self.view.sel().clear()
        if extreme == "bof":
            ext_region = Region(files.a, files.a)
        else:
            name_point = self.view.extract_scope(self.view.line(files.b).a + 2).a
            ext_region = Region(name_point, name_point)
        self.view.sel().add(ext_region)
        self.view.show_at_center(ext_region)

    def move(self, forward=None):
        """
        Moves the cursor one line forward or backwards.  Clears all sections.
        """
        assert forward in (True, False), 'forward must be set to True or False'

        files = self.fileregion(with_parent_link=True)
        if not files:
            return

        new_sels = []
        for s in list(self.view.sel()):
            new_sels.append(self._get_name_point(self.next_line(forward, s.a, files)))

        self.view.sel().clear()
        for n in new_sels:
            self.view.sel().add(Region(n, n))
        name_point = new_sels[~0] if forward else new_sels[0]
        surroundings = True if self.view.rowcol(name_point)[0] < 3 else False
        self.view.show(name_point, surroundings)

    def next_line(self, forward, pt, filergn):
        '''Return Region of line for pt within filergn'''
        if filergn.contains(pt):
            # Try moving by one line.
            line = self.view.line(pt)
            pt = forward and (line.b + 1) or (line.a - 1)
        if not filergn.contains(pt):
            # Not (or no longer) in the list of files, so move to the closest edge.
            pt = (pt > filergn.b) and filergn.b or filergn.a
        return self.view.line(pt)

    def _get_name_point(self, line):
        '''Return point at which filename starts (i.e. after icon & whitespace)'''
        scope = self.view.scope_name(line.a)
        if 'indent' in scope:
            name_point = self.view.extract_scope(line.a).b
        else:
            name_point = line.a
        return name_point + (2 if 'parent_dir' not in scope else 0)

    def show_parent(self):
        return self.view.settings().get('dired_show_parent', False)

    def fileregion(self, with_parent_link=False):
        """
        Returns a region containing the lines containing filenames.
        If there are no filenames None is returned.
        """
        if with_parent_link:
            all_items = sorted(self.view.find_by_selector('dired.item'))
        else:
            all_items = sorted(self.view.find_by_selector('dired.item.directory') +
                               self.view.find_by_selector('dired.item.file'))
        if not all_items:
            return None
        return Region(all_items[0].a, all_items[~0].b)

    def get_parent(self, line: Region, path: str) -> str:
        '''
        Returns relative path for line
            • line is a region
            • path is self.path
            • self.index is list stored in view settings as 'dired_index'
        '''
        return self.get_fullpath_for(line).replace(path, '', 1)

    def get_fullpath_for(self, line: Region) -> str:
        return self.index[self.view.rowcol(line.a)[0]]

    def get_all(self):
        """
        Returns a list of all filenames in the view.
        dired_index is always supposed to represent current state of view,
        each item matches corresponding line, thus list will never be empty unless sth went wrong;
        if header is enabled then first two elements are empty strings
        """
        index = self.view.settings().get('dired_index', [])
        if not index:
            return sublime.error_message('FileBrowser:\n\n"dired_index" is empty,\n'
                                         'that shouldn’t happen ever, there is some bug.')
        return index

    def get_all_relative(self, path):
        return [f.replace(path, '', 1) for f in self.get_all()]

    def get_selected(self, parent=True, full=False) -> list[str]:
        """
        parent
            if False, returned list does not contain PARENT_SYM even if it is in view
        full
            if True, items in returned list are full paths, else relative

        Returns a list of selected filenames.
        self.index should be assigned before call it
        """
        fileregion = self.fileregion(with_parent_link=parent)
        if not fileregion:
            return []
        path = self.get_path()
        names = []
        for line in self._get_lines([s for s in self.view.sel()], fileregion):
            text = self.get_fullpath_for(line) if full else self.get_parent(line, path)
            if text and text not in names:
                names.append(text)
        return names

    def get_marked(self, full=False) -> list[str]:
        """Return the set of marked items based on persisted paths.

        Intersects `dired_marked_paths` with the current, visible index so the
        result reflects what’s shown in this view. Returns absolute paths when
        `full=True`, otherwise paths relative to the current root.
        """
        if not self.filecount():
            return []
        self.index = self.get_all()
        marked = set(self.view.settings().get('dired_marked_paths') or [])
        visible = [p for p in self.index if p and p in marked]
        if full:
            return visible
        root = self.get_path()
        if not root:
            return visible
        return [p.replace(root, '', 1) if p.startswith(root) else p for p in visible]

    def _get_lines(self, regions, within):
        '''
        regions is a list of non-overlapping region(s), each may have many lines
        within  is a region which is supposed to contain each line
        '''
        return (
            line
            for line in chain(*(self.view.lines(r) for r in regions))
            if within.contains(line)
        )

    def set_ui_in_rename_mode(self, edit):
        header = self.view.settings().get('dired_header', False)
        if header:
            regions = self.view.find_by_selector(
                'text.dired header.dired punctuation.definition.separator.dired')
        else:
            regions = self.view.find_by_selector('text.dired dired.item.parent_dir')
        if not regions:
            return
        region = regions[0]
        start = region.begin()
        self.view.erase(edit, region)
        if header:
            new_text = "——[RENAME MODE]——" + ("—" * (region.size() - 17))
        else:
            new_text = "⠤ [RENAME MODE]"
        self.view.insert(edit, start, new_text)

    def set_status(self):
        '''Update status-bar;
        self.show_hidden must be assigned before call it'''
        # if view isnot focused, view.window() may be None
        window          = self.view.window() or sublime.active_window()
        path_in_project = any(folder == self.path[:-1] for folder in window.folders())
        settings        = self.view.settings()
        copied_items    = settings.get('dired_to_copy', [])
        cut_items       = settings.get('dired_to_move', [])
        status = " 𝌆 [?: Help] {0}Hidden: {1}{2}{3}".format(
            'Project root, ' if path_in_project else '',
            'On' if self.show_hidden else 'Off',
            ', copied(%d)' % len(copied_items) if copied_items else '',
            ', cut(%d)' % len(cut_items) if cut_items else ''
        )
        self.view.set_status("__FileBrowser__", status)

    def prepare_filelist(self, names, path, goto, indent):
        '''About self.index see DiredRefreshCommand
        could be called from  DiredExpand.expand_single_folder
                     or from  DiredRefresh.continue_refresh
        '''
        items   = []
        tab     = self.view.settings().get('tab_size')
        line    = self.view.line(self.sel.a if self.sel is not None else self.view.sel()[0].a)
        content = self.view.substr(line).replace('\t', ' ' * tab)
        ind     = re.compile(r'^(\s*)').match(content).group(1)
        level   = indent * int((len(ind) / tab) + 1) if ind else indent
        files   = []
        index_dirs  = []
        index_files = []
        for name in names:
            full_name = join(path, goto, name)
            if isdir(full_name):
                index_dirs.append('%s%s' % (full_name, os.sep))
                items.append(''.join([level, "▸ ", name, os.sep]))
            else:
                index_files.append(full_name)
                files.append(''.join([level, "≡ ", name]))
        index = index_dirs + index_files
        self.index = self.index[:self.number_line] + index + self.index[self.number_line:]
        items += files
        return items

    def is_hidden(self, filename, path, goto=''):
        if not (path or goto):  # special case for ThisPC
            return False
        tests = self.view.settings().get('dired_hidden_files_patterns', ['.*'])
        if isinstance(tests, str):
            tests = [tests]
        if any(fnmatch.fnmatch(filename, pattern) for pattern in tests):
            return True
        if sublime.platform() != 'windows':
            return False
        # check for attribute on windows:
        try:
            attrs = ctypes.windll.kernel32.GetFileAttributesW(join(path, goto, filename))
            assert attrs != -1
            result = bool(attrs & 2)
        except (AttributeError, AssertionError):
            result = False
        return result

    def try_listing_directory(self, path):
        '''Return tuple of two element
            items  sorted list of filenames in path, or empty list
            error  exception message, or empty string
        '''
        items, error = [], ''
        # Special root: ThisPC (empty path) → list drives on Windows
        if not path:
            items = [f"{s}:" for s in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' if isdir(f"{s}:")]
        else:
            try:
                items = os.listdir(path)
            except OSError as e:
                error = str(e)
                if NT:
                    error = (
                        error
                        .split(':')[0]
                        .replace('[Error 5] ', 'Access denied')
                        .replace('[Error 3] ', 'Not exists, press r to refresh')
                    )

        if not self.show_hidden:
            items = [name for name in items if not self.is_hidden(name, path)]
        # Apply optional filters
        s = self.view.settings()
        enabled = s.get('dired_filter_enabled', True)
        if enabled:
            if ext_filter := s.get('dired_filter_extension', '').lower():
                items = [n for n in items if os.path.splitext(n)[1].lower() == ext_filter]
            if flt := s.get('dired_filter'):
                items = rx_fuzzy_filter(flt, items)

        sort_nicely(items)
        return items, error

    def try_listing_directory_raw(self, path):
        '''Like try_listing_directory but without applying name/extension filters.
        Respects hidden-file setting and returns sorted names.'''
        items, error = [], ''
        if not path:
            items = [f"{s}:" for s in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' if isdir(f"{s}:")]
        else:
            try:
                items = os.listdir(path)
            except OSError as e:
                error = str(e)
                if NT:
                    error = (
                        error
                        .split(':')[0]
                        .replace('[Error 5] ', 'Access denied')
                        .replace('[Error 3] ', 'Not exists, press r to refresh')
                    )
        if not self.show_hidden:
            items = [name for name in items if not self.is_hidden(name, path)]
        sort_nicely(items)
        return items, error

    def try_listing_only_dirs(self, path):
        '''Same as self.try_listing_directory, but items contains only directories.
        Used for prompt completion'''
        items, error = self.try_listing_directory(path)
        if items:
            items = [n for n in items if isdir(join(path, n))]
        return (items, error)

    def restore_sels(self, sels=None):
        '''
        sels is tuple of two elements:
            0 list of filenames
                relative paths to search in the view
            1 list of Regions
                copy of view.sel(), used for fallback if filenames are not found
                in view (e.g. user deleted selected file)
        '''
        if sels:
            seled_fnames, seled_regions = sels
            path = self.get_path()
            regions = []
            for selection in seled_fnames or []:
                matches = self._find_in_view(selection)
                for region in matches:
                    filename = self.get_parent(region, path)
                    if filename == selection:
                        name_point = self._get_name_point(region)
                        regions.append(Region(name_point, name_point))
                        break
            if regions:
                return self._add_sels(regions)
            else:
                # If we’re live filtering and previously landed on the parent line,
                # prefer the first actual item instead of restoring the old region.
                flt = self.view.settings().get('dired_filter')
                enabled = self.view.settings().get('dired_filter_enabled', True)
                filter_extension = self.view.settings().get('dired_filter_extension')
                if enabled and (flt or filter_extension):
                    # Check if first selection region is the parent link
                    is_parent = 'parent_dir' in self.view.scope_name(seled_regions[0].a)
                    has_items = bool(
                        self.view.find_by_selector('text.dired dired.item.directory string.name.directory.dired ')
                        or self.view.find_by_selector('text.dired dired.item.file string.name.file.dired ')
                    )
                    if is_parent and has_items:
                        return self._add_sels()  # triggers filtered-first fallback
                # e.g. when user remove file(s), we just restore sel RegionSet
                # despite positions may be wrong sometimes
                return self._add_sels(seled_regions)
        # fallback:
        return self._add_sels()

    def _find_in_view(self, item):
        '''item is Unicode'''
        fname = re.escape(basename(os.path.abspath(item)) or item.rstrip(os.sep))
        if item[~0] == os.sep:
            pattern = r'^\s*[▸▾] '
            sep = re.escape(os.sep)
        else:
            pattern = r'^\s*≡ '
            sep = ''
        return self.view.find_all('%s%s%s' % (pattern, fname, sep))

    def _add_sels(self, sels=None):
        self.view.sel().clear()

        if sels:
            eof = self.view.size()
            for s in sels:
                if s.begin() <= eof:
                    self.view.sel().add(s)

        if not sels or not list(self.view.sel()):  # all sels more than eof
            fbs = self.view.find_by_selector
            # Prefer first real item when a live filter is active
            flt = self.view.settings().get('dired_filter')
            enabled = self.view.settings().get('dired_filter_enabled', True)
            filter_extension = self.view.settings().get('dired_filter_extension')
            if enabled and (flt or filter_extension):
                item = (
                    fbs('text.dired dired.item.directory string.name.directory.dired ')
                    or fbs('text.dired dired.item.file string.name.file.dired ')
                    or fbs('text.dired dired.item.parent_dir ')
                )
            else:
                item = (
                    fbs('text.dired dired.item.parent_dir ')
                    or fbs('text.dired dired.item.directory string.name.directory.dired ')
                    or fbs('text.dired dired.item.file string.name.file.dired ')
                )
            s = Region(item[0].a, item[0].a) if item else Region(0, 0)
            self.view.sel().add(s)

        self.view.show(s, False)

    # --- Clipboard highlight helpers -----------------------------------------

    def _build_regions_for_paths(self, paths):
        """Return line regions covering filename spans for given absolute paths.

        Uses `self.index` (row -> full path) for a direct lookup.
        """
        if not paths:
            return []

        # Ensure we have the up-to-date index
        self.index = self.get_all()

        # Build path -> row map, skipping header and parent entries
        row_by_path = {
            p: i for i, p in enumerate(self.index) if p and p != PARENT_SYM
        }

        regions = []
        for item in set(paths):
            row = row_by_path.get(item)
            if row is None:
                continue
            pt = self.view.text_point(row, 0)
            line = self.view.line(pt)
            name_point = self._get_name_point(line)
            regions.append(Region(name_point, line.b))
        return regions

    def refresh_clipboard_highlights(self, copied=None, cut=None):
        """Apply visual highlights for items in the internal clipboard.

        If lists are not provided, read them from settings. This is invoked:
        - after copy/cut to show immediate feedback,
        - after refresh/expand to restore highlights after a redraw.
        """
        if copied is None or cut is None:
            # Merge from both global and view settings for robustness
            g = sublime.load_settings('dired.sublime-settings')
            v = self.view.settings()
            if copied is None:
                copied = (g.get('dired_to_copy') or []) or (v.get('dired_to_copy') or [])
            if cut is None:
                cut = (g.get('dired_to_move') or []) or (v.get('dired_to_move') or [])

        copied_regions = self._build_regions_for_paths(copied)
        cut_regions = self._build_regions_for_paths(cut)
        self.view.add_regions('copied', copied_regions, 'dired.copied', '', MARK_OPTIONS)
        self.view.add_regions('cut', cut_regions, 'dired.cut', '', MARK_OPTIONS)

    def refresh_mark_highlights(self):
        """Re-create marked item highlights from source-of-truth paths.

        Reads absolute paths from `dired_marked_paths` on the view and draws
        regions using the current index. Keeps visuals in sync after any redraw.
        """
        marks = self.view.settings().get('dired_marked_paths') or []
        regions = self._build_regions_for_paths(marks)
        if regions:
            self.view.add_regions('marked', regions, 'dired.marked', '', MARK_OPTIONS)
        else:
            self.view.erase_regions('marked')

    def display_path(self, folder):
        return display_path(folder)

    # --- Live filter highlight -------------------------------------------------
    def update_filter_highlight(self):
        """Highlight occurrences of the active filter within visible item names.

        Uses `add_regions` with a soft background style. Clears highlights when
        no filter is set. Safe to call after any view rewrite (refresh/expand).
        """
        key = 'dired_filter_hits'
        s = self.view.settings()
        enabled = s.get('dired_filter_enabled', True)
        flt = s.get('dired_filter')
        filter_extension = s.get('dired_filter_extension')
        if not enabled or (not flt and not filter_extension):
            self.view.erase_regions(key)
            return

        regions = []
        # Extension highlight
        if filter_extension:
            ext_l = filter_extension.lower()
            for r in self.view.find_by_selector('text.dired string.name.file.dired'):
                name = self.view.substr(r)
                if len(name) >= len(ext_l) and name.lower().endswith(ext_l):
                    start = r.b - len(ext_l)
                    regions.append(Region(start, r.b))
        # Name fuzzy highlight
        if flt:
            for r in (
                self.view.find_by_selector('text.dired string.name.file.dired')
                + self.view.find_by_selector('text.dired string.name.directory.dired')
            ):
                name = self.view.substr(r)
                if pos := rx_fuzzy_match(flt, name):
                    regions.extend(Region(r.a + p, r.a + p + 1) for p in pos)

        # Subtle when not actively typing in the filter input panel
        subtle_highlighting = not self.view.settings().get('dired_filter_live', False)
        if regions:
            # Use a standard region scope for visibility across themes
            self.view.add_regions(
                key,
                combine_adjacent_regions(sorted(regions)),
                scope="region.bluish",
                flags=(
                    64
                    | (
                        sublime.DRAW_SOLID_UNDERLINE | sublime.DRAW_NO_OUTLINE
                        if subtle_highlighting else
                        0
                    )
                    | sublime.RegionFlags.DRAW_NO_FILL
                    | sublime.RegionFlags.NO_UNDO
                ),
            )
        else:
            self.view.erase_regions(key)


def display_path(folder):
    display = folder
    home = os.path.expanduser("~")
    if folder.startswith(home):
        display = folder.replace(home, "~", 1)
    return display


def combine_adjacent_regions(regions: Iterable[sublime.Region]) -> list[sublime.Region]:
    prev = None
    rv = []
    for r in regions:
        if prev is None or prev.b != r.a:
            rv.append(r)
            prev = r
        else:
            prev.b = r.b
    return rv


def rx_fuzzy_match(term: str, text: str) -> list[int]:
    R""" Boundary-only fuzzy match with non-greedy gaps.

    Pattern inserts a non-greedy, non-word-terminated gap between characters.
    Example term "abc" → (?:^|\W)(a)(?:.*\W)??(b)(?:.*\W)??(c)
    If term starts with '.', we do not anchor the first char to a word-boundary
    so that extensions like ".txt" match naturally.

    Returns start indices for each matched character or the falsy [].
    """
    if not term:
        return []

    parts = zip(
        chain(
            ['(?:.*)'] if term[0] == "." else [],
            repeat(r'(?:.*\W)??')
        ),
        [f'({re.escape(ch)})' for ch in term]
    )
    pattern = ''.join(flatten(parts))
    if m := re.search(pattern, text, re.IGNORECASE):
        return [m.start(i + 1) for i in range(len(term))]
    return []


def rx_fuzzy_filter(term: str, items: list[str]) -> list[str]:
    return [s for s in items if (m := rx_fuzzy_match(term, s))]
