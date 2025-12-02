'''Common stuff, used in other modules'''
from __future__ import annotations
import re
import os
import time
from os.path import isdir, join, basename
import fnmatch
from itertools import chain, repeat
from typing import Iterable, NamedTuple

import sublime
from sublime import Region

try:  # unavailable dependencies shall not break basic functionality
    import package_events
except ImportError:
    package_events = None

flatten = chain.from_iterable
PLATFORM = sublime.platform()
NT = PLATFORM == 'windows'
LIN = PLATFORM == 'linux'
OSX = PLATFORM == 'osx'
FILE_ATTRIBUTE_HIDDEN = 0x2


class EntryInfo(NamedTuple):
    name: str
    full_path: str
    is_dir: bool
    size: int | None
    mtime: float | None


class ListingItem(NamedTuple):
    name: str
    full_path: str
    is_dir: bool
    size: int | None
    mtime: float | str | None
    depth: int
    expanded: bool
    note: str

if NT:
    import ctypes


EVENT_TOPIC = 'FileBrowser'
MARK_OPTIONS = sublime.DRAW_NO_OUTLINE
RE_FILE = re.compile(r'^(\s*)([^\\//].*)$')
PARENT_SYM = "⠤"


def format_size_short(num_bytes: int) -> str:
    """Return a compact human-readable size like '12kB' or '3MB'."""
    if num_bytes <= 0:
        return ""
    units = ["B", "kB", "MB", "GB", "TB", "PB"]
    size = float(num_bytes)
    unit = 0
    while size >= 1024.0 and unit < len(units) - 1:
        size /= 1024.0
        unit += 1
    rounded = int(size + 0.5)
    return f"{rounded}{units[unit]}"

# Maximum acceptable time (seconds) for a single os.path.getsize call when
# computing summary sizes for status-bar display. If a call exceeds this,
# we abort size computation and show counts only.
SIZE_STAT_TIMEOUT = 0.005


def first(seq, pred):
    '''similar to built-in any() but return the object instead of boolean'''
    return next((item for item in seq if pred(item)), None)


def natural_sort_key(value: str):
    """Return a key for human-friendly sorting (numbers in numerical order)."""
    return [
        int(chunk) if chunk.isdigit() else chunk.lower()
        for chunk in re.split('([0-9]+)', value)
    ]


def NATURAL_SORT(info):
    return -info.is_dir, natural_sort_key(info.name)


def sort_nicely(names):
    """Sort the given list in the way that humans expect."""
    names.sort(key=natural_sort_key)


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
    width = view.settings().get('dired_width', [0.25, 0.5])
    if isinstance(width, float):
        pass
    elif isinstance(width, int):  # assume it is pixels
        vw, _ = view.viewport_extent()
        width = 1 - round((vw - width) / vw, 2)
        if width >= 1:
            width = 0.9
    else:
        if not isinstance(width, list) or len(width) != 2:
            sublime.error_message(
                "FileBrowser:\n\ndired_width must be set to a list of two floats."
            )
            width = [0.25, 0.5]
        width = dynamic_width(view, *sorted(width))
    width = max(0.1, min(1.0, width))
    return width


def dynamic_width(view, lo=0.25, hi=0.5):
    '''
    Compute a reasonable sidebar width for a dired view.

    If stats are shown, use the visual length of the last line (tabular
    content). Otherwise, assume a ~40 character content width. In both
    cases, use `em_width()` to convert characters to pixels and cap the
    result to at most half the window width.
    '''
    em_width = view.em_width()
    viewport_width, _ = view.viewport_extent()

    s = view.settings()
    show_stats = s.get('dired_show_stats', False)

    if show_stats and view.size():
        # Use the last line as a proxy for the widest tabular row.
        last_region = view.line(view.size())
        line_text = view.substr(last_region).rstrip('\n')
        line_len = len(line_text)
        # Cut the date away
        chars = line_len - 14
    else:
        # No stats: assume a compact listing of about 40 characters.
        chars = 40

    width = (chars * em_width) / viewport_width
    width = max(lo, min(hi, width))
    return width


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
        '''Update status-bar'''
        # if view is not focused, view.window() may be None
        window          = self.view.window() or sublime.active_window()
        path_in_project = any(folder == self.path[:-1] for folder in window.folders())
        settings        = self.view.settings()
        copied_items    = settings.get('dired_to_copy', [])
        cut_items       = settings.get('dired_to_move', [])
        marked_items    = settings.get('dired_marked_paths') or []

        # Derive hidden-files flag view settings if not set
        show_hidden = getattr(self, 'show_hidden', settings.get('dired_show_hidden_files', True))

        # Show contextual help hints in the status bar
        enabled = settings.get('dired_filter_enabled', True)
        has_flt = settings.get('dired_filter') or settings.get('dired_filter_extension')
        filter_active = enabled and has_flt
        help_segments = ['?: Help']
        if filter_active:
            help_segments.append('I: Disable Filter')
        if marked_items:
            help_segments.append('u: Unmark All')
        if copied_items or cut_items:
            help_segments.append('ctrl+z: Clear Clipboard')
        help_segment = "[{0}] ".format(', '.join(help_segments))

        def format_mark(items, name):
            if not items:
                return ''
            size = _total_size(items)
            segments = [str(len(items))]
            if size > 0:
                segments.append(format_size_short(size))
            return f', {name}({"; ".join(segments)})'

        def _total_size(paths):
            total = 0
            for p in paths:
                if isdir(p):
                    return 0
                start = time.monotonic()
                try:
                    total += os.path.getsize(p)
                except OSError:
                    continue
                else:
                    if time.monotonic() - start > SIZE_STAT_TIMEOUT:
                        return 0
            return total

        marked_info = format_mark(marked_items, 'marked')
        copied_info = format_mark(copied_items, 'copied')
        cut_info = format_mark(cut_items, 'cut')
        status = "{help}{root}Hidden: {hidden}{marked}{copied}{cut}".format(
            help=help_segment,
            root='Project root, ' if path_in_project else '',
            hidden='On' if show_hidden else 'Off',
            marked=marked_info,
            copied=copied_info,
            cut=cut_info
        )
        self.view.set_status("__FileBrowser__", status)

    def prepare_filelist(self, names, path, goto, indent, insert_at):
        '''
        Splice the corresponding absolute paths into `self.index` at the
        given `insert_at` row so row→path mapping stays correct.
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
        self.index = self.index[:insert_at] + index + self.index[insert_at:]
        items += files
        return items

    def is_hidden(self, filename, path, stat=None):
        if not path:  # special case for ThisPC
            return False
        tests = self.view.settings().get('dired_hidden_files_patterns', ['.*'])
        if isinstance(tests, str):
            tests = [tests]
        if any(fnmatch.fnmatch(filename, pattern) for pattern in tests):
            return True
        if not NT:
            return False

        try:
            attrs = stat.st_file_attributes
        except AttributeError:
            return False
        else:
            return bool(attrs & FILE_ATTRIBUTE_HIDDEN)

    def list_directory(self, path) -> tuple[list[str], str]:
        '''List entries in a directory or return an error.
        Respects hidden-file setting and returns sorted names.'''
        try:
            entries = self._our_scandir(path)
            items = [info.name for info in entries]
            error = ''
        except OSError as e:
            items = []
            error = self._format_os_error(e)
        return items, error

    def list_only_dirs(self, path) -> tuple[list[str], str]:
        '''List all directories (unfiltered by name/extension), respecting hidden setting.
        Used for prompt completion.'''
        items, error = self.list_directory(path)
        if items:
            items = [n for n in items if isdir(join(path, n))]
        return items, error

    def _format_os_error(self, error: OSError) -> str:
        message = str(error)
        if NT:
            message = (
                message
                .split(':')[0]
                .replace('[Error 5] ', 'Access denied')
                .replace('[Error 3] ', 'Not exists, press r to refresh')
            )
        return message

    def _filter_entries(self, entries, settings):
        enabled = settings.get('dired_filter_enabled', True)
        if enabled:
            if ext_filter := settings.get('dired_filter_extension', '').lower():
                entries = [e for e in entries if os.path.splitext(e.name)[1].lower() == ext_filter]
            if flt := settings.get('dired_filter'):
                entries = [e for e in entries if rx_fuzzy_match(flt, e.name)]
        return entries

    def _our_scandir(self, path: str, sortfunc=NATURAL_SORT) -> list[EntryInfo]:
        """Return EntryInfo objects for the given directory, respecting hidden settings."""
        show_hidden = getattr(self, 'show_hidden', self.view.settings().get('dired_show_hidden_files', True))

        if not path:
            entries = [
                EntryInfo(f"{drive}:", f"{drive}:", True, None, None)
                for drive in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                if isdir(f"{drive}:")
            ]
            entries.sort(key=lambda info: natural_sort_key(info.name))
            return entries

        items: list[EntryInfo] = []
        with os.scandir(path) as iterator:
            for entry in iterator:
                name = entry.name
                try:
                    stat_res = entry.stat(follow_symlinks=False)
                except OSError:
                    stat_res = None
                if not show_hidden and self.is_hidden(name, path, stat_res):
                    continue
                is_directory = entry.is_dir(follow_symlinks=False)
                size = stat_res.st_size if stat_res and not is_directory else None
                mtime = stat_res.st_mtime if stat_res else None
                full_path = entry.path
                if is_directory and not full_path.endswith(os.sep):
                    full_path += os.sep
                items.append(EntryInfo(name, full_path, is_directory, size, mtime))

        items.sort(key=sortfunc)
        return items

    def walkdir(
        self,
        dir_path: str,
        *,
        expanded: set[str] | None = None,
        auto_expand: int = 0,
        depth: int = 0,
        sortfunc=NATURAL_SORT
    ) -> tuple[list[ListingItem], str]:
        """Return a flattened tree of ListingItem objects for the given root."""
        expanded = expanded or set()
        target = dir_path.rstrip(os.sep) if dir_path else dir_path
        try:
            infos = self._our_scandir(target, sortfunc=sortfunc)
        except OSError as error:
            return [], self._format_os_error(error)

        entries: list[ListingItem] = []
        for info in infos:
            if info.is_dir and (depth < auto_expand or info.full_path in expanded):
                sub_entries, sub_err = self.walkdir(
                    info.full_path, expanded=expanded, auto_expand=auto_expand, depth=depth + 1)
                note = sub_err or ('empty' if not sub_entries else '')
                entries.append(ListingItem(*info, depth, True, note))
                entries.extend(sub_entries)
            else:
                entries.append(ListingItem(*info, depth, False, ""))

        return entries, ''

    def recreate_dired_expanded_paths_from_view(self):
        # Update persisted expanded paths based on current view state
        # Collect current expanded directory paths from visible arrows
        regions = self.view.find_all(r'^\s*▾')
        if regions:
            self.index = self.get_all()
            paths = []
            for r in regions:
                row = self.view.rowcol(r.a)[0]
                if 0 <= row < len(self.index):
                    paths.append(self.index[row])
            self.view.settings().set('dired_expanded_paths', paths)
        else:
            self.view.settings().set('dired_expanded_paths', [])

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
        fbs = self.view.find_by_selector
        self.view.sel().clear()

        def bof_():
            if header := fbs('text.dired header.dired'):
                return header[0].b
            return 0

        if sels:
            bof = bof_()
            eof = self.view.size()
            for s in sels:
                if bof < s.begin() <= eof:
                    self.view.sel().add(s)

        if not sels or not list(self.view.sel()):  # all sels more than eof
            # Prefer first real item when a live filter is active
            flt = self.view.settings().get('dired_filter')
            enabled = self.view.settings().get('dired_filter_enabled', True)
            filter_extension = self.view.settings().get('dired_filter_extension')
            if enabled and (flt or filter_extension):
                item = (
                    fbs('text.dired dired.item.file string.name.file.dired ')
                    or fbs('text.dired dired.item.directory string.name.directory.dired ')
                    or fbs('text.dired dired.item.parent_dir ')
                )
            else:
                item = (
                    fbs('text.dired dired.item.directory string.name.directory.dired ')
                    or fbs('text.dired dired.item.file string.name.file.dired ')
                    or fbs('text.dired dired.item.parent_dir ')
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

    # --- History management ----------------------------------------------------

    def history_push(self):
        s = self.view.settings()
        entry = self._build_history_entry()
        history = s.get('dired_history', [])
        cursor = s.get('dired_history_cursor', 0)
        history = history[:cursor] + [entry]
        cursor = len(history)
        s.set('dired_history', history)
        s.set('dired_history_cursor', cursor)
        return history, cursor


    def history_replace(self):
        s = self.view.settings()
        entry = self._build_history_entry()
        history = s.get('dired_history')
        cursor = s.get('dired_history_cursor')
        if history is None:
            raise RuntimeError("history_replace called before any `history` is recorded")
        if cursor is None:
            raise RuntimeError("history_replace called before `history_cursor` is initialized")

        history = history[:cursor] + [entry] + history[cursor + 1:]
        s.set('dired_history', history)
        return history


    def _build_history_entry(self):
        s = self.view.settings()
        self.index = self.get_all()  # hidden dependency
        return {
            'path': self.path,
            'expanded': s.get('dired_expanded_paths', []),
            'selection': self.get_selected(parent=False, full=False),
            'regions': [(r.a, r.b) for r in self.view.sel()],
            'viewport': self.view.viewport_position(),
        }

    def _apply_history_entry(self, entry):
        """Restore a saved history entry: path, expanded dirs, selections, viewport."""
        window = self.view.window()
        if not window:
            return   # view has been closed already

        from .show import show
        show(window, entry['path'], reuse_view=self.view, to_expand=entry['expanded'])
        self.restore_sels((entry['selection'], [Region(a, b) for a, b in entry['regions']]))
        self.view.set_viewport_position(entry['viewport'], False)

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
