# FileBrowser model

This file captures the current data model and path conventions used by the
FileBrowser (dired) views.

## Path and view invariants

- Directory paths include a trailing `os.sep`.
- File paths do not include a trailing `os.sep`.
- `dired_path` is the view root and is stored with a trailing `os.sep` for
  normal directories.
- `dired_path` can be set to the sentinel `ThisPC\\` to represent the drive
  listing. `DiredBaseCommand.get_path()` normalizes that sentinel to the empty
  string (`''`).
- The empty string (`''`) is used internally to indicate the "This PC" root
  when listing drives on Windows.

## `dired_index`

`dired_index` is a list aligned 1:1 with the visible lines of the buffer.
It contains:

- Two empty strings for the header lines (when the header is enabled).
- `PARENT_SYM` for the parent link line (when the parent link is shown).
- Absolute paths for each visible entry.
- Directory entries use a trailing `os.sep`, file entries do not.

The index is the source of truth for visibility (e.g., "is this folder visible
in this view?").

## `dired_expanded_paths`

`dired_expanded_paths` stores the set of expanded directories for a view.

- Each entry is an absolute directory path with a trailing `os.sep`.
- It is persisted on the view and rebuilt from the buffer in
  `recreate_dired_expanded_paths_from_view()`.
- It is merged into refreshes to decide which directories should be expanded.
