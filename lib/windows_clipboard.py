"""Interop with the Windows shell file clipboard."""

from __future__ import annotations

import ctypes
import os
import time
from ctypes import wintypes
from typing import NamedTuple, Sequence


CF_UNICODETEXT = 13
CF_HDROP = 15
DROP_EFFECT_COPY = 1
DROP_EFFECT_MOVE = 2
GMEM_MOVEABLE = 0x0002
GMEM_ZEROINIT = 0x0040
_OPEN_ATTEMPTS = 10
_OPEN_RETRY_DELAY = 0.01


class FileClipboard(NamedTuple):
    paths: list[str]
    cut: bool
    sequence_number: int
    is_ours: bool


class ClipboardBusyError(OSError):
    pass


class DROPFILES(ctypes.Structure):
    _fields_ = [
        ('pFiles', wintypes.DWORD),
        ('pt', wintypes.POINT),
        ('fNC', wintypes.BOOL),
        ('fWide', wintypes.BOOL),
    ]


_user32 = ctypes.WinDLL('user32', use_last_error=True)
_kernel32 = ctypes.WinDLL('kernel32', use_last_error=True)
_shell32 = ctypes.WinDLL('shell32', use_last_error=True)

_user32.OpenClipboard.argtypes = [wintypes.HWND]
_user32.OpenClipboard.restype = wintypes.BOOL
_user32.CloseClipboard.argtypes = []
_user32.CloseClipboard.restype = wintypes.BOOL
_user32.EmptyClipboard.argtypes = []
_user32.EmptyClipboard.restype = wintypes.BOOL
_user32.IsClipboardFormatAvailable.argtypes = [wintypes.UINT]
_user32.IsClipboardFormatAvailable.restype = wintypes.BOOL
_user32.GetClipboardData.argtypes = [wintypes.UINT]
_user32.GetClipboardData.restype = wintypes.HANDLE
_user32.SetClipboardData.argtypes = [wintypes.UINT, wintypes.HANDLE]
_user32.SetClipboardData.restype = wintypes.HANDLE
_user32.RegisterClipboardFormatW.argtypes = [wintypes.LPCWSTR]
_user32.RegisterClipboardFormatW.restype = wintypes.UINT
_user32.GetClipboardSequenceNumber.argtypes = []
_user32.GetClipboardSequenceNumber.restype = wintypes.DWORD
_user32.GetForegroundWindow.argtypes = []
_user32.GetForegroundWindow.restype = wintypes.HWND

_kernel32.GlobalAlloc.argtypes = [wintypes.UINT, ctypes.c_size_t]
_kernel32.GlobalAlloc.restype = wintypes.HGLOBAL
_kernel32.GlobalFree.argtypes = [wintypes.HGLOBAL]
_kernel32.GlobalFree.restype = wintypes.HGLOBAL
_kernel32.GlobalLock.argtypes = [wintypes.HGLOBAL]
_kernel32.GlobalLock.restype = ctypes.c_void_p
_kernel32.GlobalUnlock.argtypes = [wintypes.HGLOBAL]
_kernel32.GlobalUnlock.restype = wintypes.BOOL
_kernel32.GlobalSize.argtypes = [wintypes.HGLOBAL]
_kernel32.GlobalSize.restype = ctypes.c_size_t

_shell32.DragQueryFileW.argtypes = [
    wintypes.HANDLE,
    wintypes.UINT,
    wintypes.LPWSTR,
    wintypes.UINT,
]
_shell32.DragQueryFileW.restype = wintypes.UINT

_PREFERRED_DROP_EFFECT = _user32.RegisterClipboardFormatW('Preferred DropEffect')
_last_written_sequence: int | None = None


def get_file_clipboard() -> FileClipboard | None:
    """Return shell file clipboard contents, or ``None`` for a text clipboard."""
    _open_clipboard()
    try:
        if not _user32.IsClipboardFormatAvailable(CF_HDROP):
            return None

        paths = _read_drop_paths(_clipboard_handle(CF_HDROP))
        if not paths:
            return None

        sequence_number = int(_user32.GetClipboardSequenceNumber())
        drop_effect = _read_drop_effect()
        return FileClipboard(
            paths=[_to_internal_path(path) for path in paths],
            cut=(
                bool(drop_effect & DROP_EFFECT_MOVE)
                and not bool(drop_effect & DROP_EFFECT_COPY)
            ),
            sequence_number=sequence_number,
            is_ours=sequence_number == _last_written_sequence,
        )
    finally:
        _user32.CloseClipboard()


def set_file_clipboard(paths: Sequence[str], cut: bool = False) -> None:
    """Publish paths for Explorer and as plain text for terminal pasting."""
    global _last_written_sequence

    shell_paths = list(dict.fromkeys(_to_shell_path(path) for path in paths))
    if not shell_paths:
        return

    handles = []
    transferred = 0
    opened = False
    try:
        handles.append((CF_HDROP, _drop_paths_handle(shell_paths)))
        handles.append(
            (CF_UNICODETEXT, _unicode_handle('\r\n'.join(shell_paths)))
        )
        if _PREFERRED_DROP_EFFECT:
            handles.append(
                (
                    _PREFERRED_DROP_EFFECT,
                    _bytes_handle(
                        wintypes.DWORD(
                            DROP_EFFECT_MOVE if cut else DROP_EFFECT_COPY
                        )
                    ),
                )
            )

        _open_clipboard(_user32.GetForegroundWindow())
        opened = True
        if not _user32.EmptyClipboard():
            raise ctypes.WinError(ctypes.get_last_error())
        for format_id, handle in handles:
            if not _user32.SetClipboardData(format_id, handle):
                raise ctypes.WinError(ctypes.get_last_error())
            transferred += 1
        _last_written_sequence = int(_user32.GetClipboardSequenceNumber())
    except Exception:
        for _, handle in handles[transferred:]:
            _kernel32.GlobalFree(handle)
        raise
    finally:
        if opened:
            _user32.CloseClipboard()


def clear_file_clipboard(sequence_number: int | None = None) -> bool:
    """Clear file data unless another application has replaced the clipboard."""
    global _last_written_sequence

    if (
        sequence_number is not None
        and int(_user32.GetClipboardSequenceNumber()) != sequence_number
    ):
        return False

    _open_clipboard()
    try:
        if (
            sequence_number is not None
            and int(_user32.GetClipboardSequenceNumber()) != sequence_number
        ):
            return False
        if not _user32.IsClipboardFormatAvailable(CF_HDROP):
            return False
        if not _user32.EmptyClipboard():
            raise ctypes.WinError(ctypes.get_last_error())
        _last_written_sequence = None
        return True
    finally:
        _user32.CloseClipboard()


def clear_our_file_clipboard() -> bool:
    """Clear file data only if FileBrowser still owns the current clipboard."""
    if _last_written_sequence is None:
        return False
    return clear_file_clipboard(_last_written_sequence)


def _open_clipboard(owner=None) -> None:
    for _ in range(_OPEN_ATTEMPTS):
        if _user32.OpenClipboard(owner):
            return
        time.sleep(_OPEN_RETRY_DELAY)
    raise ClipboardBusyError(
        ctypes.get_last_error(), 'The Windows clipboard is busy'
    )


def _clipboard_handle(format_id: int):
    handle = _user32.GetClipboardData(format_id)
    if not handle:
        raise ctypes.WinError(ctypes.get_last_error())
    return handle


def _read_drop_paths(handle) -> list[str]:
    count = _shell32.DragQueryFileW(handle, 0xFFFFFFFF, None, 0)
    paths = []
    for index in range(count):
        length = _shell32.DragQueryFileW(handle, index, None, 0)
        buffer = ctypes.create_unicode_buffer(length + 1)
        if _shell32.DragQueryFileW(handle, index, buffer, len(buffer)):
            paths.append(buffer.value)
    return paths


def _read_drop_effect() -> int:
    if (
        not _PREFERRED_DROP_EFFECT
        or not _user32.IsClipboardFormatAvailable(_PREFERRED_DROP_EFFECT)
    ):
        return DROP_EFFECT_COPY

    handle = _clipboard_handle(_PREFERRED_DROP_EFFECT)
    if _kernel32.GlobalSize(handle) < ctypes.sizeof(wintypes.DWORD):
        return DROP_EFFECT_COPY
    pointer = _kernel32.GlobalLock(handle)
    if not pointer:
        return DROP_EFFECT_COPY
    try:
        return int(ctypes.cast(pointer, ctypes.POINTER(wintypes.DWORD)).contents.value)
    finally:
        _kernel32.GlobalUnlock(handle)


def _to_internal_path(path: str) -> str:
    path = os.path.normpath(path)
    return path + os.sep if os.path.isdir(path) else path


def _to_shell_path(path: str) -> str:
    return os.path.normpath(path)


def _drop_paths_handle(paths: Sequence[str]):
    encoded_paths = ('\0'.join(paths) + '\0\0').encode('utf-16-le')
    header = DROPFILES(
        pFiles=ctypes.sizeof(DROPFILES),
        pt=wintypes.POINT(0, 0),
        fNC=False,
        fWide=True,
    )
    handle = _global_handle(ctypes.sizeof(header) + len(encoded_paths))
    pointer = _lock_handle(handle)
    try:
        ctypes.memmove(pointer, ctypes.byref(header), ctypes.sizeof(header))
        ctypes.memmove(
            pointer + ctypes.sizeof(header), encoded_paths, len(encoded_paths)
        )
    finally:
        _kernel32.GlobalUnlock(handle)
    return handle


def _unicode_handle(text: str):
    return _bytes_handle((text + '\0').encode('utf-16-le'))


def _bytes_handle(data) -> wintypes.HGLOBAL:
    if isinstance(data, bytes):
        size = len(data)
        source = data
    else:
        size = ctypes.sizeof(data)
        source = ctypes.byref(data)

    handle = _global_handle(size)
    pointer = _lock_handle(handle)
    try:
        ctypes.memmove(pointer, source, size)
    finally:
        _kernel32.GlobalUnlock(handle)
    return handle


def _global_handle(size: int):
    handle = _kernel32.GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, size)
    if not handle:
        raise MemoryError('Could not allocate Windows clipboard data')
    return handle


def _lock_handle(handle) -> int:
    pointer = _kernel32.GlobalLock(handle)
    if not pointer:
        _kernel32.GlobalFree(handle)
        raise ctypes.WinError(ctypes.get_last_error())
    return pointer
