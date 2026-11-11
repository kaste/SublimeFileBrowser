"""Clipboard lifetime regressions; all Win32 entrypoints are mocked."""

import ctypes
import importlib.util
import os
from unittest import TestCase, mock

from FileBrowser import common


class ClipboardReadTest(TestCase):
    def test_closes_before_path_classification_even_if_it_fails(self):
        clipboard = self.clipboard

        def unavailable_path(path):
            clipboard._user32.CloseClipboard.assert_called_once_with()
            raise RuntimeError('classification failed')

        fake_os = mock.Mock(sep=os.sep)
        fake_os.path.normpath.return_value = '/unavailable/file'
        fake_os.path.isdir.side_effect = unavailable_path
        with mock.patch.object(clipboard, '_read_drop_paths', return_value=['/unavailable/file']), \
                mock.patch.object(clipboard, '_read_drop_effect', return_value=1), \
                mock.patch.object(clipboard, 'os', fake_os):
            with self.assertRaisesRegex(RuntimeError, 'classification failed'):
                clipboard.get_file_clipboard()

        clipboard._user32.CloseClipboard.assert_called_once_with()

    def test_read_preserves_snapshot_and_guards_cleanup_after_clipboard_changes(self):
        clipboard = self.clipboard
        paths = ['/audit/folder', '/audit/file.txt']
        drop_effect = ctypes.wintypes.DWORD(clipboard.DROP_EFFECT_MOVE)
        clipboard._kernel32.GlobalSize.return_value = ctypes.sizeof(drop_effect)
        clipboard._kernel32.GlobalLock.return_value = ctypes.addressof(drop_effect)

        def query_path(handle, index, buffer, length):
            clipboard._user32.CloseClipboard.assert_not_called()
            if index == 0xFFFFFFFF:
                return len(paths)
            if buffer is not None:
                buffer.value = paths[index]
            return len(paths[index])

        def replace_clipboard():
            clipboard._user32.GetClipboardSequenceNumber.return_value = 8
            clipboard._user32.IsClipboardFormatAvailable.return_value = False
            drop_effect.value = clipboard.DROP_EFFECT_COPY

        def is_directory(path):
            clipboard._user32.CloseClipboard.assert_called_once_with()
            return path == os.path.normpath(paths[0])

        clipboard._shell32.DragQueryFileW.side_effect = query_path
        clipboard._user32.CloseClipboard.side_effect = replace_clipboard
        fake_os = mock.Mock(sep=os.sep)
        fake_os.path.normpath.side_effect = os.path.normpath
        fake_os.path.isdir.side_effect = is_directory
        with mock.patch.object(clipboard, 'os', fake_os):
            snapshot = clipboard.get_file_clipboard()

        self.assertEqual(snapshot, clipboard.FileClipboard(
            paths=[os.path.normpath(paths[0]) + os.sep, os.path.normpath(paths[1])],
            cut=True, sequence_number=7, is_ours=True,
        ))
        self.assertFalse(clipboard.clear_file_clipboard(snapshot.sequence_number))
        clipboard._user32.OpenClipboard.assert_called_once_with(None)
        clipboard._user32.EmptyClipboard.assert_not_called()
        clipboard._kernel32.GlobalUnlock.assert_called_once()

    def setUp(self):
        # Load an isolated copy, including on Linux: do not replace the live
        # module's native handles or register any Windows clipboard formats.
        path = os.path.join(os.path.dirname(common.__file__), 'lib', 'windows_clipboard.py')
        spec = importlib.util.spec_from_file_location('filebrowser_clipboard_test', path)
        self.clipboard = importlib.util.module_from_spec(spec)
        user32 = mock.Mock()
        user32.RegisterClipboardFormatW.side_effect = [0xC000, 0xC001]
        user32.GetClipboardSequenceNumber.return_value = 7
        with mock.patch.object(ctypes, 'WinDLL', create=True) as dll:
            dll.side_effect = lambda name, **kwargs: user32 if name == 'user32' else mock.Mock()
            spec.loader.exec_module(self.clipboard)
