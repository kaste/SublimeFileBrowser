import os

import sublime
import sublime_plugin


class SublimeFileBrowserFixUpPaths(sublime_plugin.TextCommand):
    '''
    usage: open console and run:
        view.run_command('sublime_file_browser_fix_up_paths')
    purpose: ensure that all paths end with os.sep
    '''

    def run(self, edit):
        for window in sublime.windows():
            for view in window.views():
                if view.settings():
                    path = view.settings().get("dired_path")
                    view.settings().set("dired_path", ensure_trailing_sep(path))

        settings = sublime.load_settings('dired.sublime-settings')
        jp = settings.get('dired_jump_points', {})
        if jp:
            fix_jp = {name: ensure_trailing_sep(path) for name, path in jp.items()}
            settings.set('dired_jump_points', fix_jp)
            sublime.save_settings('dired.sublime-settings')

        print('\nFileBrowser:\n\tAll fixed. Thank you for patience!\n')


def ensure_trailing_sep(s):
    return s if s.endswith(os.sep) else (s + os.sep)
