# coding: utf8
import sublime
from sublime import status_message, ok_cancel_dialog, load_settings, save_settings, Region
from sublime_plugin import TextCommand
import os
from os.path import isdir, basename

from .common import DiredBaseCommand
from .show import show
from .show import set_proper_scheme
SYNTAX_EXTENSION = '.sublime-syntax'


def load_jump_points():
    return load_settings('dired.sublime-settings').get('dired_jump_points', {})


def save_jump_points(points, reverse=False):
    if reverse:
        points = dict((n, t) for t, n in points.items())
    load_settings('dired.sublime-settings').set('dired_jump_points', points)
    save_settings('dired.sublime-settings')


def jump_points():
    sorted_jp = sorted(load_jump_points().items(), key=lambda x: x[0].lower())
    return sorted_jp


def jump_targets():
    return load_jump_points()


def jump_names():
    return {t: n for n, t in load_jump_points().items()}


class DiredJumpCommand(TextCommand, DiredBaseCommand):
    def run(self, edit, new_window=False):
        jp = jump_points()
        if not jp:
            status_message("No jump points available. To create jump point for this directory use 'P'.")
            return
        # show_quick_panel didn't work with dict_items
        self.new_window = new_window
        self.jump_points = [[n, t] for n, t in jp]
        self.display_jump_points = []
        longest_name = max([len(n) for n in jump_names().values()])
        for n, t in jp:
            offset = ' ' * (longest_name - len(n) + 1)
            path = self.display_path(t)
            name = '%s%s%s' % (n, offset, path.rstrip(os.sep))
            self.display_jump_points.append(name)
        self.view.window().show_quick_panel(
            self.display_jump_points,
            self.on_pick_point,
            sublime.MONOSPACE_FONT
        )

    def on_pick_point(self, index):
        if index == -1:
            return
        name, target = self.jump_points[index]
        if isdir(target):
            settings = load_settings('dired.sublime-settings')
            smart_jump = settings.get('dired_smart_jump', False)
            auto = self.new_window == 'auto'
            if self.new_window is True or ((not smart_jump) and auto) or (smart_jump and auto and len(self.view.window().views()) > 0):
                self.view.run_command("dired_open_in_new_window", {"project_folder": [target]})
            else:
                show(self.view.window(), target, view_id=self.view.id())
                status_message("Jumping to point '{0}' complete".format(name))
        else:
            # workaround ST3 bug https://github.com/SublimeText/Issues/issues/39
            self.view.window().run_command('hide_overlay')
            msg = "Can't jump to '{0} → {1}'.\n\nRemove that jump point?".format(name, target)
            if ok_cancel_dialog(msg):
                points = load_jump_points()
                del points[name]
                save_jump_points(points)
                status_message("Jump point '{0}' was removed".format(name))
                self.view.run_command('dired_refresh')


class DiredEditJumpPointCommand(TextCommand, DiredBaseCommand):
    def run(self, edit, item=False):
        self.names = jump_names()
        self.project_path = item and item[1] or self.path
        self.item = item
        name = item and item[0] or self.names.get(self.project_path)
        if not name:
            prompt = 'Create jump point:'
            name = basename(self.project_path)
        else:
            prompt = 'Edit jump point (clear to Remove):'
        self.view.window().show_input_panel(prompt, name, self.edit_jump_point, None, None)

    def edit_jump_point(self, name):
        if name:  # edit or create jump point
            for t, n in self.names.items():
                if n == name:
                    msg = "The jump point with name '{0}' is already exists ({1})\n\n".format(n, t)
                    msg += "Do you want to overwrite it?"
                    if ok_cancel_dialog(msg):
                        del self.names[t]
                    else:
                        return
            self.names[self.project_path] = name
            status_message("Jump point for this directory was set to '{0}'".format(name))
        elif self.project_path in self.names:
            del self.names[self.project_path]
            status_message("Jump point for this directory was removed")
        else:
            status_message("Jump point wasn't created")
            return
        save_jump_points(self.names, reverse=True)
        if self.item:
            self.view.run_command('dired_jump_list', {"reuse": True})
        else:
            self.view.run_command('dired_refresh')


class DiredJumpListRenderCommand(TextCommand):
    def run(self, edit):
        self.view.erase(edit, Region(0, self.view.size()))
        self.view.insert(edit, 0, self.render())
        self.view.sel().clear()
        pt = self.view.text_point(3, 0)
        self.view.sel().clear()
        self.view.sel().add(Region(pt, pt))
        self.view.set_read_only(True)

    def render(self):
        self.view_width = 79
        self.col_padding = 2
        self.jump_points = [[n, t] for n, t in jump_points()]
        self.names = [n for n, t in jump_points()]
        content = "Jump to…\n" + "—" * self.view_width + "\n\n"

        if len(self.names) > 0:
            self.max_len_names = max([len(n) for n, _ in jump_points()])
            self.view.settings().set('dired_project_count', len(self.names))
        else:
            content += "Jump list is empty!\n\nAdd a folder to your jump list by pressing P (shift + p)\nwhile you are browsing that folder in FileBrowser"

        for p in self.jump_points:
            content += '★ {0}→{1}\n'.format(self.display_name(p[0]), self.display_path(p[1]))
        return content

    def display_name(self, name):
        return name + " " * (self.max_len_names - len(name) + self.col_padding)

    def display_path(self, folder):
        display = folder.rstrip(os.sep)
        home = os.path.expanduser("~")
        label_characters = self.view_width - 4 - (self.col_padding*2) - self.max_len_names
        if folder.startswith(home):
            display = folder.replace(home, "~", 1)
        if len(display) > label_characters:
            chars = int(label_characters/2)
            display = display[:chars] + "…" + display[-chars:]
        return " " * self.col_padding + display


class DiredJumpListCommand(TextCommand):
    def run(self, edit, reuse=False):
        if reuse:
            view = self.view
            view.set_read_only(False)
        else:
            view = self.view.window().new_file()
            view.settings().add_on_change('color_scheme', lambda: set_proper_scheme(view))

        view.set_name("FileBrowser: Jump List")
        view.set_scratch(True)
        view.set_syntax_file('Packages/FileBrowser/dired_jumplist' + SYNTAX_EXTENSION)
        view.settings().set('line_numbers', False)
        view.settings().set('draw_centered', True)
        view.run_command('dired_jump_list_render')
        sublime.active_window().focus_view(view)


class DiredProjectNextLineCommand(TextCommand, DiredBaseCommand):
    def run(self, edit, forward=None):
        assert forward in (True, False), 'forward must be set to True or False'

        count = self.view.settings().get('dired_project_count', 0)
        files = Region(self.view.text_point(3, 0), self.view.text_point(count + 3 - 1, 0))

        if files.empty():
            return

        pt = self.view.sel()[0].a
        line = self.next_line(forward, pt, files)

        self.view.sel().clear()
        self.view.sel().add(Region(line.a, line.a))


class DiredProjectSelectCommand(TextCommand):
    def run(self, edit):
        pt = self.view.sel()[0].a
        row, col = self.view.rowcol(pt)
        points = [[n, t] for n, t in jump_points()]
        current_project = [points[row - 3][1]]
        settings = load_settings('dired.sublime-settings')
        smart_jump = settings.get('dired_smart_jump', False)
        if smart_jump and len(self.view.window().views()) == 1:
            show(self.view.window(), current_project[0])
        else:
            self.view.run_command("dired_open_in_new_window", {"project_folder": current_project})

        sublime.set_timeout(self.view.close, 100)


class DiredProjectEditJumpPointCommand(TextCommand):
    def run(self, edit):
        pt = self.view.sel()[0].a
        row, _ = self.view.rowcol(pt)
        points = [[n, t] for n, t in jump_points()]
        current_project = points[row - 3]
        self.view.run_command("dired_edit_jump_point", {"item": current_project})
