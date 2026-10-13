import os
import tempfile

from unittesting import DeferrableViewTestCase

from FileBrowser.prompt import dired_prompt, map_window_to_ctx, PromptContext, start


class DirectoryPromptTest(DeferrableViewTestCase):
    def test_resolves_relative_directory_before_calling_back(self):
        self.command.on_done('.')

        self.assertEqual(self.calls, [(os.path.abspath('.'), 'extra')])

    def test_preserves_absolute_directory(self):
        with tempfile.TemporaryDirectory() as root:
            self.command.on_done(root)

            self.assertEqual(self.calls, [(os.path.abspath(root), 'extra')])

    def test_starts_prompt_with_a_normalized_directory(self):
        with tempfile.TemporaryDirectory() as root:
            start('Directory:', self.window, root, self.command.ctx.callback, 'extra')
            yield lambda: self.window.active_panel() == 'input'

            ctx = map_window_to_ctx[self.window.id()]
            self.assertEqual(ctx.msg, 'Directory:')
            self.assertEqual(ctx.path, root + os.sep)
            self.assertEqual(ctx.args, ('extra',))
            self.assertEqual(self.calls, [])

    def test_reopens_prompt_for_missing_directory(self):
        self.assertNotEqual(self.window.active_panel(), 'input')
        with tempfile.TemporaryDirectory() as root:
            self.command.on_done(os.path.join(root, 'missing'))
            yield lambda: self.window.active_panel() == 'input'

            self.assertEqual(self.calls, [])
            self.assertIs(map_window_to_ctx[self.window.id()], self.command.ctx)

    def setUp(self):
        super().setUp()
        self.calls = []
        self.command = dired_prompt(self.window)
        self.command.ctx = PromptContext(
            'Directory:', '.', lambda *args: self.calls.append(args), 'extra'
        )
        previous_ctx = map_window_to_ctx.get(self.window.id())
        self.addCleanup(self.restore_context, previous_ctx)
        self.addCleanup(self.window.run_command, 'hide_panel', {'cancel': True})
        self.window.run_command('hide_panel', {'cancel': True})
        map_window_to_ctx[self.window.id()] = self.command.ctx

    def restore_context(self, previous_ctx):
        if previous_ctx is None:
            map_window_to_ctx.pop(self.window.id(), None)
        else:
            map_window_to_ctx[self.window.id()] = previous_ctx
