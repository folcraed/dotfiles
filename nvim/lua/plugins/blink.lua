-- I find using the tab key easier than using return
-- Many times I want to add a newline, not do a completion
-- and having it complete something is annoying to me.

return {
  {
    "saghen/blink.cmp",
    opts = {
      keymap = {
        preset = "super-tab",
      },
    },
  },
}
