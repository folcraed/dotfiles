return {
  "epwalsh/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  lazy = true,
  -- ft = "markdown",
  -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
  event = {
    -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/**.md"
    "BufReadPre /home/rob/Dropbox/Notebook/**.md",
    "BufNewFile /home/rob/Dropbox/Notebook/**.md",
  },
  dependencies = {
    -- Required.
    "nvim-lua/plenary.nvim",

    -- see below for full list of optional dependencies 👇
  },
  opts = {
    workspaces = {
      {
        name = "Notebook",
        path = "~/Dropbox/Notebook/",
      },
    },

    -- see below for full list of options 👇
  },
  keys = {
    { "<leader>mn", "<cmd>ObsidianNew<cr>", desc = "Create a new note" },
    { "<leader>mq", "<cmd>ObsidianQuickSwitch<cr>", desc = "Switch to another note" },
    { "<leader>mb", "<cmd>ObsidianBackLinks<cr>", desc = "Show backlinks" },
    { "<leader>ms", "<cmd>ObsidianSearch<cr>", desc = "Search notes in vault" },
    { "<leader>ml", "<cmd>ObsidianLinks<cr>", desc = "Show links" },
  },
}
