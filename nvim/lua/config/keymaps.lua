-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set(
  "n",
  "<leader>sx",
  require("telescope.builtin").resume,
  { noremap = true, silent = true, desc = "Resume last search" }
)

vim.keymap.set("n", "<leader>fc", ":cd %:h<CR>", { noremap = true, silent = true, desc = "Switch to CWD" })

vim.keymap.set("n", "<leader>bc", ":ColorizerToggle<CR>", { noremap = true, silent = true, desc = "Toggle colors" })
