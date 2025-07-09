return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = { fish_lsp = { "fish-lsp", "start" } },
      filetypes = { "fish" },
    },
  },
}
