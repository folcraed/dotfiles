return {
  {
    "uga-rosa/ccc.nvim",
    lazy = true,
    config = function()
      require("ccc").setup()
    end,
    keys = { { "<leader>ut", "<cmd>CccHighlighterToggle<cr>", desc = "Toggle color highlight" } },
  },
}
