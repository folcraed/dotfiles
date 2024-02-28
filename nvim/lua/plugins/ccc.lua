return {
  {
    "uga-rosa/ccc.nvim",
    lazy = true,
    config = function()
      require("ccc").setup()
    end,
    cmd = "CccHighlighterToggle",
    keys = { { "<leader>uh", "<cmd>CccHighlighterToggle<cr>", desc = "Toggle color highlight" } },
  },
}
