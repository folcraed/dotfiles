-- Install neogit plugin
return {
  {
    "TimUntersberger/neogit",
    dependencies = { "nvim-lua/plenary.nvim" },
    keys = {
      { "<leader>gn", "<cmd>Neogit<CR>", silent = true, desc = "Neogit" },
    },
    config = function()
      require("neogit").setup({
        disable_commit_confirmation = true,
      })
    end,
  },
}
