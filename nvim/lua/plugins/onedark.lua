return {
  {
    "olimorris/onedarkpro.nvim",
    lazy = true,
    config = function()
      require("onedarkpro").setup({
        options = {
          transparency = true,
        },
        styles = {
          comments = "italic",
          keywords = "italic",
          functions = "bold, italic",
          methods = "italic",
        },
      })
    end,
  },
}
