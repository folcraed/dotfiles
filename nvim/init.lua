-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

vim.cmd [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]]

local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  use 'numToStr/Comment.nvim' -- "gc" to comment visual regions/lines
  use 'navarasu/onedark.nvim' -- Theme inspired by Atom
  use 'vifm/vifm.vim' -- Powerful file manager
  use 'norcalli/nvim-colorizer.lua' -- Shows hex colors in the color
  use 'kyazdani42/nvim-web-devicons' -- Icons for the status line
  use { 'nvim-lualine/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
}
  use { 'nvim-telescope/telescope.nvim', branch = '0.1.x',
         requires = (('nvim-lua/plenary.nvim'))
}
  use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make'}
end)

--**************************
--Options
--**************************
--Make line numbers default
vim.wo.number = true
vim.wo.relativenumber = true

--Use spaces instead of tabs
vim.o.expandtab = true

--Enable mouse mode
vim.o.mouse = 'a'

--Enable break indent and word wrapping
vim.o.breakindent = true
vim.o.linebreak = true

--Save undo history
vim.opt.undofile = true

--Sane splits
vim.o.splitbelow = true
vim.o.splitright = true

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

--Set spelling options
vim.o.spelllang = 'en_us'

--Remove the tildes at the end of file
vim.wo.fcs = 'eob: '

--Set the terminal type
vim.o.termguicolors = true

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

--Remap command key to something eaier
vim.api.nvim_set_keymap('n', ';', ':', { noremap = true })

--Set gx to open files with default app
vim.api.nvim_set_keymap('n', 'gx', ':!xdg-open <cfile><CR><ESC>', { noremap = true, silent = true })

--Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

--Setup some auto brackets
vim.api.nvim_set_keymap('i', '\"', '\"\"<ESC>i', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '{', '{}<ESC>i', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '[', '[]<ESC>i', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '(', '()<ESC>i', { noremap = true, silent = true })

--Keep searches centered in the page
vim.api.nvim_set_keymap('n', 'n', 'nzzzv', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'N', 'Nzzzv', { noremap = true, silent = true })

--Make Y yank from cursor to end of line
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true })

--Change directory to current file directory
vim.api.nvim_set_keymap('n', '<leader>cd', ':cd %:h<CR>', { noremap = true, silent = true })

--Copy and paste to system clipboard
vim.api.nvim_set_keymap('v', '<C-c>', '\"+yi<ESC>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-x>', '+c<ESC>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '<C-v>', 'c<ESC>\"+p', { noremap = true, silent = true })
vim.api.nvim_set_keymap('i', '<C-v>', '<ESC>\"+pa', { noremap = true, silent = true })

--Make window switching easier
vim.api.nvim_set_keymap('n', '<A-j>', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-k>', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-h>', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<A-l>', '<C-w>l', { noremap = true, silent = true })

--Insert today's date at cursor
vim.api.nvim_set_keymap('i', '<F4>', '<C-R>=strftime("%a %d %b %Y")<CR>', { noremap = true, silent = true })

--Turn on and off spell checking
vim.api.nvim_set_keymap('', '<F6>', ':set spell<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('', '<F7>', ':set nospell<CR>', { noremap = true, silent = true })

--Toggle Colorizer
vim.api.nvim_set_keymap('', '<F10>', ':ColorizerToggle<CR>', { noremap = true, silent = true })

--Move line shortcuts
vim.api.nvim_set_keymap('n', '<C-down>', ':m .+1<CR>==', { noremap = true})
vim.api.nvim_set_keymap('n', '<C-up>', ':m .-2<CR>==', { noremap = true})
vim.api.nvim_set_keymap('i', '<C-down>', '<Esc>:m .+1<CR>==gi', { noremap = true})
vim.api.nvim_set_keymap('i', '<C-up>', '<Esc>:m .-2<CR>==gi', { noremap = true})

--Resize window splits
vim.api.nvim_set_keymap('n', '<A-left>', '<C-W>>', { noremap = true})
vim.api.nvim_set_keymap('n', '<A-right>', '<C-W><', { noremap = true})
vim.api.nvim_set_keymap('n', '<A-up>', '<C-W>+', { noremap = true})
vim.api.nvim_set_keymap('n', '<A-down>', '<C-W>-', { noremap = true})

--Map blankline
vim.g.indent_blankline_char = '┊'
vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile' }
vim.g.indent_blankline_show_trailing_blankline_indent = false

-- Enable Comment.nvim
require('Comment').setup()

-- Settings for vifm
vim.g.vifm_embed_split = 1
vim.g.vifm_embed_cwd = 1
vim.api.nvim_set_keymap('n', '<leader>vn', [[<cmd>Vifm<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>vv', [[<cmd>VsplitVifm<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>vs', [[<cmd>SplitVifm<CR>]], { noremap = true, silent = true })

-- Setup Telescope
require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')

-- Keys for Telescope
vim.api.nvim_set_keymap('n', '<leader>ff', [[<cmd>Telescope find_files<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fg', [[<cmd>Telescope live_grep<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fb', [[<cmd>Telescope buffers<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fh', [[<cmd>Telescope help_tags<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fs', [[<cmd>Telescope current_buffer_fuzzy_find<CR>]], { noremap = true, silent = true })

-- Highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]

-- Colorize hex codes
require 'colorizer'.setup()

--Set colorscheme (order is important here)
require('onedark').setup({
        style = 'dark',
        transparent = true,
})

require('onedark').load()

--Set statusbar
require 'lualine'.setup({
        options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = { left = ' ', right = ' ' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = {},
        always_divide_middle = true,
        globalstatus = true,
      },
      sections = {
        lualine_a = { 'mode' },
        lualine_b = {{ 'branch', icon = '' },{ 'diff' }},
        lualine_c = { 'filename' },
        lualine_x = { 'filetype' },
        lualine_y = { 'progress' },
        lualine_z = { 'location' }
      },
      tabline = {},
      extensions = {}
})
