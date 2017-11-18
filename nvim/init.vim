" init.vim by Rob Boudreau
" Last change:	2 Nov 2017

" Call and/or install plugins with vim-plug
call plug#begin('~/.config/nvim/plugged')

Plug 'https://github.com/ctrlpvim/ctrlp.vim.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/vim-airline/vim-airline.git'
Plug 'https://github.com/vim-airline/vim-airline-themes.git'
Plug 'https://github.com/lilydjwg/colorizer'
Plug 'https://github.com/tpope/vim-commentary.git'
Plug 'https://github.com/rakr/vim-one'
Plug 'https://github.com/Shougo/deoplete.nvim'
Plug 'https://github.com/eugen0329/vim-esearch'
" Plug 'https://github.com/edkolev/tmuxline.vim'

call plug#end()

" Set global options
" set t_ut=
set termguicolors
set encoding=utf-8
set expandtab
set tabstop=4
set shiftwidth=4
set history=50
set linebreak
set wildmode=list:longest,full
set ttimeoutlen=50
set dir=~/Temp
set splitbelow
set splitright
set noswapfile
set number
set relativenumber
set backupdir=~/Temp
set noshowmode

" Startup deoplete on init
let g:deoplete#enable_at_startup = 1

" --{{ Settings for vim-esearch
let g:esearch = {
  \ 'adapter':    'grep',
  \ 'backend':    'nvim',
  \ 'out':        'win',
  \ 'batch_size': 1000,
  \ 'use':        ['word_under_cursor'],
  \}
" --}} end of settings for vim-esearch

" This is supposed to allow italics in terminal
" set t_ZH=[3m
" set t_ZR=[23m

" Set up colorschemes
" let g:onedark_terminal_italics = 1
" let g:onedark_termcolors = 256
" let g:one_allow_italics = 1

set background=dark
" colorscheme onedark
" colorscheme OceanicNext
colorscheme hybrid_material
" colorscheme base16-ocean

" --{{{ Start of key mappings

" These are for auto-brackets
ino " ""<ESC>i
ino { {}<ESC>i
ino [ []<ESC>i
ino ( ()<ESC>i
nno ; :

" These keep searches centered in the page
nno n nzzzv
nno N Nzzzv

" These increase/decrease window split sizes
nno <c-left> 5<c-w>>
nno <c-right> 5<c-w><

" This turns off search highlighting
nno <silent><leader>\ :noh<cr>

"Make moving back and forth in buffers easier
nno <silent><leader>[ :bp<cr>
nno <silent><leader>] :bn<cr>

" This closes the currently viewed buffer and loads the last in the window
nno <silent><leader>c :bp\|bd #<CR>

" This makes unrecognized code files use shell syntax highlighting
nno <F5> :set syntax=sh<cr>

"Makes moving throught windows more sane
nno <C-j> <C-w>j
nno <C-k> <C-w>k
nno <C-h> <C-w>h
nno <C-l> <C-w>l

"Make moving through lines normal, instead of jumping past wraps
nno <silent> <Up> gk
nno <silent> <Down> gj
nno <silent> k gk
nno <silent> j gj

" Move lines up or down using CTRL+arrow key
nno <C-down> ddp
nno <C-up> ddkP

" Copy and paste
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <ESC>"+pa

" Cleans up trailing whitespace from current edit
nno <leader>w :%s/\s\+$//<cr>:let @/=''<cr>

" Control keys for CtrlP to open it in different modes
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlP'
nno <leader>b :CtrlPBuffer<cr>
nno <leader>p :CtrlPMRU<cr>

" zoom a vim pane, <C-w>- to re-balance
nno <leader>+ :wincmd =<cr>:wincmd \|<cr>
nno <leader>- :wincmd =<cr>

" Snippets
nno <leader>= o==================================================<cr><ESC>

" Settings for NerdTree so it's more sane
nno <silent><leader>n :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeHijackNetrw=1
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1
nno <silent><leader>e :edit .<cr>

" Save a admin file from regular user
nno <silent><leader>r :w !sudo tee %

" Don't use Ex mode, use Q for formatting
map Q gq

" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =

  " Set the file selection window to tree view
  " CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
  " so that you can undo CTRL-U after inserting a line break.
ino <C-U> <C-G>u<C-U>

" }}}-- End of mappings

" Only do this part when compiled with support for autocommands.
if has("autocmd")

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
au!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

endif " has("autocmd")

  " Convenient command to see the difference between the current buffer and the
  " file it was loaded from, thus the changes you made.
  " Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
  \ | wincmd p | diffthis
endif

" Set the Airline to use Powerline customized fonts for extra glyphs
let g:airline_powerline_fonts=1
" the separator used on the left side
" let g:airline_left_sep=''
" the separator used on the right side
" let g:airline_right_sep=''
" set the bufferline to something more useful
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_nr_show = 1
" let g:airline#extensions#tabline#show_splits = 0
" let g:airline#extensions#tabline#show_tab_nr = 1
" let g:airline#extensions#tabline#tab_nr_type = 2
" let g:airline_theme='onedark'
let g:airline_theme='hybrid'

  " Set the directory depth for CtrlP to open
let g:ctrlp_by_filename = 1
let g:ctrlp_mac_depth = 3
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden = 1
let g:ctrlp_switch_buffer = 'Et'
let g:ctrlp_working_path_mode = 'w'
highlight Comment cterm=italic