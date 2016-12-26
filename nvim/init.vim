" init.vim by Rob Boudreau
" Last change:	2016 Jun 26

" Call and/or install plugins with vim-plug
call plug#begin('~/.config/nvim/plugged')

Plug 'https://github.com/ctrlpvim/ctrlp.vim.git'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/edkolev/tmuxline.vim.git'
Plug 'https://github.com/vim-airline/vim-airline.git'
Plug 'https://github.com/vim-airline/vim-airline-themes.git'
Plug 'https://github.com/KabbAmine/vCoolor.vim.git'
Plug 'https://github.com/lilydjwg/colorizer'
Plug 'https://github.com/tpope/vim-commentary.git'
Plug 'https://github.com/rakr/vim-one'

call plug#end()

set expandtab   "use spaces instead of tabs
set tabstop=4
set shiftwidth=4
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set linebreak		" Always break right margin on whole word
set clipboard=unnamedplus
set wildmode=list:longest,full
set ttimeoutlen=50 " need this to keep Airline from hanging switching modes
set dir=~/Temp   " this sets where the swapfile is, to keep it out of other folders.
set splitbelow  "Sets new splits so they feel more natural to me
set splitright  "Same as above
set noswapfile

"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
    "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Set line numbers on by default and make them relative
set number
set relativenumber

" Set up colorscheme
let g:one_allow_italics = 1
set background=dark
" let g:onedark_terminal_italics = 1
colorscheme onedark

" When typing enclosures, automatically duplicate them
ino " ""<ESC>i
ino { {}<ESC>i
ino [ []<ESC>i
ino ( ()<ESC>i
nno ; :
nno n nzzzv
nno N Nzzzv
nno <silent><leader>\ :noh<cr>
nno <F5> :set syntax=sh<cr>

"Makes moving throught windows more sane
nno <C-j> <C-w>j
nno <C-k> <C-w>k
nno <C-h> <C-w>h
nno <C-l> <C-w>l
nno <leader>e :edit .<cr>

" Search through current project folder for highlighted text and display matches in split
nno <leader>s :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cw<CR>

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

" Quick keys for turning on and off the color picker
nno <silent><leader>v :VCoolor<cr>
ino <F6> :VCoolor<cr>

" Control keys for CtrlP to open it in different modes
let g:ctrlp_map = '<leader>f'
let g:ctrlp_cmd = 'CtrlP'
nno <leader>b :CtrlPBuffer<cr>
nno <leader>p :CtrlPMRU<cr>

" zoom a vim pane, <C-w>= to re-balance
nno <leader>- :wincmd =<cr>:wincmd \|<cr>
nno <leader>= :wincmd =<cr>

"  Settings for NerdTree so it's more sane
nno <silent><leader>n :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeHijackNetrw=1
nno <leader>e :edit .<cr>

" Don't use Ex mode, use Q for formatting
map Q gq

" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =

" Set the Netrw file selection window to tree view
" let g:netrw_liststyle=3

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
  \ | wincmd p | diffthis
endif

" Set the Airline to use Powerline customized fonts for extra glyphs
" let g:airline_powerline_fonts=1
" Use Powerline without the symbols
" the separator used on the left side
let g:airline_left_sep=''
" the separator used on the right side
let g:airline_right_sep=''
" and set the tabline to something more useful
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline#extensions#tabline#tab_nr_type = 2
" if !exists('g:airline_symbols')
"   let g:airline_symbols = {}
" endif
" let g:airline_symbols.space = "\ua0"
let g:airline_theme='onedark'

" Set the directory depth for CtrlP to open
let g:ctrlp_by_filename = 1
let g:ctrlp_mac_depth = 10
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden = 1
let g:ctrlp_switch_buffer = 'Et'
let g:ctrlp_working_path_mode = 'w'

" Use italics for comments
highlight Comment cterm=italic
