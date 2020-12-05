" ====== init.vim by Rob Boudreau ======

"==================================================
" Call and/or install plugins with vim-plug
"==================================================
call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'joshdick/onedark.vim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'easymotion/vim-easymotion'
Plug 'godlygeek/tabular'
Plug 'vifm/vifm.vim'

call plug#end()

"==================================================
" Set global options
"==================================================
set encoding=utf-8
set expandtab
set tabstop=4
set shiftwidth=4
set history=50
set linebreak
set ignorecase smartcase
set wildmode=list:longest,full
set ttimeoutlen=50
set dir=~/Temp
set splitbelow
set splitright
set showtabline=2
set noswapfile
set number
set relativenumber
set inccommand=split
set nobackup
set nowritebackup
set noshowmode
set conceallevel=2
set concealcursor=nc
set foldcolumn=2
set fcs=eob:\
set spelllang=en_us
set spellfile=~/.config/nvim/spell/en.utf-8.add
set termguicolors
let mapleader = " "

"==================================================
" Set xdg-open to open links with gx
"==================================================
let g:netrw_browsex_viewer = "xdg-open"

"==================================================
" Key to format tables in markdown
"==================================================
nno <leader>t :TableFormat<cr>

"==================================================
" Settings for Vifm
"==================================================
let g:vifm_embed_split=1
nno <leader>v :Vifm<CR>

"==================================================
" Settings for FZF
"==================================================
let g:fzf_buffers_jump = 1
let g:fzf_layout = {'up':'~90%', 'window': { 'width': 0.8, 'height': 0.8,'yoffset':0.5,'xoffset': 0.5, 'border': 'sharp' } }

"==================================================
" --{{ Settings for Airline
"==================================================
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_theme = 'onedark'

" --}} End of Airline settings

" --{{ Set up colorschemes
"==================================================
" let g:neodark#background = '#2b303b'
let g:onedark_terminal_italics = 1
" let g:spacegray_use_italics = 1
" let g:spacegray_underline_search = 1
" let g:onedark_termcolors = 256
" let g:one_allow_italics = 1
" let g:nord_italic = 1
" let g:nord_italic_comments = 1

" set background=dark termguicolors
" colorscheme neodark
colorscheme onedark
" colorscheme spacegray
" colorscheme Tomorrow-Night
" colorscheme solarized8
" colorscheme materialbox
" colorscheme base16-ocean
" colorscheme nord
" colorscheme hybrid_material

" Make sure terminal background isn't overwritten
" hi! Normal ctermbg=NONE guibg='#232629'
hi Normal guibg=NONE ctermbg=NONE
" hi! NonText ctermbg=NONE guibg='#232629' guifg=NONE ctermfg=NONE
" --}} End of colorschemes

"==================================================
" --{{{ Start of key mappings }}}--
"==================================================

"==================================================
" FZF Keybindings
"==================================================
nno <silent><leader>o :Files<cr>
nno <silent><leader>a :FZF ~<cr>
nno <silent><leader>b :Buffers<cr>
nno <silent><leader>s :BLines<cr>
nno <silent><leader>f :Lines<cr>
nno <leader>g :Rg<cr>

"==================================================
" This opens markdown in okular
"==================================================
nno <silent><F9> :!okular %:p &<cr><cr>

"==================================================
" These are for auto-brackets
"==================================================
ino " ""<ESC>i
ino { {}<ESC>i
ino [ []<ESC>i
ino ( ()<ESC>i
nno ; :

"==================================================
" These keep searches centered in the page
"==================================================
nno n nzzzv
nno N Nzzzv

"==================================================
" These increase/decrease window split sizes
"==================================================
nno <A-left> <C-W>>
nno <A-right> <C-W><
nno <A-up> <C-W>+
nno <A-down> <C-W>-

"==================================================
" This turns off search highlighting
"==================================================
nno <silent><leader>h :noh<cr>

"==================================================
"Make moving back and forth in buffers easier
"==================================================
nno <silent><leader>l :bp<cr>
nno <silent><leader>r :bn<cr>

"==================================================
" Make moving back and forth in tabs easier
"==================================================
nno <silent><tab> :tabnext
nno <silent><S-tab> :tabprev

"==================================================
" This closes the currently viewed buffer and loads the last in the window
"==================================================
nno <silent><leader>c :bp\|bd #<CR>

"==================================================
" Insert today's date at the cursor
"==================================================
nno <F4> "=strftime("%a %d %b %Y")<cr>P
ino <F4> <C-R>=strftime("%a %d %b %Y")<cr>

"==================================================
" This makes unrecognized code files use shell syntax highlighting
"==================================================
nno <F5> :set syntax=sh<cr>

"==================================================
"Turn on and off spell checking
"==================================================
map <silent><F6> :set spell<cr>
map <silent><F7> :set nospell<cr>

"==================================================
"Makes moving throught windows more sane
"==================================================
nno <A-j> <C-w>j
nno <A-k> <C-w>k
nno <A-h> <C-w>h
nno <A-l> <C-w>l

"==================================================
"Make moving through lines normal, instead of jumping past wraps
"==================================================
nno <silent> <Up> gk
nno <silent> <Down> gj
nno <expr> k (v:count? 'k' : 'gk')
nno <expr> j (v:count? 'j' : 'gj')

"==================================================
" Move lines up or down using CTRL+arrow key
"==================================================
nno <C-down> ddp
nno <C-up> ddkP

"==================================================
" Copy and paste
"==================================================
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <ESC>"+pa

"==================================================
" Cleans up trailing whitespace from current edit
"==================================================
nno <leader>w :%s/\s\+$//<cr>:let @/=''<cr>

"==================================================
" Zoom a vim pane, <C-w>- to re-balance
"==================================================
nno <leader>z :wincmd =<cr>:wincmd \|<cr>
nno <leader>u :wincmd =<cr>

"==================================================
" Snippets
"==================================================

"==================================================
" Save a admin file from regular user
"==================================================
nno <silent><leader>x :w !sudo -S tee %

"==================================================
" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
"==================================================
ino <C-U> <C-G>u<C-U>

"==================================================
" }}}-- End of mappings --{{{
"==================================================

"==================================================
" Only do this part when compiled with support for autocommands.
"==================================================
if has("autocmd")
lua require'colorizer'.setup()

"==================================================
" Put these in an autocmd group, so that we can delete them easily.
"==================================================
augroup vimrcEx
au!

"==================================================
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
"==================================================
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

endif " has("autocmd")

"==================================================
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
"==================================================
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
  \ | wincmd p | diffthis
endif

"==================================================
" Put this at the end to enure it gets loaded last
"==================================================
highlight Comment cterm=italic
