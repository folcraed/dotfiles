" ====== init.vim by Rob Boudreau ======

"==================================================
" Call and/or install plugins with vim-plug
"==================================================
call plug#begin('~/.config/nvim/plugged')

Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'itchyny/lightline.vim'
Plug 'taohexxx/lightline-buffer'
Plug 'joshdick/onedark.vim'
Plug 'lilydjwg/colorizer'
Plug 'plasticboy/vim-markdown'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'godlygeek/tabular'
Plug 'eugen0329/vim-esearch'
Plug 'majutsushi/tagbar'
Plug 'lvht/tagbar-markdown'
" Plug 'ryanoasis/vim-devicons'

call plug#end()

"==================================================
" Set global options
"==================================================
set termguicolors
set guifont=Iosevka:h12
set encoding=utf-8
set expandtab
set tabstop=4
set shiftwidth=4
set history=50
set linebreak
set ignorecase
set smartcase
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
set fcs=eob:\
set spelllang=en_us
set spellfile=~/.config/nvim/spell/en.utf-8.add
 
"==================================================
" Set xdg-open to open links with gx
"==================================================
let g:netrw_browsex_viewer = "xdg-open"

"==================================================
" Key to format tables in markdown
"==================================================
nno <leader>t :TableFormat<cr>

"==================================================
" Settings for Vim-Markdown
"==================================================
let g:vim_markdown_conceal = 1
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_follow_anchor = 1
let g:vim_markdown_autowrite = 1

"==================================================
" --{{ Settings for vim-esearch
" It uses <leader>ff to search
"==================================================
let g:esearch = {
  \ 'adapter':    'rg',
  \ 'backend':    'nvim',
  \ 'out':        'qflist',
  \ 'batch_size': 1000,
  \ 'use':        ['word_under_cursor'],
  \ 'default_mappings': 1,
  \}
" --}} end of settings for vim-esearch

"==================================================
" Settings for NerdTree so it's more sane
"==================================================
nno <silent><leader>n :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeHijackNetrw=1
let NERDTreeShowBookmarks=1
let NERDTreeShowHidden=1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
nno <silent><leader>e :edit .<cr>

"==================================================
" Settings CtrlP
"==================================================
let g:ctrlp_map = '<C-p>'
let g:ctrlp_cmd = 'CtrlP'
nno <leader>b :CtrlPBuffer<cr>
nno <leader>p :CtrlPMRU<cr>
let g:ctrlp_by_filename = 1
let g:ctrlp_mac_depth = 3
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden = 1
let g:ctrlp_switch_buffer = 'Et'
let g:ctrlp_working_path_mode = 'w'

"==================================================
" --{{ Settings for Lightline
"==================================================
let g:lightline = {
    \ 'colorscheme': 'one',
    \ 'tabline': {
    \   'left': [ [ 'bufferinfo' ],
    \             [ 'separator' ],
    \             [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
    \   'right': [ [ 'close' ], ],
    \ },
    \ 'component_expand': {
    \   'buffercurrent': 'lightline#buffer#buffercurrent',
    \   'bufferbefore': 'lightline#buffer#bufferbefore',
    \   'bufferafter': 'lightline#buffer#bufferafter',
    \ },
    \ 'component_type': {
    \   'buffercurrent': 'tabsel',
    \   'bufferbefore': 'raw',
    \   'bufferafter': 'raw',
    \ },
    \ 'component_function': {
    \   'bufferinfo': 'lightline#buffer#bufferinfo',
    \   'gitbranch': 'fugitive#head',
    \ },
    \ 'component': {
    \   'separator': '',
    \ },
    \ 'active': {
    \   'left': [ ['mode', 'paste'],
    \             ['gitbranch', 'readonly', 'filename', 'modified']]
    \ },
    \ }
" --}} End of Lightline settings

"==================================================
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

set background=dark
" colorscheme neodark
colorscheme onedark
" colorscheme spacegray
" colorscheme Tomorrow-Night
" colorscheme solarized8
" colorscheme materialbox
" colorscheme base16-ocean
" colorscheme nord
" colorscheme hybrid_material
" --}} End of colorschemes

"==================================================
" --{{{ Start of key mappings }}}--
"==================================================

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
nno <silent><leader>\ :noh<cr>

"==================================================
"Make moving back and forth in buffers easier
"==================================================
nno <silent><leader>[ :bp<cr>
nno <silent><leader>] :bn<cr>

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
nno <leader>+ :wincmd =<cr>:wincmd \|<cr>
nno <leader>- :wincmd =<cr>

"==================================================
" Session management. F2 loads the notes session, F3 saves it.
"==================================================
nno <F2> :source ~/Dropbox/Docs/Notes/Session.vim<CR>
nno <F3> :wa<Bar>exe "mksession! " . v:this_session<CR>

"==================================================
" Toogle the markdown outline view.
" and open it on the left side
"==================================================
nno <silent><F8> :TagbarToggle<cr>
ino <silent><F8> :TagbarToggle<cr>
let g:tagbar_left=0

"==================================================
" Snippets
"==================================================
nno <leader>= o==================================================<cr><ESC>

"==================================================
" Save a admin file from regular user
"==================================================
nno <silent><leader>r :w !sudo tee % .

"==================================================
" Don't use Ex mode, use Q for formatting
"==================================================
map Q gq

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
