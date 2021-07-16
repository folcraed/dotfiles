" ====== init.vim by folcred ======

"==================================================
" Call and/or install plugins with vim-plug
"==================================================
call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline'
Plug 'joshdick/onedark.vim'
Plug 'norcalli/nvim-colorizer.lua'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'easymotion/vim-easymotion'
Plug 'godlygeek/tabular'
Plug 'vifm/vifm.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

"==================================================
" Set global options
"==================================================
set expandtab
set tabstop=2
set shiftwidth=2
set history=500
set linebreak
set ignorecase smartcase
set wildmode=list:longest,full
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
set mouse=a
set foldmethod=manual
set foldcolumn=2
set fcs=eob:\
set spelllang=en_us
set spellfile=~/.config/nvim/spell/en.utf-8.add
set complete+=kspell
set completeopt=menuone,longest
set shortmess+=c
set pumheight=12
set termguicolors
let mapleader = " "

"==================================================
" Set xdg-open to open links with gx
"==================================================
let g:netrw_browsex_viewer = "xdg-open"

"==================================================
" Settings for Vifm
"==================================================
let g:vifm_embed_split=1
let g:vifm_embed_cwd=1
nno <silent><leader>vn :Vifm<CR>
nno <silent><leader>vv :VsplitVifm<CR>
nno <silent><leader>vs :SplitVifm<CR>

"==================================================
" --{{ Settings for Airline
"==================================================
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''
let g:airline#extensions#tabline#right_sep = ''
let g:airline#extensions#tabline#right_alt_sep = ''
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_powerline_fonts = 1
let g:airline_symbols.branch = ''
let g:airline_symbols.dirty = ' '
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''

" --}} End of Airline settings

"==================================================
" Settings for CoC
"==================================================
" Use Tab key to trigger completion
let g:coc_node_path='/bin/node'
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
        \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gn <Plug>(coc-rename)
nmap <leader>gl :CocList<cr>

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" Formatting selected code.
xmap <leader>F <Plug>(coc-format-selected)
nmap <leader>F <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType javascript,typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

"==================================================
" Color Scheme
"==================================================
let g:onedark_terminal_italics = 1
colorscheme onedark
let g:airline_theme = 'onedark'
hi NORMAL guibg=NONE ctermbg=NONE

"==================================================
" --{{{ Start of key mappings }}}--
"==================================================

"==================================================
" FZF Keybindings
"==================================================
nno <silent><leader>f :Files<cr>
nno <silent><leader>k :Maps<cr>
nno <silent><leader>b :Buffers<cr>
nno <silent><leader>/ :BLines<cr>
nno <silent><leader>m :Marks<cr>
nno <leader>s :Rg<cr>

"==================================================
" Markdown settings
"==================================================
nno <silent><F9> :!okular %:p &<cr><cr>
nno <silent><leader>t :Tabularize /\|<cr>

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
nno <silent><leader>e :noh<cr>

"==================================================
"Make moving back and forth in buffers easier
"==================================================
nno <silent><leader>h :bp<cr>
nno <silent><leader>l :bn<cr>

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
vmap <C-c> "+yi<ESC>
vmap <C-x> "+c<ESC>
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
" if has("autocmd")
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
