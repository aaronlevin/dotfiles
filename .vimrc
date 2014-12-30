set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'Shougo/vimshell.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/neocomplete'
Plugin 'travitch/hasksyn'
Plugin 'flazz/vim-colorschemes'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'derekwyatt/vim-scala'
Plugin 'guns/vim-clojure-static'
Plugin 'plasticboy/vim-markdown'
Plugin 'krisajenkins/vim-pipe'
Plugin 'scrooloose/syntastic'
Plugin 'majutsushi/tagbar'
" Plugin 'kien/ctrlp.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


set modeline                      " this is off in Ubuntu by default; f that
set t_Co=256                      " terminal has 256 colours
set encoding=utf-8                " encoding is used for display purposes
scriptencoding utf-8
set noerrorbells                  " shut up!
set ignorecase                    " ignore case when searching.  Prefix search with \c to match case
set smartcase                     " When search contains a capital letter, become case sensitive
set backspace=indent,eol,start    " allow backspacing over everything in INS mode
set whichwrap=<,>,[,],h,l,b,s,~   " Make end/beginning-of-line cursor wrapping behave human-like, not vi-like
set wrap                          " use wrapping
"set showbreak=---------->         " but emphasize when it occurs
set autoindent
set history=1000                  " keep lots of history of commands
set expandtab                     " spaces, not tabs
set background=light
set swapfile
set dir=~/tmp
syntax enable                     " use syntax hilighting
syntax on

hi Search cterm=NONE ctermfg=none ctermbg=black

set ruler                  " always show cursor location in file
set showcmd                " show partially typed commands
set incsearch              " do incremental searching
set hls                    " highlight searches
set tabstop=2              " number of spaces that a <Tab> in the file counts for
set shiftwidth=2           " number of spaces to use for each step of autoindent
set hidden                 " allow switching buffers even if not saved
set showmatch              " match parentheses as you type them
set foldmethod=syntax
set foldlevel=100          " Don't autofold anything
set nolist                   " show normally hidden characters
hi SpecialKey guifg=darkgray  " make the listchars characters show up dark gray
set listchars=tab:→\ ,trail:·,extends:#,nbsp:.
set list
set wildmenu               " Wild!  This thing kicks ass.
set wildmode=longest,full  " First match only to the longest common string, then use full/wildmenu match
set wildignore=*.o,*.pyc,*.class
set laststatus=2           " Always show status bar
set statusline=%<%f\ %y[%{&ff}]%m%r%w%a\ %=%l/%L,%c%V\ %P  " cooler status line
set nosol                  " don't jump to the start of the line on a bunch of different movement commands
set complete=.,w,b,u,U,d,k,t  " Better auto completion, full tags last
"set guioptions-=t         " No tear-off menus
"set guioptions-=T         " No toolbar
"set guioptions-=m         " No top menu
set guioptions-=r         " No right scrollbar
set guioptions-=R         " No right scrollbar in splits
set guioptions-=l         " No left srollbar
set guioptions-=L         " No left srollbar in vertical splits
set guioptions-=b         " No bottom scrollbar
set guioptions-=e         " Use textmode tabs even in gvim
set guitablabel=\[%N\]\ %t\ %M " Display tab number and filename in tab
set grepprg=ack-grep\ --column
set grepformat=%f:%l:%c:%m
" set tags=./tags;/         
set tags=tags;/,codex.tags;/ " <-- searches parent dirs for tags files
set autochdir             " change working dir to be the location of the current file
let mapleader = ","
let maplocalleader = "\\"
autocmd BufNewFile,BufRead * setlocal formatoptions-=cro
set formatoptions+=l      " Don't break and auto-format long lines.
"set formatoptions-=t      " Don't autoformat shit
"set formatoptions-=cro

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Necomplecache
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:neocomplete#enable_at_startup = 1
let g:necoghc_debug = 1
let g:necoghc_enable_detailed_browse = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim-Airline plugin
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline#extensions#whitespace#enabled = 0

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntastic
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_auto_loc_list = 0
let g:syntastic_haskell_ghc_mod_exec = '/usr/bin/env ghc-mod'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" >>= Haskell
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:haddock_browser="/usr/bin/firefox"
let g:ghcmod_ghc_options = [ '-isrc', '-idist/build/autogen', '-fno-warn-missing-signatures',  '-fno-warn-orphans' ]
let g:ghc="/usr/bin/env ghc"

autocmd BufWritePost *.hs GhcModCheckAndLintAsync
au Filetype haskell setlocal tabstop=8 expandtab softtabstop=4 shiftwidth=4 shiftround

map <silent> tt :GhcModType<CR>
map <silent> tw :GhcModTypeClear<CR>

let g:tagbar_type_haskell = {
    \ 'ctagsbin'  : 'hasktags',
    \ 'ctagsargs' : '-x -c -o-',
    \ 'kinds'     : [
        \  'm:modules:0:1',
        \  'd:data: 0:1',
        \  'd_gadt: data gadt:0:1',
        \  't:type names:0:1',
        \  'nt:new types:0:1',
        \  'c:classes:0:1',
        \  'cons:constructors:1:1',
        \  'c_gadt:constructor gadt:1:1',
        \  'c_a:constructor accessors:1:1',
        \  'ft:function types:1:1',
        \  'fi:function implementations:0:1',
        \  'o:others:0:1'
    \ ],
    \ 'sro'        : '.',
    \ 'kind2scope' : {
        \ 'm' : 'module',
        \ 'c' : 'class',
        \ 'd' : 'data',
        \ 't' : 'type'
    \ },
    \ 'scope2kind' : {
        \ 'module' : 'm',
        \ 'class'  : 'c',
        \ 'data'   : 'd',
        \ 'type'   : 't'
    \ }
\ }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Alt-] to open a tag in a new split
map <A-]> :sp <CR>:exec("tag ".expand("<cword>"))<CR>

" set some mappings to easly cycle through buffers
noremap <C-Tab> :bnext<CR>
inoremap <C-Tab> <Esc>:bnext<CR>
cnoremap <C-Tab> :bnext<CR>

noremap <C-S-Tab> :bprev<CR>
inoremap <C-S-Tab> <Esc>:bprev<CR>
cnoremap <C-S-Tab> :bprev<CR>

" a handy mapping to fix tabs and kill trailing whitespace
map <F11> m`:retab<CR>:%s/\s\+$//eg<CR>``

" a mapping to refresh the syntax colouring easily -- this is really only
" useful when writing syntax files.
map <F12> :syn sync fromstart<CR>

" map :TagbarToggle
nmap <silent> <leader>t :TagbarToggle<CR>


"##############################################################################
" Easier split navigation
"##############################################################################

" Use ctrl-[hjkl] to select the active split! (http://www.vim.org/tips/tip.php?tip_id=173)
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

" faster splits and tabs
map <leader>v :vsplit<CR>
"map <leader>s :split<CR>
map <leader>c :close<CR>
" open current split in new tab
" map <leader>t <C-W>T


" Hit <s-CR> (used to use <CR>, but screws up use of quickfix window) to
" highlight the current word without moving the screen.  n/N works to jump
" between matches.
" http://vim.wikia.com/wiki/Highlight_all_search_pattern_matches
let g:highlighting = 0
function! Highlighting()
  if g:highlighting == 1 && @/ =~ '^\\<'.expand('<cword>').'\\>$'
    let g:highlighting = 0
    return ":silent nohlsearch\<CR>"
  endif
  let @/ = '\<'.expand('<cword>').'\>'
  let g:highlighting = 1
  return ":silent set hlsearch\<CR>"
endfunction
nnoremap <silent> <expr> <s-CR> Highlighting()

" Hit space to remove highlighting
nmap <Space> :noh<CR>

" Make Python follow PEP8 (http://www.python.org/dev/peps/pep-0008/)
au Filetype python setlocal ts=4 sw=4 sts=4 tw=79

au Filetype scala setlocal foldmethod=indent tw=80 formatoptions+=l

au Filetype mkd setlocal foldlevel=100

" Assume postgres
"let g:sql_type_default = 'pgsql'
au BufNewFile,BufRead *.sql setf pgsql

" For now, overthink's only use of vim-pipe is showing rendered markdown
let b:vimpipe_command="multimarkdown | lynx -dump -stdin"
