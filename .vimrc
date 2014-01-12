set nocompatible                  " this enables lots of good stuff
filetype off                      " required by Vundle

" Use Vundle to manage plugins: https://github.com/gmarik/vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" Vundle needs to manage itself
Bundle 'gmarik/vundle'

Bundle 'scrooloose/nerdtree'

Bundle 'derekwyatt/vim-scala'
Bundle 'plasticboy/vim-markdown'
Bundle 'majutsushi/tagbar'
Bundle 'tsaleh/vim-matchit'

Bundle 'tpope/vim-fireplace'
Bundle 'overthink/vim-classpath'
Bundle 'guns/vim-clojure-static'

Bundle 'Lokaltog/vim-powerline'
Bundle 'tpope/vim-fugitive'

Bundle 'ervandew/supertab'

Bundle 'flazz/vim-colorschemes'
Bundle 'krisajenkins/vim-pipe'
Bundle 'wting/rust.vim'
Bundle 'exu/pgsql.vim'

Bundle 'Shougo/vimproc'
Bundle 'git://github.com/travitch/hasksyn.git'
"Bundle 'git://github.com/laurilehmijoki/haskellmode-vim.git'
Bundle 'eagletmt/ghcmod-vim'


filetype plugin indent on

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
syntax enable                     " use syntax hilighting
syntax on
if !has("gui_running")
  colorscheme ir_black
else
  colorscheme ir_black
endif
set bg=light                      " background is light?

"set guifont=Envy\ Code\ R:11:cDEFAULT,ProFontWindows:h10:cANSI,Lucida_Console:h10:cANSI,Courier_New:h10:cANSI
"set guifont=Envy\ Code\ R
"set guifont=ProFont\ 11
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
set guioptions-=t         " No tear-off menus
set guioptions-=T         " No toolbar
set guioptions-=m         " No top menu
set guioptions-=r         " No right scrollbar
set guioptions-=R         " No right scrollbar in splits
set guioptions-=l         " No left srollbar
set guioptions-=L         " No left srollbar in vertical splits
set guioptions-=b         " No bottom scrollbar
set guioptions-=e         " Use textmode tabs even in gvim
set guitablabel=\[%N\]\ %t\ %M " Display tab number and filename in tab
set grepprg=ack-grep\ --column
set grepformat=%f:%l:%c:%m
set tags=./tags;/         " tags=.tags;/ <-- searches parent dirs for tags files
set autochdir             " change working dir to be the location of the current file
let mapleader = ","
let maplocalleader = "\\"
set formatoptions+=l      " Don't break and auto-format long lines.
set formatoptions-=t      " Don't autoformat shit

"##############################################################################
" Mappings
"##############################################################################

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
map <leader>s :split<CR>
map <leader>c :close<CR>
" open current split in new tab
map <leader>t <C-W>T

" Stuff stolen from vim-sensible: https://github.com/tpope/vim-sensible
set viminfo^=!

" Dirs not created automatically: mkdir -p ~/.local/share/vim/{swap,backup,undo}
let s:dir = has('win32') ? '~/Application Data/Vim' : has('mac') ? '~/Library/Vim' : '~/.local/share/vim'
if isdirectory(expand(s:dir))
  if &directory =~# '^\.,'
    let &directory = expand(s:dir) . '/swap//,' . &directory
  endif
  if &backupdir =~# '^\.,'
    let &backupdir = expand(s:dir) . '/backup//,' . &backupdir
  endif
  if exists('+undodir') && &undodir =~# '^\.\%(,\|$\)'
    let &undodir = expand(s:dir) . '/undo//,' . &undodir
  endif
endif
if exists('+undofile')
  set undofile
endif

" Tagbar
nmap <F8> :TagbarToggle<CR>
let g:tagbar_type_scala = {
    \ 'ctagstype' : 'Scala',
    \ 'kinds'     : [
        \ 'p:packages:1',
        \ 'V:values',
        \ 'v:variables',
        \ 'T:types',
        \ 't:traits',
        \ 'o:objects',
        \ 'a:aclasses',
        \ 'c:classes',
        \ 'r:cclasses',
        \ 'm:methods'
    \ ]
\ }

"##############################################################################
" Functions
"
"   syntax: "function!" causes a function to be replaced if it exists already
"##############################################################################

" Ripped from https://github.com/mkitt/tabline.vim
function! Tabline()
  let s = ''
  for i in range(tabpagenr('$'))
    let tab = i + 1
    let winnr = tabpagewinnr(tab)
    let buflist = tabpagebuflist(tab)
    let bufnr = buflist[winnr - 1]
    let bufname = bufname(bufnr)
    let bufmodified = getbufvar(bufnr, "&mod")

    let s .= '%' . tab . 'T'
    let s .= (tab == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= ' ' . tab .':'
    let s .= (bufname != '' ? '['. fnamemodify(bufname, ':t') . '] ' : '[No Name] ')

    if bufmodified
      let s .= '[+] '
    endif
  endfor

  let s .= '%#TabLineFill#'
  return s
endfunction
set tabline=%!Tabline()

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

" Haskell
au Filetype haskell setlocal tabstop=8 expandtab softtabstop=4 shiftwidth=4 smarttab shiftround nojoinspaces
"au BufEnter *.hs compiler ghc
let g:haddock_browser = "/user/bin/chromium"

" Run a command on the current file and put result in a new buffer in a new
" split:
" :new | r ! hg annotate -ud #

" For now, my only use of vim-pipe is showing rendered markdown
let b:vimpipe_command="multimarkdown | lynx -dump -stdin"

" SuperTab config
let g:SuperTabDefaultCompletionType = "context"
let g:SuperTabContextDefaultCompletionType = "<C-x><C-o>"
let g:SuperTabClosePreviewOnPopupClose = 1

au FileType *
  \ if &omnifunc != '' |
  \   call SuperTabChain(&omnifunc, "<c-x><c-n>") |
  \   call SuperTabSetDefaultCompletionType("<c-x><c-o>") |
  \ endif

" vim-fireplace clojure mappings
au FileType clojure map <localleader>q :w<CR>:Require!<CR>
" reset: http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded
au FileType clojure map <localleader>r :w<CR>:Eval (user/reset)<CR>
au FileType clojure map <localleader>t :w<CR>:Require<CR>:Eval (user/test)<CR>
au FileType clojure map <localleader>T :w<CR>:Require<CR>:Eval (user/test-all)<CR>

" Assume postgres
"let g:sql_type_default = 'pgsql'
au BufNewFile,BufRead *.sql setf pgsql
