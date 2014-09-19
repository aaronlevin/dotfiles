set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/syntastic'
Plugin 'Shougo/neocomplete'
Plugin 'eagletmt/neco-ghc'
Plugin 'Shougo/vimshell.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'majutsushi/tagbar'
Plugin 'travitch/hasksyn'
Plugin 'lukerandall/haskellmode-vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'bitc/lushtags'
Plugin 'bitc/vim-hdevtools'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'derekwyatt/vim-scala'
Plugin 'kien/ctrlp.vim'
Plugin 'guns/vim-clojure-static'
Plugin 'plasticboy/vim-markdown'
Plugin 'krisajenkins/vim-pipe'

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
syntax enable                     " use syntax hilighting
syntax on
"if !has("gui_running")
"  colorscheme ir_black
"else
"  colorscheme ir_black
"endif

hi Search cterm=NONE ctermfg=none ctermbg=black

"set bg=dark                      " background is light?

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

" >>= Haskell
au BufEnter *.hs compiler ghc
let g:haddock_browser="/usr/bin/firefox"
let g:haskellmode_completion_ghc=0
let g:haskellmode_completion_haddock=0

" let g:syntastic_haskell_checkers = ['ghc-mod']

au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>

let g:ghcmodtypetoggle = 0

function! GhcModTypeToggle()
  if g:ghcmodtypetoggle == 0
    let g:ghcmodtypetoggle=1
    GhcModType
  else
    let g:ghcmodtypetoggle=0
    GhcModTypeClear
  endif
endfunction

function! s:find_basedir() "{{{
" search Cabal file
  if !exists('b:ghcmod_basedir')
    let l:ghcmod_basedir = expand('%:p:h')
    let l:dir = l:ghcmod_basedir
    for _ in range(6)
      if !empty(glob(l:dir . '/*.cabal', 0))
        let l:ghcmod_basedir = l:dir
        break
      endif
      let l:dir = fnamemodify(l:dir, ':h')
    endfor
    let b:ghcmod_basedir = l:ghcmod_basedir
  endif
  return b:ghcmod_basedir
endfunction "}}}

" use ghc functionality for haskell files
let sandbox_dir = '/.cabal-sandbox/x86_64-linux-ghc-7.8.3-packages.conf.d'
let g:ghc="/usr/bin/ghc"
augroup filetype_hs
    autocmd!
    "autocmd Bufenter *.hs let dir = s:find_basedir() . sandbox_dir
    autocmd Bufenter *.hs compiler ghc
    "autocmd Bufenter *.hs let b:ghc_staticoptions = '-package-db ' . dir
    "autocmd Bufenter *.hs let g:ghcmod_ghc_options = ['-package-db ' . dir]
    " autocmd Bufenter *.hs let g:syntastic_haskell_ghc_mod_args = '-g -package-db='.dir
    "autocmd Bufenter *.hs let g:hdevtools_options = '-g-ilib -g-isrc -g-i. -g-idist/build/autogen -g-Wall -g-package-db='.dir
augroup END

" Toggle between active and passive type checking
" to always show the error list, enable this flag:
"   let g:syntastic_auto_loc_list=1
map <silent> <Leader>e :Errors<CR>
map <Leader>s :SyntasticToggleMode<CR>

" ghc-mod settings
" Reload
"map <silent> tw :call GHC_BrowseAll()<CR>
map <silent> tw :GhcModType<CR>
" Type Lookup
"map <silent> tw :call GHC_ShowType(0)<CR>
map <silent> tu :GhcModTypeClear<CR>
map <silent>tt :call GhcModTypeToggle()<CR>

"##############################################################################
" Clojure
"##############################################################################

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

" map :TagbarOpen
"nmap <silent> <c-t> :TagbarOpen<CR>

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
map <leader>t <C-W>T


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

" Assume postgres
"let g:sql_type_default = 'pgsql'
au BufNewFile,BufRead *.sql setf pgsql

" For now, overthink's only use of vim-pipe is showing rendered markdown
let b:vimpipe_command="multimarkdown | lynx -dump -stdin"
