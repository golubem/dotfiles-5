

"""""""""""""""""""""""""""""" general


" set path
set path=.,,**

" enable syntax highlight
syn enable

" enable lines numeration
set number

" width of '>>' and '<<' shifts
set shiftwidth=4

" use spaces instead of tabs
set expandtab

" number of spaces used for tab
set tabstop=4

" tab at start of line will result in shift
set smarttab

" copy tab from previous line; add tab after '{' etc.
set smartindent

" show popup when using autocompletion in vim shell
set wildmenu

" completion popup
set completeopt=longest,menuone

" enable type-specific plugins loading
filetype plugin on

" make backspace work
set backspace=indent,eol,start

" make search case-insensitive
set ignorecase

" and make it case-sensitive when typing uppercase
set smartcase

" remove unused bars
set guioptions-=T
set guioptions-=m
set guioptions-=r
set guioptions-=L

" set font in gvim
set guifont=PragmataPro\ 10.5

" set colorscheme (and window size for gvim)
if has('gui_running')
    colorscheme Tomorrow-Night-Blue
    set lines=35 columns=120
else
    colorscheme elflord
endif

" remove unwanted whitespaces
autocmd BufWritePre * :%s/\s\+$//e

" highlight long lines
let w:m1=matchadd('Search', '\%<80v.\%>79v', -1)
let w:m2=matchadd('ErrorMsg', '\%>79v.\+', -1)

" auto :set paste
let &t_SI .= "\<Esc>[?2004h"
let &t_EI .= "\<Esc>[?2004l"
inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()
function! XTermPasteBegin()
  set pastetoggle=<Esc>[201~
  set paste
  return ""
endfunction

" keybindings
let mapleader = ","
nnoremap <A-j> 4jzz
nnoremap <A-k> 4kzz
nnoremap <Leader> :%s//<Left>
nnoremap <C-b> :BufExplorer<Return>
nnoremap <C-s> :w<Return>
vnoremap <Leader> :s//<Left>
inoremap kj <Esc>
inoremap <C-o> <C-x><C-o>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Return>
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l


"""""""""""""""""""""""""""""" plugins


" vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'bling/vim-airline'
Plugin 'https://github.com/scrooloose/nerdtree.git'
Plugin 'https://github.com/jlanzarotta/bufexplorer/'
Plugin 'https://github.com/vim-ruby/vim-ruby'
Plugin 'https://github.com/vim-scripts/OmniCppComplete'
call vundle#end()
filetype plugin indent on

" airline
if has('gui_running')
        let g:airline_theme='tomorrow'
        let g:airline#extensions#whitespace#enabled = 0
        let g:airline_enable_bufferline=1
        let g:airline_left_sep = ' '
        let g:airline_right_sep = ' '
        set laststatus=2
endif

" nerdtree
hi NERDTreeDir ctermfg=darkblue

" omnicppcomplete
set tags+=~/.vim/tags/cpp
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview


