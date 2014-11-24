""" ~/.vimrc file

""" general

" main
set path=.,,**
syn enable
set number
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set smartindent
set wildmenu
set completeopt=longest,menuone
filetype plugin on

" gui options
set guioptions-=T
set guioptions-=m
set guioptions-=r
set guioptions-=L
set guifont=PragmataPro\ 10

" colorsheme
if has('gui_running')
        colorscheme Tomorrow-Night
else
        colorscheme delek
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

""" plugins

" vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'bling/vim-airline'
Plugin 'https://github.com/scrooloose/nerdtree.git'
Plugin 'https://github.com/jlanzarotta/bufexplorer/'
Plugin 'https://github.com/vim-ruby/vim-ruby'
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

" vim-ruby
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1

" omnicppcomplete
set tags+=~/.vim/tags/cpp
let OmniCpp_NamespaceSearch = 1
let OmniCpp_GlobalScopeSearch = 1
let OmniCpp_ShowAccess = 1
let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
let OmniCpp_MayCompleteDot = 1 " autocomplete after .
let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
set completeopt=menuone,menu,longest,preview
