""" ~/.vimrc file

""" general

" main
set number
set tabstop=4
set shiftwidth=4
set expandtab
set smarttab
set smartindent
set wildmenu
syn enable
let c_comment_strings=1
set guifont=PragmataPro\ 10

if has('gui_running')
        colorscheme Tomorrow
else
        colorscheme blue
endif

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
nnoremap  <A-j> 4jzz
nnoremap  <A-k> 4kzz
inoremap kj <Esc>
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l

"" plugins

" vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'bling/vim-airline'
Plugin 'https://github.com/kien/ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'
Plugin 'https://github.com/scrooloose/nerdtree.git'
Plugin 'https://github.com/Shougo/neocomplete.vim'
call vundle#end()
filetype plugin indent on

" airline
if has('gui_running')
        let g:airline_theme='tomorrow'
        let g:airline#extensions#whitespace#enabled = 0
        let g:airline_enable_bufferline=1
        let g:airline_left_sep = '⮀'
        let g:airline_right_sep = '⮂'
        set laststatus=2
endif

" nerdtree
hi NERDTreeDir ctermfg=darkblue

" neocomplete
if has('gui_running')
        let g:neocomplete#enable_at_startup = 1
        let g:neocomplete#enable_smart_case = 1
        inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

        if !exists('g:neocomplete#omni_patterns')
            let g:neocomplete#omni_patterns = {}
        endif
        let g:neocomplete#omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
        let g:neocomplete#omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
        let g:neocomplete#omni_patterns.cpp = '[^.[:digit:]*\t]\%(\.\|->\)\|\h\w*::'
        let g:neocomplete#omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
        autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
endif

