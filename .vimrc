
" set path
set path=.,,**

" enable syntax highlight
syn enable

" enable hybrid line numeration
set number
set relativenumber

" enable mouse
set mouse=a

" width of '>>' and '<<' shifts
set shiftwidth=4

" use spaces instead of tabs
set expandtab

" number of spaces used for tab
set tabstop=4

" tab at start of line will result in shift
set smarttab

" don't fit long lines to window width
set nowrap

" copy tab from previous line; add tab after '{' etc.
set smartindent

" show popup when using autocompletion in vim shell
set wildmenu

" completion popup
set completeopt=menuone,menu,longest,preview

" enable type-specific plugins loading
filetype plugin on

" make backspace work
set backspace=indent,eol,start

" make search case-insensitive...
set ignorecase

" ...and make it case-sensitive when typing uppercase
set smartcase

" statusline
set ruler

" highlight current line
set cursorline

" set colorscheme
" colorscheme morning
colorscheme evening
" colorscheme elflord

" lines above/below cursor
set scrolloff=4

" remove unwanted whitespaces
autocmd BufWritePre * :%s/\s\+$//e

" highlight long lines
let w:m1=matchadd('Search', '\%<80v.\%>79v', -1)
let w:m2=matchadd('ErrorMsg', '\%>79v.\+', -1)

" code running shortcuts
map <F5> :!gcc -std=c99 -Wall -Werror -pedantic -O2 "%" -o "%:r" && ./"%:r"
map <F6> :!g++ -std=c++11 -Wall -Werror -O2 "%" -o "%:r" && ./"%:r"
map <F7> :!python3 "%"

" :set paste shortcut
set pastetoggle=<F12>

" keybindings
let mapleader = ","
nnoremap <S-w> :w<Return>
nnoremap <Leader>s :%s//<Left>
vnoremap kj <Esc>
vnoremap <S-w> :w<Return>
vnoremap <Leader>s :s//<Left>
vnoremap < <gv
vnoremap > >gv
inoremap kj <Esc>
inoremap <C-o> <C-x><C-o>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Return>
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-h> <C-w>h
nmap <C-l> <C-w>l

" filetype specific settings
autocmd FileType c setlocal shiftwidth=2 tabstop=2
autocmd FileType sh setlocal shiftwidth=2 tabstop=2
