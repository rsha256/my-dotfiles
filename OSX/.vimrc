command! Wq wq

set number
syntax enable
set background=dark
let g:solarized_termcolors = 256  " New line!!
colorscheme solarized
let g:airline_theme='powerlineish'
set rtp+=/usr/local/opt/fzf
let g:airline#extensions#tabline#enabled = 1

set nocompatible
filetype off    " Required

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'altercation/vim-colors-solarized'
Bundle 'vim-airline/vim-airline'
Bundle 'vim-airline/vim-airline-themes'
Bundle 'airblade/vim-gitgutter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'godlygeek/tabular'
Bundle 'ervandew/supertab'

filetype plugin indent on " Required<Paste>

