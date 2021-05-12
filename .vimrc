"
" Harish's vimrc
"

" General Settings {{{1

syntax on
set number
set foldmethod=marker
set scrolloff=0
set nomodeline
set wildmenu
set incsearch
set ruler
set background=light
set laststatus=1

let mapleader = ","

inoremap jk <Esc>

" Execute Python code and put result on next line
xnoremap <leader>xpy yP'<O<Esc>gv:!python<CR>

" Scroll horizontally
"nnoremap <C-L> zL
"nnoremap <C-H> zH

" For terminal mode
if exists (":tnoremap")
    tnoremap <Esc> <C-\><C-n>
    tnoremap jk <C-\><C-n>
    tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
endif

" Show tab as backtick {{{2
set list
set listchars=tab:\`\ 

" Tab press is 4 spaces {{{2
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab

" Autocommands {{{2
" FileType: TeX {{{3
augroup vimrc_tex
    au!
    au FileType tex setlocal softtabstop=2
    au FileType tex setlocal shiftwidth=2
augroup END

" FileType: Markdown {{{3
augroup vimrc_markdown
    au!
    "au FileType markdown iabbrev <buffer> mdcm [//]: # (
augroup END

" FileType: Python {{{3
augroup vimrc_python
    au!
    au FileType python set foldmethod=indent
augroup END

" Gnupg Settings {{{1

let g:GPGPreferSymmetric = 1
let g:GPGPreferArmor = 1

