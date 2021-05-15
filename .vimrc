"
" Harish's vimrc
"

" General Settings {{{1

syntax on
set number
set scrolloff=0
set nomodeline
set foldmethod=marker
set wildmenu
set incsearch
set ruler
set background=light
set laststatus=1
set autoindent
filetype plugin indent on

runtime macros/matchit.vim

let mapleader = ","

inoremap jk <Esc>

nnoremap <Leader>b :ls<CR>:b<Space>

" Custom Commands {{{2
" Commands: HNCopyFileName {{{3

command! HNCopyFileName      let @*=expand("%")
command! HNCopyFullFileName  let @*=expand("%:p")

" Command: HNWriteBackup {{{3

command! HNWriteBackup execute "w %:p." . system("date +%s")

" Commands: HNExecuteRange {{{3
"
" Filter range with command in new buffer
"
function! HNExecuteRange(command) range
    " Yank range and restore register
    let savereg = @a
    execute "normal! " . a:firstline . "gg\"ay" . a:lastline . "gg"
    let input = @a
    let @a = l:savereg
    " Open a new split and set it up.
    split __Results__
    normal! ggdG
    setlocal buftype=nofile
    " Paste yanked range
    call append(0, split(input, '\v\n'))
    execute "normal! ggVG:!" . a:command . "\<CR>"
endfunction

command! -range=% HNExecutePython <line1>,<line2>call HNExecuteRange("python")
command! -range=% HNExecuteBash <line1>,<line2>call HNExecuteRange("bash")

" Terminal Mode {{{2

if exists (":tnoremap")
    tnoremap <Esc> <C-\><C-n>
    tnoremap jk <C-\><C-n>
    tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
endif

" List Characters {{{2

set list
set listchars=tab:\`\ 

" Tab Settings {{{2

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

" FileType: SystemVerilog {{{3

augroup vimrc_systemverilog
    au!
    au FileType verilog set softtabstop=2
    au FileType verilog set shiftwidth=2
    au FileType verilog_systemverilog set softtabstop=2
    au FileType verilog_systemverilog set shiftwidth=2
    "au FileType verilog_systemverilog let g:verilog_syntax_fold_lst="all"
    "au FileType verilog_systemverilog set foldmethod=syntax
augroup END

" Gnupg Settings {{{1

let g:GPGPreferSymmetric = 1
let g:GPGPreferArmor = 1

