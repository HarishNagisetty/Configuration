"
" Harish's vimrc
"

" General Settings {{{1

set nocompatible

syntax on
set autoindent
filetype plugin indent on
set number
set nomodeline
set laststatus=1
set background=light
set ruler

set scrolloff=0
set foldmethod=marker
set wildmenu
set incsearch
set tags=./tags;

runtime macros/matchit.vim

" Custom Key Mappings {{{2

let mapleader = ","
inoremap jk <Esc>

nnoremap <Leader> :echo "
            \ b: Show Buffers\n
            \ t: Tags"<CR>

" Show Buffers {{{3

nnoremap <Leader>b :ls<CR>:b<Space>

" Tags {{{3

nnoremap <Leader>t :echo "
            \ tf: Follow Tag\n
            \ tp: Previous (Pop) Tag\n
            \ tn: Next (Push) Tag\n
            \ tl: List Tags"<CR>

" Follow Tag
nnoremap <Leader>tf g]
" Pop
nnoremap <Leader>tp <C-t>
" Next (Push?)
nnoremap <Leader>tn :tag<CR>
" List
nnoremap <Leader>tl :tags<CR>

" Custom Commands {{{2
" Commands: HNCopyFileName {{{3

command! HNCopyFileName      let @*=expand("%")
command! HNCopyFullFileName  let @*=expand("%:p")

" Command: HNWriteBackup {{{3

command! HNWriteBackup execute "w %:p." . system("date +%s")

" Commands: HN(En|De)cryptRegion {{{3

command! -range HNEncryptRegion <line1>,<line2>! gpg -ca
command! -range HNDecryptRegion <line1>,<line2>! gpg -dq

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
    au FileType python setlocal foldmethod=indent
augroup END

" FileType: SystemVerilog {{{3

augroup vimrc_systemverilog
    au!
    au FileType verilog setlocal softtabstop=2
    au FileType verilog setlocal shiftwidth=2
    au FileType verilog_systemverilog setlocal softtabstop=2
    au FileType verilog_systemverilog setlocal shiftwidth=2
    au FileType verilog_systemverilog setlocal foldmethod=manual
    au FileType verilog_systemverilog setlocal colorcolumn=100
augroup END

" Gnupg Settings {{{1

let g:GPGPreferSymmetric = 1
let g:GPGPreferArmor = 1

" Verilog-SystemVerilog Settings {{{1

let g:verilog_syntax_fold_lst = "all"

