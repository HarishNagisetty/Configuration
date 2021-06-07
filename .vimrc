"
" Harish's vimrc
"

" General Settings {{{1

filetype plugin indent on
set autoindent
set scrolloff=0
set foldmethod=marker
set wildmenu
set incsearch
" For the semicolon, see :help file-searching
set tags=./tags;
set ttimeoutlen=50
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab
set nomodeline

runtime macros/matchit.vim

syntax on
set number
set laststatus=1
set ruler
set list
set listchars=tab:\`\ 

set background=light
if has('gui_running')
    colorscheme solarized
endif

" Mappings {{{1

let mapleader = ","

inoremap jk         <Esc>
nnoremap <Tab>      za
nnoremap <Leader>   :echo "
            \ b: Show Buffers\n
            \ d: Insert Date\n
            \ m: Open Bookmarks\n
            \ n: Open Notes\n
            \ s: Spell Check\n
            \ t: Tags"<CR>
            \:call HNFinishKeyMapping("\<Leader>")<CR>
nnoremap <Leader>b  :ls<CR>:b<Space>
nnoremap <Leader>d  :r! date<CR>
nnoremap <Leader>m  :sp ~/.marks<CR>
nnoremap <Leader>n  :sp ~/.notes<CR>
nnoremap <Leader>s  :echo "
            \ a: Add Spelling\n
            \ i: Ignore Spelling\n
            \ n: Next Error\n
            \ p: Previous Error\n
            \ r: Remove Spelling\n
            \ t: Toggle Spell Check On/Off"<CR>
            \:call HNFinishKeyMapping("\<Leader>s")<CR>
nnoremap <Leader>sa zg
nnoremap <Leader>si zG
nnoremap <Leader>sn ]s
nnoremap <Leader>sp [s
nnoremap <Leader>sr zug
nnoremap <Leader>st :setlocal invspell<CR>
nnoremap <Leader>t  :echo "
            \ f: Follow Tag\n
            \ l: List Tags\n
            \ n: Next (Push) Tag\n
            \ p: Previous (Pop) Tag"<CR>
            \:call HNFinishKeyMapping("\<Leader>t")<CR>
nnoremap <Leader>tf g]
nnoremap <Leader>tl :tags<CR>
nnoremap <Leader>tn :tag<CR>
nnoremap <Leader>tp <C-t>

" Commands {{{1

command!          HNCopyFileName  let @*=expand("%:p") | echo @*
command!          HNWriteBackup   execute "w %:p." . system("date +%s")
command! -range   HNEncryptRegion <line1>,<line2>! gpg -ca
command! -range   HNDecryptRegion <line1>,<line2>! gpg -dq
command! -range=% HNExecutePython <line1>,<line2>call HNExecuteRange("python")
command! -range=% HNExecuteBash   <line1>,<line2>call HNExecuteRange("bash")

" Functions {{{1

" Repeat input key prefixed with previous keys
function! HNFinishKeyMapping(keys)
    let key = input('Enter Key: ')
    redraw
    if len(key) > 0
        call feedkeys(a:keys . key)
    endif
endfunction

" Filter range with command in new buffer
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
    setlocal filetype=temporary
    " Paste yanked range
    call append(0, split(input, '\v\n'))
    execute "normal! ggVG:!" . a:command . "\<CR>"
endfunction

" Terminal Mode {{{1

if exists (":tnoremap")
    tnoremap <Esc> <C-\><C-n>
    tnoremap jk <C-\><C-n>
    " Insert register contents with Ctrl-R
    tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
endif

" Autocommands {{{1

augroup vimrc_text
    au!
    au FileType text setlocal colorcolumn=80
    au FileType text setlocal textwidth=79
    au FileType text setlocal spell
augroup END

augroup vimrc_systemverilog
    au!
    au FileType verilog_systemverilog setlocal foldmethod=manual
    au FileType verilog_systemverilog setlocal colorcolumn=100
    au FileType verilog_systemverilog,verilog setlocal softtabstop=2
    au FileType verilog_systemverilog,verilog setlocal shiftwidth=2
augroup END

augroup vimrc_notes
    au!
    au BufNewFile,BufRead *.notes set filetype=temporary syntax=conf
augroup END

augroup vimrc_marks
    au!
    au BufNewFile,BufRead *.marks set filetype=temporary syntax=markdown
    " gf (follow file) after first "(" on that line
    au BufNewFile,BufRead *.marks nnoremap <buffer> <CR> 0f(lgf
augroup END

augroup vimrc_temporary
    au!
    au FileType temporary setlocal noswapfile
    au FileType temporary nnoremap <buffer> q :q<CR>
augroup END

" Gnupg Settings {{{1

let g:GPGPreferSymmetric = 1
let g:GPGPreferArmor = 1

" Verilog-SystemVerilog Settings {{{1

let g:verilog_syntax_fold_lst = "all"

" Netrw Settings {{{1

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25
