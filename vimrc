" Enable Pathogen
filetype off
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Re-enable filetype detection
filetype plugin indent on

" prevents vim from emulating the original vi's bugs and limitations
set nocompatible

set backspace=start,indent,eol " make backspace work like 'normal' text editors

set number                     " show line numbers

" Status
set showcmd                    " show the command being typed
set ruler                      " always show current position
set laststatus=2               " always show statusline
set wildmenu                   " better command autocompletion

" Tabbing
set tabstop=4                  " width of a tab character in spaces
set softtabstop=4              " defines number of spaces for when adding/remving tabs
set shiftwidth=4               " number of spaces to use for autoindent
set expandtab                  " use spaces instead of tab characters

" Indention
set autoindent
set cindent

" Disable annoying features
set noerrorbells               " Don't beep
set shortmess+=I               " Disable startup splash

" Undo
" Undo file
set undodir=~/.vim/undodir
set undofile
au BufWritePre /tmp/* setlocal noundofile   " Disable undofile in /tmp

set undolevels=1000             " maximum number of changes that can be undone
set undoreload=10000            " maximum number lines to save for undo on a buffer reload
set history=1000                " Lots of history
set backupdir=~/.vim/tmp,.      " Backup Dir
set directory=~/.vim/tmp,.      " Swap Dir

set hidden                     " allow buffer to be changed without writing to disk

" Ignore useless files
set wildignore+=*.o,*.so,*.swp,tags,*.P

" Search
set incsearch
set hlsearch
set smartcase
" clear highlighting on <esc> press
nnoremap <CR> :noh<CR><CR>

" Prevent delay pressing O after ESC
set timeout timeoutlen=1000 ttimeoutlen=100


" Use Wombat for colorful terminals, xterm16 for low color terms
if &t_Co > 16 || has("gui_running")
    syntax enable                  " enable syntax highlighting
    set t_Co=256                   " use 256 colours in terminal vim
    colorscheme wombat256mod
else
    syntax enable
    let xterm16_colormap = 'standard'
    let xterm16_brightness = 'default'
    colorscheme xterm16
endif

" When editing a file, always jump to the last cursor position
if has("autocmd")
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \   exe "normal! g'\"" |
  \ endif
endif

" Except, don't remember the cursor position in git commits
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/

" Display tabs
set listchars=tab:».
set list

" Keybindings

" best mapping ever - swap ; and :
nnoremap ; :

" Paste toggle (for saving indention)
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>

"Change directory to the dir of the current buffer
noremap \cd :cd %:p:h<CR>

" Window switching
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

" K splits lines (opposite of J)
nmap K i<cr><esc>k$

" go opens a new line, but does not enter insert mode
nmap go o<esc>
nmap gO O<esc>

" Windows-like copy/cut/paste mappings
" CTRL-V is Paste in insert mode
imap <C-V>              "+gpa
" CTRL-C is Copy, CTRL-X is Cut, in visual mode
vmap <C-C>              "+y
vmap <C-x>              "+d
" Use CTRL-Q to do what CTRL-V used to do
noremap <C-Q>           <C-V>

" Save the file with admin privs
cmap w!! w !sudo tee %

" F9 is Taglist
noremap <F9> :TlistToggle<CR>
" Tlist on the right *****Set in .gvimrc*****
let Tlist_Use_Right_Window = 1

" F5 is Gundo
nnoremap <F5> :GundoToggle<CR>

" Ctags
set tags=tags;/         " Search up for tags in Ctags
" C-\ Open definition in new tab
map <A-]> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
" A-] Open definition in vert split
map <C-\> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
" Generate Ctags on save
au BufWritePost .c,.cc,.cpp,*.h silent! !ctags -R &


" Syntax highlighting

autocmd! BufNewFile,BufRead *.pde setlocal ft=arduino   " Enable syntax highlighting for Arduino files.

" Go uses real tabs
au FileType go set noexpandtab
