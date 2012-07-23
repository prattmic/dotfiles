" prevents vim from emulating the original vi's bugs and limitations
set nocompatible               " enabled when (g)vimrc is found

set backspace=start,indent,eol " make backspace work like 'normal' text editors
set number                     " show line numbers
set showcmd                    " show the command being typed
set ruler                      " always show current position
set tabstop=4                  " width of a tab character in spaces
set softtabstop=4              " defines number of spaces for when adding/remving tabs
set shiftwidth=4               " number of spaces to use for autoindent
set expandtab                  " use spaces instead of tab characters
set autoindent
set cindent
set hidden                     " allow buffer to be changed without writing to disk
set wildmenu                   " better command autocompletion
set laststatus=2               " always show statusline
set noerrorbells               " Don't beep
set shortmess+=I               " Disable startup splash

" Enable undo after saving
au BufWritePre /tmp/* setlocal noundofile
set undodir=~/.vim/undodir
set undofile
set undolevels=1000             " maximum number of changes that can be undone
set undoreload=10000            " maximum number lines to save for undo on a buffer reload
set history=1000                " Lots of history
set backupdir=~/.vim/tmp,.      " Backup Dir
set directory=~/.vim/tmp,.      " Swap Dir 

" searching related
set incsearch
set hlsearch
set smartcase

set timeout timeoutlen=1000 ttimeoutlen=100     " Prevent delay pressing O after ESC

" Paste toggle (for saving indention)
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>

syntax enable                  " enable syntax highlighting
set t_Co=256                   " use 256 colours in terminal vim
colorscheme wombat256mod

autocmd! BufNewFile,BufRead *.pde setlocal ft=arduino   " Enable syntax highlighting for Arduino files.

filetype off                   " For Pathogen
call pathogen#runtime_append_all_bundles() " For sane plugin management
call pathogen#helptags()       " Help tags for Pathogen
filetype plugin indent on      " let vim detect filetype and load appropriate scripts

" When editing a file, always jump to the last cursor position
if has("autocmd") 
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \   exe "normal! g'\"" |
  \ endif
endif

" Useful default mappings

"Change directory to the dir of the current buffer
noremap \cd :cd %:p:h<CR>  

" clear highlighting on <esc> press
" nnoremap <esc> :noh<return><esc>
nnoremap <CR> :noh<CR><CR>

" Window switching
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

" Make C-BS and C-Del work like they do in most text editors for the sake of muscle memory
imap <C-BS> <C-W>
imap <C-Del> <esc>Ea<C-W>

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

" best mapping ever - swap ; and :
nnoremap ; :

" F9 is NERDTree, F10 is Taglist
noremap <F9> :TlistToggle<CR>

" Search up for tags in Ctags
set tags=tags;/
" C-\ Open definition in new tab
map <A-]> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>   
" A-] Open definition in vert split
map <C-\> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>    
" Generate Ctags on save
au BufWritePost .c,.cc,.cpp,*.h silent! !ctags -R &
" Tlist on the right *****Set in .gvimrc*****
let Tlist_Use_Right_Window = 1
