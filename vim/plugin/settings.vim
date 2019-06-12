set shell=zsh
set updatetime=2000
" Make backspace behave in a sane manner.
set backspace=indent,eol,start
set showcmd
set nojoinspaces
set startofline
"Use os clipboard
set clipboard^=unnamedplus,unnamed
set path-=/usr/include
"{{{ completions
set omnifunc=syntaxcomplete#Complete
set wildmenu
set wildcharm=<C-z>
set wildmode=longest:full,full
"}}}
"{{{ builtin plugins
packadd! matchit
packadd! editexisting
"}}}
"{{{ file handling
" auto-reload modified files
set autoread
" write files on buffer switch, and other actions
set autowrite
"}}}
"{{{ terminal speed optimizations
set lazyredraw
set ttyfast
"}}}
"{{{ Show line numbers by default, see mappings below for toggle
set number
set relativenumber
"}}}
"{{{ tab settings
set expandtab
set shiftwidth=4
set softtabstop=4
set autoindent
"}}}
"{{{ fold settings
set foldcolumn=4
set foldmethod=indent
set foldlevel=999
"}}}
"{{{ wrap and scrolling
set wrap linebreak
set sidescrolloff=15
set scrolloff=2
"}}}
set listchars=tab:▸\ ,trail:·,eol:¬,extends:❯,precedes:❮"{{{
set showbreak=…→"}}}
"{{{ search settings
set nowrapscan
set incsearch
set ignorecase
set smartcase
"}}}
"{{{ shortmess
set shortmess+=A " ignore annoying swapfile messages
set shortmess+=I " no splash screen
set shortmess+=O " file-read message overwrites previous
set shortmess+=T " truncate non-file messages in middle
set shortmess+=W " don't echo "[w]"/"[written]" when writing
set shortmess+=a " use abbreviations in messages eg. `[RO]` instead of `[readonly]`
set shortmess+=o " overwrite file-written messages
set shortmess+=t " truncate file messages at start
"}}}
"{{{ turn off error bells
set novisualbell
set noerrorbells
if exists('&belloff')
    set belloff=all
endif
"}}}
"{{{ conditional settings
if &diff
    nnoremap <C-q> :qa!<cr>
    set foldmethod=diff
    set list
    set nowrap
    augroup diff
        autocmd!
        autocmd VimEnter * ALEDisable
    augroup end
endif

if !has('nvim') && &ttimeoutlen == -1
    set ttimeout
    set ttimeoutlen=100
endif

if exists('+breakindent')
    set breakindent
endif
"
" Centralize backups, swapfiles and undo history
if exists("&backupdir")
    set backupdir=~/.vim/backup//
endif
if exists("&directory")
    set directory=~/.vim/swaps//
endif
if exists("&undodir")
    set undolevels=5000
    set undodir=~/.vim/undo//
    set undofile
endif
"}}}
"{{{ statusline
" Show statusline, always
set laststatus=2
hi def focused ctermbg=2 ctermfg=0
augroup status
    autocmd!
    autocmd VimEnter * let &statusline = status#Active()
    autocmd VimEnter * call status#Autocmd()
augroup end
"}}}
"{{{ splits and buffers
set hidden
set splitbelow
set splitright
set winheight=2
set winminheight=2
"}}}
