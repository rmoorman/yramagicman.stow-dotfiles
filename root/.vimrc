"{{{ defaults
scriptencoding utf-8
if filereadable(expand('$VIMRUNTIME/defaults.vim'))
    unlet! g:skip_defaults_vim
    source $VIMRUNTIME/defaults.vim
endif
" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Enable file type detection and do language-dependent indenting.
filetype plugin indent on

" Switch syntax highlighting on
syntax on
"}}}
colorscheme portable
set background=dark
"{{{ Plugin loading and settings
let g:VimPack_Setup_Folders = ['after', 'autoload', 'backup', 'colors', 'doc', 'snippets', 'spell', 'swaps', 'syntax', 'tags', 'undo']
let g:VimPack_Update_Frequency = 5
let g:VimPack_Auto_Install = 1
let g:VimPack_Auto_Update = 1

call pack#load()

PlugStart 'editorconfig/editorconfig-vim'
PlugStart 'tpope/vim-commentary'
PlugStart 'vim-scripts/vim-indent-object'
PlugStart 'tpope/vim-surround'
PlugStart 'bronson/vim-visual-star-search'
PlugStart 'tpope/vim-repeat'
PlugStart 'Raimondi/delimitMate'
PlugOpt 'dzeban/vim-log-syntax'
PlugOpt 'w0rp/ale'
PlugOpt 'mileszs/ack.vim'
PlugOpt 'leafgarland/typescript-vim'
PlugOpt 'shawncplus/phpcomplete.vim'
PlugOpt 'hail2u/vim-css3-syntax'
PlugOpt 'vim-scripts/Sass'
PlugOpt 'othree/html5.vim'
PlugOpt 'jwalton512/vim-blade'
PlugOpt 'posva/vim-vue'
if system('which fzf')[:-2] != 'fzf not found'
    if filereadable('/usr/share/vim/vimfiles/plugin/fzf.vim')
        source /usr/share/vim/vimfiles/plugin/fzf.vim
        PlugStart 'junegunn/fzf.vim'
        cnoreabbrev b Buffers<CR>
        cnoreabbrev find Files<CR>
        cnoreabbrev gf GFiles<CR>
        cnoreabbrev fg GFiles<CR>
    endif
endif
"}}}
"{{{ autocommands for loading extensions
augroup extensions
    autocmd!
    autocmd FileType vim,css,scss,sass,html,javascript,python,php,c,cpp,typescript,zsh,sh silent! packadd ale | redraw
    autocmd BufRead *.ts  set filetype=typescript
    autocmd BufRead *.org  set filetype=org
    autocmd FileType typescript packadd typescript-vim
    autocmd FileType html packadd html5.vim
    autocmd FileType scss packadd Sass
    autocmd FileType scss,css packadd vim-css3-syntax
    autocmd FileType org packadd vim-speeddating
    autocmd FileType org packadd vim-orgmode
    autocmd FileType php silent! packadd  phpcomplete.vim | redraw
    autocmd BufRead *.blade.php silent! packadd vim-blade | redraw
    autocmd BufRead *.blade.php silent! set filetype=blade | redraw
    autocmd BufRead *.vue silent! packadd vim-vue | redraw
    autocmd BufRead *.vue silent! packadd ale | redraw
    autocmd BufRead *.vue silent! set filetype=vue | redraw
    autocmd FileType vue syn sync fromstart
augroup end
"}}}
"{{{ ale settings
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
"}}}
let mapleader=","

set shell=zsh

" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" enable Omnicomplete
set omnifunc=syntaxcomplete#Complete
"{{{ builtin plugins
packadd! matchit
packadd! editexisting
"}}}

" auto-reload modified files
set autoread
" write files on buffer switch, and other actions
set autowrite

"terminal speed optimizations
set lazyredraw
set ttyfast

" Show statusline, always
set laststatus=2

" Don't show line numbers by default, see mappings below for toggle
set nonumber
set norelativenumber
set expandtab
set eol
set nowrapscan
set showcmd
set foldcolumn=4
set foldmethod=indent
set wildmenu
set foldlevel=999
set wildmode=full
set shiftwidth=4
set softtabstop=4
set autoindent
set smartindent
set list wrap linebreak sidescrolloff=15
set listchars=tab:▸\ ,trail:·,eol:¬,extends:❯,precedes:❮
set showbreak=…→

set nojoinspaces

set splitbelow
set splitright
set scrolloff=2
" 256 colors
set t_Co=256
"Use os clipboard
set clipboard^=unnamedplus,unnamed
set incsearch

" Ignore case of searches
set ignorecase
set smartcase
set startofline
set shortmess+=A " ignore annoying swapfile messages{{{
set shortmess+=I " no splash screen
set shortmess+=O " file-read message overwrites previous
set shortmess+=T " truncate non-file messages in middle
set shortmess+=W " don't echo "[w]"/"[written]" when writing
set shortmess+=a " use abbreviations in messages eg. `[RO]` instead of `[readonly]`
set shortmess+=o " overwrite file-written messages
set shortmess+=t " truncate file messages at start}}}
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
endif"}}}
"{{{ statusline
hi def focused ctermbg=2 ctermfg=0
augroup status
    autocmd!
    autocmd BufEnter,BufWritePost,InsertEnter * call status#Refresh()
augroup end
"}}}
set hidden
set winheight=2
set winminheight=2
"{{{ autocmds for everything else
augroup defaults
    autocmd!
    autocmd BufWritePost $MYVIMRC source %
    autocmd BufWritePre,InsertLeave * checktime
    autocmd BufWritePre,InsertLeave * :%s/\s\+$//e
    autocmd BufWritePre * silent! :%s#\($\n\s*\)\+\%$##
    autocmd BufWritePre,InsertLeave * silent! :retab!
    autocmd InsertLeave * call functions#Save()
    autocmd BufEnter * set cursorline
    autocmd BufLeave * set nocursorline
    autocmd BufWritePost * redraw!
    autocmd FileType * set textwidth=80
    autocmd FileType mail set textwidth=0
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=python3complete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    autocmd FileType clojure setlocal omnifunc=clojurecomplete#Complete
    autocmd FileType sql setlocal omnifunc=sqlcomplete#Complete
    autocmd BufRead,BufEnter .env :ALEDisableBuffer
    autocmd BufEnter,CursorHold * checktime
    autocmd CursorHold * call functions#Save()
    autocmd BufWritePost *.vue,*.js call functions#CompileJS()
augroup end
"}}}
hi ExtraWhitespace cterm=underline
match ExtraWhitespace /\s\+$/

"{{{ kill arrow keys
noremap <left>  <Nop>
noremap <down>  <Nop>
noremap <up>    <Nop>
noremap <right> <Nop>
inoremap <left>  <Nop>
inoremap <down>  <Nop>
inoremap <up>    <Nop>
inoremap <right> <Nop>
"}}}
"{{{ escape and save
inoremap <space><space> <Esc>
vnoremap <space><space> <Esc>
nnoremap <space><space> :call functions#Save()<cr>
nnoremap .<space> i<space><Esc>
"}}}
"{{{ Tab complete keywords
inoremap <expr> <tab> functions#InsertTabWrapper()
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
cnoremap <expr> %% expand('%:h').'/'
inoremap <c-f> <c-x><c-f>
"}}}
"{{{ mappings
nnoremap <leader><space> :set hlsearch!<cr>
nnoremap <leader><leader> <C-^>
nnoremap * :set hlsearch<cr>*
nnoremap <F1> :call functions#NumberToggle()<cr>
nnoremap <F2> :call functions#WrapToggle()<cr>
nnoremap <F3> :set list!<cr>
nnoremap <F4> :call functions#StatusToggle()<cr>
nnoremap <F6> :call functions#FoldColumnToggle()<cr>
nnoremap <C-w>m :call functions#Maximize()<cr>
nnoremap <leader>r :syn sync fromstart<cr>
command! Scratch call functions#Scratch()
"}}}
