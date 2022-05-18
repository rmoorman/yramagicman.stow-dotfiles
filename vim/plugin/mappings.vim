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
inoremap <leader><leader> <Esc>
xnoremap <leader><leader> <Esc>
nnoremap <leader>s :call functions#Save()<cr>
nnoremap <leader><leader> :call functions#Save()<cr>
nnoremap .<leader> i<leader><Esc>
"}}}
"{{{ completion mappings
cnoremap <expr> %% expand('%:h').'/'
"}}}
"{{{ terminal mode mappings
if ! has('nvim')
    tnoremap <leader><leader> <C-w>N
    tnoremap :: <C-w>N:
    nnoremap <C-w>t :vert term<cr>
    nnoremap <leader>t :vert term<cr>
else
    tnoremap <leader><leader> <C-\><C-N>
    tnoremap :: <C-\><C-N>:
    nnoremap <C-w>t :vsplit<cr> :term<cr>
    nnoremap <leader>t :vsplit<cr> :term<cr>
endif

"}}}
"{{{ normal mode mappings
nnoremap <leader>; :set hlsearch!<cr>
nnoremap <leader>, <C-^>
nnoremap * :call Hlsearch()<cr>*
nnoremap # :call Hlsearch()<cr>#
nnoremap / :call Hlsearch()<cr>/
nnoremap n :call Hlsearch()<cr>n
nnoremap N :call Hlsearch()<cr>N
nnoremap <F1> :call functions#NumberToggle()<cr>
nnoremap <F2> :call functions#WrapToggle()<cr>
nnoremap <F3> :set list!<cr>
nnoremap <F4> :call functions#StatusToggle()<cr>
nnoremap <F6> :call functions#FoldColumnToggle()<cr>
nnoremap <C-w>m :call functions#Maximize()<cr>
nnoremap <leader>r :syn sync fromstart<cr>
nnoremap >> V>gv
nnoremap << V<gv
nnoremap <leader>e :e **/*
nnoremap <leader>f :find **/*
nnoremap <leader>b :ls<cr>:b
nnoremap <leader>U :!cat % \| urlview<cr>
cnoreabbrev ,e e **/*
cnoreabbrev ,f find **/*
cnoreabbrev ,b ls<cr>:b
cnoreabbrev e, e **/*
cnoreabbrev f, find **/*
cnoreabbrev b, ls<cr>:b
"}}}
"{{{ visual mode mappings
xnoremap > >gv
xnoremap < <gv
"}}}
"{{{ command line movement mappings
cnoremap <C-a>  <Home>
cnoremap <C-b>  <Left>
cnoremap <C-f>  <Right>
cnoremap <C-d>  <Delete>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>
cnoremap <Esc>d <S-right><Delete>
cnoremap <C-e>  <End>
"}}}
"{{{ Kill annoying mapping
nnoremap gs <nop>
"}}}
inoreabbrev debugger; debugger;//eslint-disable-line
inoreabbrev  conosle console
