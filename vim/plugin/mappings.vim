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
xnoremap <space><space> <Esc>
nnoremap <space><space> :call functions#Save()<cr>
nnoremap .<space> i<space><Esc>
"}}}
"{{{ completion mappings
cnoremap <expr> %% expand('%:h').'/'
"}}}
"{{{ terminal mode mappings
if ! has('nvim')
    tnoremap <space><space> <C-w>N
    tnoremap :: <C-w>N:
else
    tnoremap <space><space> <C-\><C-N>
    tnoremap :: <C-\><C-N>:
endif

"}}}
"{{{ normal mode mappings
nnoremap <leader><space> :set hlsearch!<cr>
nnoremap <leader><leader> <C-^>
nnoremap * :set hlsearch<cr>*
nnoremap # :set hlsearch<cr>#
nnoremap / :set hlsearch<cr>/
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
vnoremap gs <nop>
inoremap gs <nop>
"}}}
