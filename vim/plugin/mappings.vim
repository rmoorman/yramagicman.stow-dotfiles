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
"{{{ completion mappings
inoremap <expr> <tab> completions#InsertTabWrapper()
inoremap <expr> <c-k>  completions#CompleteKeyword()
call completions#BindCompleteTag("\<c-t>")
call completions#BindCompleteKeyword("\<c-k>")
call completions#BindCompleteFile("\<c-f>")
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"
cnoremap <expr> %% expand('%:h').'/'
"}}}
"{{{ terminal mode mappings
tmap <space><space> <C-w>N
tmap :: <C-w>N:
"}}}
"{{{ normal mode mappings
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
nnoremap >> V>gv
nnoremap << V<gv
nnoremap <leader>e :e **/*
nnoremap <leader>f :find **/*
nnoremap <leader>b :ls<cr>:b
"}}}
"{{{ visual mode mappings
vnoremap > >gv
vnoremap < <gv
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
