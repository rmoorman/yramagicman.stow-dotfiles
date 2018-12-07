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
