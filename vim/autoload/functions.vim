"{{{ Fix Line Endings
function! functions#LineEndings()
    update
    e ++ff=dos
    setlocal ff=unix
    :w
endfunction
"}}}
"{{{ open scratch buffer
function! functions#Scratch()
    let tmp=system('mktemp')
    call execute( ":botright edit " . tmp[0:-1] )
endfunction
"}}}
"{{{ Maximize
function! functions#Maximize()
    wincmd _
    wincmd |
endfunction
"}}}
"{{{ wrap toggle, provides a nice message when wrap is toggled.
function! functions#WrapToggle()
    set wrap!
    if &wrap == 1
        echom 'wrap on'
    else
        echom 'wrap off'
    endif
endfunction
"}}}
"{{{ tab completion
" function! functions#InsertTabWrapper()
"     let col = col('.') - 1
"     if !col || getline('.')[col - 1] !~ '\k'
"         return "\<tab>"
"     elseif pumvisible()
"         return "\<C-n>"
"     else
"         return "\<C-n>"
"     endif
" endfunction
"}}}
"{{{ toggle status line
function! functions#StatusToggle()
    if &laststatus == 2
        set laststatus=0
    else
        set laststatus=2
    endif
endfunction
"}}}
"{{{ saving
function! functions#Save()
    if filewritable(expand('%')) == 1 || ! findfile(expand('%:t'), expand('%:h'))
        silent! w!
    endif
endfunction
"}}}
" {{{ compile javascript
function! functions#CompileJS()
    if !bufexists('compiling-js')
        call term_start(['/bin/sh', '-c', 'npm run lint'], {'term_rows': 5, 'term_finish': 'close' , 'term_name': 'compiling-js'})
        redraw!
    endif
endfunction
" }}}
" {{{ rotate through numbering options
function! functions#NumberToggle()
    if !exists('b:NumState')
        let b:NumState = 1
    endif
    if b:NumState == 1
        set number
        set norelativenumber
        let b:NumState = 2
    elseif b:NumState == 2
        set relativenumber
        set number
        let b:NumState = 3
    elseif b:NumState == 3
        set relativenumber
        set nonumber
        let b:NumState = 0
    else
        set nonumber
        set norelativenumber
        let b:NumState = 1
    endif
endfunction
"}}}
" {{{ toggle foldcolumn
function! functions#FoldColumnToggle()
    if !exists('g:FoldColumn')
        let g:FoldColumn = 1
    endif
    if g:FoldColumn == 1
        hi FoldColumn ctermfg=0
        let g:FoldColumn = 0
    else
        hi FoldColumn ctermfg=7
        let g:FoldColumn = 1
    endif
endfunction
"}}}
