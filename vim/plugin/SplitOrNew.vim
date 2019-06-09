" List to contain file paths of existing buffers
let g:SplitOrNew_names = {}
let g:SplitOrNew_buf_track = 0
" Populate g:SplitOrNew_names with the full paths of files in open buffers
function! s:list_names()
    for l:k in getbufinfo()
        let g:SplitOrNew_names[l:k['name']] =  l:k['bufnr']
    endfor
endfunction

" Determine if a list has an item
function! s:rotate_cursor(file)
    let g:SplitOrNew_buf_track = bufnr('%')
    let l:target =  get(g:SplitOrNew_names, a:file )
    if g:SplitOrNew_buf_track == l:target
        return
    else
        let l:attempts = 0
        execute 'wincmd l'
        if g:SplitOrNew_buf_track != bufnr('%')
            call s:rotate_cursor(a:file)
        endif
    endif
endfunction

" Open a split or edit existing split based on if the file has an existing
" buffer
function! SplitOrNew(file)
    " repopulate the name list
    let g:SplitOrNew_names = {}
    call s:list_names()
    " Get the entire path of a file. This is what Vims internal representation
    " of a buffer, or at least the one that's most useful in this scenario
    " buffer exists
    let l:full_path = fnamemodify(a:file, ':p')
    if  get(g:SplitOrNew_names, a:file )
        " go to the first buffer
        wincmd t
        call s:rotate_cursor(a:file)
    else
        " Create Split with new file
        execute ':vsp ' a:file
    endif
endfunction

" Define a command that takes one argument and accepts file paths as completions
command! -nargs=1 -complete=file SplitOrNew call SplitOrNew('<args>')
