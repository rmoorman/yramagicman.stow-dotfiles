" List to contain file paths of existing buffers
let g:SplitOrNew_names = []

" Populate g:SplitOrNew_names with the full paths of files in open buffers
function! s:list_names()
    for l:k in getbufinfo()
        for l:j in keys(l:k)
            if l:j == 'name'
                call add( g:SplitOrNew_names, l:k[l:j])
            endif
        endfor
    endfor
endfunction

" Determine if a list has an item
function! s:list_contains(needle, haystack)
    for l:hay in a:haystack
        echom l:hay .. ' ' .. a:needle
        if l:hay == a:needle
            return 1
        endif
    endfor
    return 0
endfunction

" Open a split or edit existing split based on if the file has an existing
" buffer
function! SplitOrNew(file)
    " repopulate the name list
    let g:SplitOrNew_names = []
    call s:list_names()
    " Get the entire path of a file. This is what Vims internal representation
    " of a buffer, or at least the one that's most useful in this scenario
    " buffer exists
    let l:full_path = fnamemodify(a:file, ':p')
    if  s:list_contains(l:full_path, g:SplitOrNew_names) == 1
        " Bring file into current active buffer
        execute ':e ' a:file
    else
        " Create Split with new file
        execute ':vsp ' a:file
    endif
endfunction

" Define a command that takes one argument and accepts file paths as completions
command! -nargs=1 -complete=file SplitOrNew call SplitOrNew('<args>')
