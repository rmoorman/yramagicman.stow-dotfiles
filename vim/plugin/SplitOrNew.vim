" List to contain file paths of existing buffers
let g:SplitOrNew_names = []

" Populate g:SplitOrNew_names with the full paths of files in open buffers
function! s:list_names()
    for l:k in getbufinfo()
        let l:bufinfo ={ 'bufname': l:k['name'], 'bufnr': l:k['bufnr'], 'shortname': split(l:k['name'], '/')[-1] }
        let g:SplitOrNew_names = add(g:SplitOrNew_names, l:bufinfo)
    endfor
endfunction

" Filter existing buffers to find match
function! s:filter_dict_list(target_prop, target_value, haystack)
    for l:target in a:haystack
        if get(l:target, a:target_prop) == a:target_value
            return l:target
        endif
    endfor
    " Consistent use of get() means that returning an empty collection here is
    " going to be less error prone
    return {}
endfunction

" Move cursor to matching buffer
function! s:rotate_cursor(target, buffer, attempts)
    if a:target > 0 && a:attempts > len(g:SplitOrNew_names) + 1 && bufnr('%') == g:SplitOrNew_start_buf
    " make sure that the function stops when there's an open buffer that isn't
    " in a split
        execute 'b' a:target
    elseif a:buffer == a:target
        " we found what we're looking for, return and stop
        return
    else
        " keep looking recursively
        execute 'wincmd w'
        call s:rotate_cursor(a:target, bufnr('%'), a:attempts + 1)
    endif
endfunction

" Open a split or edit existing split based on if the file has an existing
" buffer
function! SplitOrNew(file)
    " repopulate the name list
    let g:SplitOrNew_names = []
    call s:list_names()
    let l:target = s:filter_dict_list('shortname', split(a:file, '/')[-1], g:SplitOrNew_names)
    " track the starting buffer so we can return here if a buffer is found with
    " no open split.
    let g:SplitOrNew_start_buf = bufnr('%')
    if get(l:target, 'bufnr') > 0
        " go to the first buffer
        call s:rotate_cursor(get(l:target, 'bufnr'), 0, 0)
    else
        " Create Split with new file
        execute ':vsp ' a:file
    endif
endfunction

function! Complain(file)
    call SplitOrNew(a:file)
    echom "I'll still do what you asked, however this command has been replaced by the shorter :F command, or if you want interactive completions by buffer, the :J command"
endfunction

" Define a command that takes one argument and accepts file paths as completions
command! -nargs=1 -complete=buffer J call SplitOrNew('<args>')
command! -nargs=1 -complete=file F call SplitOrNew('<args>')
command! -nargs=1 -complete=file SplitOrNew call Complain('<args>')
