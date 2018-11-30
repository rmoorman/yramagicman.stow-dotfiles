function! status#Refresh()
    for l:nr in s:listbufs()
        if winnr() == l:nr
            let l:active = status#Active()
            call setwinvar(l:nr, '&statusline', l:active)
        else
            let l:inactive = status#Inactive()
            call setwinvar(l:nr, '&statusline', l:inactive)
        endif
        if !exists('&statusline')
            let l:broken = status#Broken()
            call setwinvar(l:nr, '&statusline', l:broken)
        endif
    endfor
endfunction

function! status#Modified(one, two)
    let l:mod = split( a:two,':')
    let l:mod = l:mod[0]
    let l:mod = l:mod[1:]
    if l:mod == 'modified'
        let b:modified = '*'
        return
    endif
endfunction

function! status#Branch(one, two)
    let l:bname = split( a:two,':')
    let l:bname = l:bname[0]
    let l:bname = l:bname[1:]
    let b:branch = l:bname
endfunction

function! status#Active()
    let l:active = ''
    let a=job_start(['/bin/sh', '-c', 'git status | grep modified'], { 'out_io': 'pipe', 'err_io':'pipe',  'out_cb': 'status#Modified', 'err_cb': 'status#Modified'})
    let b=job_start(['/bin/sh', '-c', 'git branch | grep \*'], { 'out_io': 'pipe', 'err_io':'null',  'out_cb': 'status#Branch'})
    let l:active .= '%#focused#| %m %f %r %y %#normal#'
    if exists('b:branch')
        let l:active .=' %{b:branch}'
    endif
    if exists('b:modified')
        let l:active .=' %{b:modified}'
    endif
    let l:active .=' %='
    let l:active .=' %3l/%L, %-03c'
    let l:active .= ' %P'
    let l:active .= ' |'
    return l:active
endfunction

function! status#Inactive()
    let l:inactive = ''
    let l:inactive .= '%#normal#| %m %f %r %y'
    let l:inactive .='%='
    " let l:inactive .='%l/%L, %c'
    let l:inactive .=' %P'
    let l:inactive .=' |'
    return l:inactive
endfunction

function! status#Broken()
    let l:broken = ''
    let l:broken .= '%#visual#| %m %f %r %y %#normal#'
    let l:broken .=' %='
    let l:broken .=' %3l/%L, %-03c'
    let l:broken .= ' %P'
    let l:broken .= ' |'
    return l:broken
endfunction

function! s:listbufs()
    let l:bufs = []
    for prop in getbufinfo()
        call add( l:bufs, prop['bufnr'])
    endfor
    return l:bufs
endfunction

function! status#Autocmd()
    autocmd BufEnter,BufWritePre,ShellCmdPost * let b:modified = ''
    autocmd BufEnter,BufWritePre,ShellCmdPost * let b:branch = ''
    autocmd BufEnter,BufWritePost,InsertEnter,ShellCmdPost * call status#Refresh()
endfunction
