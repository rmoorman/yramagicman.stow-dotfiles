function! status#Refresh()
    for l:nr in s:listbufs()
        if winnr() == l:nr
            call setwinvar(l:nr, '&statusline', '%!status#Active()')
        else
            call setwinvar(l:nr, '&statusline', '%!status#Inactive()')
        endif
    endfor
endfunction

function! status#Modified(one, two)
    let l:mod = split( a:two,':')
    let l:mod = l:mod[0]
    let l:mod = l:mod[1:]
    let b:modified = 'no changes'
    echom l:mod
    if l:mod == 'modified'
        let b:modified = '*'
    endif
endfunction

" comment for modification
function! status#Branch(one, two)
    let l:bname = split( a:two,':')
    let l:bname = l:bname[0]
    let l:bname = l:bname[1:]
    if len('l:bname') > 0
        let b:branch = l:bname
    else
        let b:branch = ''
    endif
endfunction

function! status#Active()
    " Always show status line
    let l:active = ''
    let a=job_start(['/bin/sh', '-c', 'git status | grep modified'], { 'out_io': 'pipe', 'err_io':'null',  'out_cb': 'status#Modified'})
    let b=job_start(['/bin/sh', '-c', 'git branch | grep \*'], { 'out_io': 'pipe', 'err_io':'null',  'out_cb': 'status#Branch'})
    let l:active .= '%#focused#| %m %f %r %y %#normal#'
        let l:active .=' %{b:branch}'
        let l:active .=' %{b:modified}'
    let l:active .=' %='
    let l:active .=' %l/%L, %-02c'
    let l:active .= ' %P'
    let l:active .= ' |'
    return l:active
endfunction

function! status#Inactive()
    " Always show status line
    let l:inactive = ''
    let l:inactive .= '%#normal#| %m %f %r %y'
    let l:inactive .='%='
    " let l:inactive .='%l/%L, %c'
    let l:inactive .=' %P'
    let l:inactive .=' |'
    return l:inactive
endfunction

function! s:listbufs()
    let l:bufs = []
    for prop in getbufinfo()
        call add( l:bufs, prop['bufnr'])
    endfor
    return l:bufs
endfunction
