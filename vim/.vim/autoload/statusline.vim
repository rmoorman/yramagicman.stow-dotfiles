function! statusline#Refresh()
    let b:modified = ' '
    let b:branch = ' '
    for l:nr in range(1, winnr('$'))
    if winnr() == l:nr
        call setwinvar(l:nr, '&statusline', '%!statusline#Active()')
    else
        call setwinvar(l:nr, '&statusline', '%!statusline#InActive()')
    endif
    endfor
endfunction

function! statusline#Modified(one, two)
    let l:mod = split( a:two,':')
    let l:mod = l:mod[0]
    let l:mod = l:mod[1:]
    if l:mod == 'modified'
        let b:modified = '*'
    else
        let b:modified = ' '
    endif
endfunction

function! statusline#Branch(one, two)
    let l:bname = split( a:two,':')
    let l:bname = l:bname[0]
    let l:bname = l:bname[1:]
    if len('l:bname') > 0
        let b:branch = l:bname
    else
        let b:branch = ' '
    endif
endfunction

function! statusline#Active()
    " Always show status line
    let l:active = ''
    let a=job_start(['/bin/sh', '-c', 'git status | grep modified'], { 'out_io': 'pipe', 'out_buf': 1, 'err_io':'null',  'out_cb': 'statusline#Modified'})
    let b=job_start(['/bin/sh', '-c', 'git branch | grep \*'], { 'out_io': 'pipe', 'out_buf': 1, 'err_io':'null',  'out_cb': 'statusline#Branch'})
    let l:active .= '%#focused#| %m %f %r %y %#normal#'
        let l:active .=' %{b:branch}'
        let l:active .=' %{b:modified}'
    let l:active .=' %='
    let l:active .=' %l/%L, %-02c'
    let l:active .= ' %P'
    let l:active .= ' |'
    return l:active
endfunction

function! statusline#InActive()
    " Always show status line
    let l:inactive = ''
    let l:inactive .= '%#normal#| %m %f %r %y'
    let l:inactive .='%='
    " let l:inactive .='%l/%L, %c'
    let l:inactive .=' %P'
    let l:inactive .=' |'
    return l:inactive
endfunction
