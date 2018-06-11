function! statusline#Refresh()
    for l:nr in range(1, winnr('$'))
        if winnr() == l:nr
            call setwinvar(l:nr, '&statusline', '%!statusline#Active()')
        else
            call setwinvar(l:nr, '&statusline', '%!statusline#InActive()')
        endif
    endfor
endfunction

function! statusline#Active()
    " Always show status line
    let l:active = ''
    let f=system('[[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"')
    let b=system('git branch 2>/dev/null | grep \* | sed "s/\*//g"')
    let c=split(b, '')
    let l:active .= '%#focused#| %m %f %r %y %#normal#'
    if len(c)
        let l:active .=' %{c[0]}'
    endif
    if len(f)
        let l:active .=' %{f[0]}'
    endif
    let l:active .=' %='
    let l:active .=' %l/%L, %-02c'
    let l:active .= ' %P'
    let l:active .= ' |'
    return l:active
endfunction

function! statusline#InActive()
    " Always show status line
    let f=system('[[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"')
    let b=system('git branch 2>/dev/null | grep \* | sed "s/\*//g"')
    let c=split(b, '')
    let l:inactive = ''
    let l:inactive .= '%#normal#| %m %f %r %y'
    if len(c)
        let l:inactive .=' %{c[0]}'
    endif
    if len(f)
        let l:inactive .=' %{f[0]}'
    endif
    let l:inactive .='%='
    " let l:inactive .='%l/%L, %c'
    let l:inactive .=' %P'
    let l:inactive .=' |'
    return l:inactive
endfunction
