function! statusline#Active()
    " Always show status line
    let f=system('[[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"')
    let b=system('git branch 2>/dev/null | grep \* | sed "s/\*//g"')
    let c=split(b, '')
    set statusline=%#normal#\|\ %m\ %f\ %r\ \%y\
    setlocal statusline=%#focused#\|\ %m\ %f\ %r\ \%y\ %#normal#
    if len(c)
        setlocal statusline+=\ \%{c[0]}
    endif
    if len(f)
        setlocal statusline+=\ %{f[0]}
    endif
    setlocal statusline+=%=
    setlocal statusline+=%l/%L\,\ %c
    setlocal statusline+=\ %P
    setlocal statusline+=\ \|
endfunction

function! statusline#InActive()
    " Always show status line
    let f=system('[[ $(git diff --shortstat 2> /dev/null | tail -n1) != "" ]] && echo "*"')
    let b=system('git branch 2>/dev/null | grep \* | sed "s/\*//g"')
    let c=split(b, '')
    set statusline=%#normal#\|\ %m\ %f\ %r\ \%y\
    setlocal statusline=%#normal#\|\ %m\ %f\ %r\ \%y
    if len(c)
        setlocal statusline+=\ \%{c[0]}
    endif
    if len(f)
        setlocal statusline+=\ %{f[0]}
    endif
    setlocal statusline+=%=
    setlocal statusline+=%l/%L\,\ %c
    setlocal statusline+=\ %P
    setlocal statusline+=\ \|
endfunction
