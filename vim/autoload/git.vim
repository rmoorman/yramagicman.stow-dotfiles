function! git#Cd()
    let l:gitdir = finddir('.git',expand('%:h') .';' . $HOME)
    if index( split( l:gitdir,'/' ), '.git' ) > 0
        let l:repopath = split(l:gitdir, '/')[0:-2]
        let l:repopath = '/'. join( l:repopath, '/' ) . '/'
        echom l:repopath
        execute 'cd ' l:repopath
    endif
endfunction
function! git#Commit(msg)
    call term_start(['git', 'add', expand('%')], {'term_name': 'gitcommit' })
    call term_start(['git','commit','-m' , a:msg ], { 'term_name': 'gitcommit' })
endfunction
