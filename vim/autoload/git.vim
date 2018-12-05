function! git#Cd()
    let l:gitdir = finddir('.git',expand('%:h') .';' . $HOME)
    if index( split( l:gitdir,'/' ), '.git' ) > 0
        let l:repopath = split(l:gitdir, '/')[0:-2]
        let l:repopath = '/'. join( l:repopath, '/' ) . '/'
        echom l:repopath
        execute 'cd ' l:repopath
    endif
endfunction
function! git#CommitFile(msg)
    call job_start(['/bin/sh', '-c', 'git', 'add', expand('%')] )
    call term_start(['git','commit','-m' , a:msg ], { 'term_name': 'gitcommit' })
endfunction

command! -nargs=1 GCommitFile call git#CommitFile('<args>')
