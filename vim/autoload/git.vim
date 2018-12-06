function! git#Cd()
    let l:gitdir = finddir('.git',expand('%:h') .';' . $HOME)
    if index( split( l:gitdir,'/' ), '.git' ) > 0
        let l:repopath = split(l:gitdir, '/')[0:-2]
        let l:repopath = '/'. join( l:repopath, '/' ) . '/'
        echom l:repopath
        execute 'cd ' l:repopath
    endif
endfunction

function! git#AddBuffer()
    call job_start(['git', 'add', expand('%')])
endfunction

function! git#Commit(msg)
    call term_start(['git','commit','-m' , a:msg], { 'term_name': 'gitcommit', 'term_rows': '4' })
endfunction

function! git#Log(number)
    call term_start(['git','log','--oneline', '--graph', '--decorate' , '-n', a:number ], { 'term_name': 'gitlog' })
endfunction

function! git#Logv(number)
    call term_start(['git','log', '-n', a:number ], { 'term_name': 'gitlog' })
endfunction

function! git#Status()
    call term_start(['git','status'], { 'term_name': 'gitstatus' })
endfunction

function! git#Push()
    call term_start(['git','push'], { 'term_name': 'gitpush' })
endfunction
