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
    call term_start(['git', 'add', '-v', expand('%')], { 'term_name': 'vimgit', 'term_rows': '4' })
endfunction

function! git#Commit(msg)
    call term_start(['git','commit','-m' , a:msg], { 'term_name': 'vimgit', 'term_rows': '4' })
endfunction

function! git#Log(number)
    call term_start(['git','log','--oneline', '--graph', '--decorate' , '-n', a:number ], { 'term_name': 'vimgit' })
endfunction

function! git#Logv(number)
    call term_start(['git','log', '-n', a:number ], { 'term_name': 'vimgit' })
endfunction

function! git#Status()
    call term_start(['git','status'], { 'term_name': 'vimgit' })
endfunction

function! git#Push()
    call term_start(['git','push'], { 'term_name': 'vimgit' })
endfunction
