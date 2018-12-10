function! git#Cd()
    let l:gitdir = finddir('.git',expand('%:h') .';' . $HOME)
    if index( split( l:gitdir,'/' ), '.git' ) > 0
        let l:repopath = split(l:gitdir, '/')[0:-2]
        let l:repopath = '/'. join( l:repopath, '/' ) . '/'
        echom l:repopath
        execute 'cd ' l:repopath
    endif
endfunction

function! s:closebuf()
    if bufexists('vimgit')
        execute 'bwipeout vimgit'
    endif
endfunction

function! s:termJob(job, rows)
    call s:closebuf()
    call term_start(a:job, { 'term_name': 'vimgit', 'term_rows': a:rows })
endfunction

function! git#AddBuffer()
    call s:termJob(['git', 'add', '-v', expand('%')], '2')
endfunction

function! git#AddAll()
    call s:termJob(['git', 'add', '-v', '.'], '2')
endfunction

function! git#CommitM(msg)
    call s:termJob(['git','commit','-m' , a:msg], '4')
endfunction

function! git#Commit()
    execute '!git commit -v' | redraw!
endfunction

function! git#CommitAll()
    execute '!git commit -av' | redraw!
endfunction

function! git#CommitAllM(msg)
    call s:termJob(['git','commit','-am' , a:msg], '4')
endfunction

function! git#Log(number)
    call s:termJob(['git','log','--oneline', '--graph', '--decorate' , '-n', a:number ],  '7')
endfunction

function! git#Logv(number)
    call s:termJob(['git','log', '-n', a:number ], '10')
endfunction

function! git#Status()
    call s:termJob(['git','status'], '15')
endfunction

function! git#Push()
    call s:termJob(['git','push'], '10')
endfunction

function! git#Pull()
    call s:termJob(['git','pull'], '10')
endfunction
