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
    call term_start(['git', 'add', expand('%')], {'term_finish': 'close', 'hidden': 'true'} )
    call term_start(['git','commit','-m' , a:msg ], { 'term_name': 'gitcommit', 'term_rows': '4' })
endfunction

function! git#Log(number)
    call term_start(['git','log','--oneline', '--graph', '--decorate' , '-n', a:number ], { 'term_name': 'gitcommit' })
endfunction
function! git#Logv(number)
    call term_start(['git','log', '-n', a:number ], { 'term_name': 'gitcommit' })
endfunction
function! git#Status()
    call term_start(['git','status'], { 'term_name': 'gitcommit' })
endfunction
command! -nargs=1 GCommitFile call git#CommitFile('<args>')
command! -nargs=1 GLog call git#Log('<args>')
command! -nargs=1 GLogVerbose call git#Logv('<args>')
command! -nargs=0 GStatus call git#Status()
