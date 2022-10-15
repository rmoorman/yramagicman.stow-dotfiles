if has('nvim')
    set packpath+=$HOME/.config/nvim/pack/vendor/
    let s:optpath = $HOME.'/.config/nvim/pack/vendor/opt'
    let s:startpath = $HOME.'/.config/nvim/pack/vendor/start'
else
    set packpath+=$HOME/.vim/pack/vendor/
    let s:optpath = $HOME.'/.vim/pack/vendor/opt'
    let s:startpath = $HOME.'/.vim/pack/vendor/start'
endif
function! s:jobstart(job)
    if has('nvim')
        " echom a:job
        :split
        return termopen(a:job)
    else
        call term_start(a:job)
    endif
endfunction
function! s:sanity_check()
    for d in split(&packpath, ',')
        if !isdirectory(d)
            silent! call mkdir(d, 'p')
        endif
    endfor
    if !isdirectory(s:optpath)
        silent! call mkdir(s:optpath)
    endif

    if !isdirectory(s:startpath)
        silent! call mkdir(s:startpath)
    endif
endfunction


function! s:read_start_dir()
    return split( globpath(s:startpath, '*'), '\n' )
endfunction


function! s:read_opt_dir()
    return split( globpath(s:optpath, '*'), '\n' )
endfunction

function! s:mkurl(plug)
    let urlparts = split(a:plug, '/')
    if urlparts[0][1:] == 'https:' || urlparts[0][1:4] == 'git@' || urlparts[0][1:4] == 'git:'
        return a:plug
    elseif urlparts[0][1:] == 'http:'
        echom 'Not secure, refusing to clone' . a:plug
    else
        return 'https://github.com/'. join(urlparts, '/')
    endif
endfunction

let s:start_plugs = []
function! s:install_start_plugins(plug)
    let url = s:mkurl(a:plug)
    let plug = split( a:plug, '/' )[1]
    let plug = plug[:-2]
    let destination = split( &packpath, ',')[-1] . '/start/'. plug
    let moduledest = split(destination, '\.')[1]
    let cdpath=join(split(moduledest,'/')[:-2], '/')

    call add(s:start_plugs, [destination, 'git clone --depth 3 '. url . ' ' . moduledest])
endfunction

let s:opt_plugs = []

function! s:install_opt_plugins(plug)
    let url = s:mkurl(a:plug)
    let plug = split( a:plug, '/' )[1]
    let plug = plug[:-2]
    let destination = split( &packpath, ',')[-1] . '/opt/'. plug
    let moduledest = split(destination, '\.')
    let moduledest = '.'. moduledest[1]

    let cdpath=join(split(moduledest,'/')[:-2], '/')
    call add(s:opt_plugs, [destination, 'git clone --depth 3 '. url . ' ' . moduledest])
endfunction

function! s:clean_plugins()
    let start_in = []
    let start_list = []
    for p in s:start_plugs
        call add(start_list,  split(p[0], '/')[-1] )
    endfor

    for p in s:read_start_dir()
        call add(start_in,  split(p, '/')[-1] )
    endfor

    let opt_in = []
    let opt_list = []
    for p in s:opt_plugs
        call add(opt_list,  split(p[0], '/')[-1] )
    endfor

    for p in s:read_opt_dir()
        call add(opt_in,  split(p, '/')[-1] )
    endfor

    if len(start_in) != len(start_list)
        for q in start_in
            if index(start_list, q) == -1
                echom 'removing ' . q
                let path = s:startpath
                echom join( systemlist('rm -rf '.path.q) )
            endif
        endfor
    else
    endif


    if len(opt_in) != len(opt_list)
        for p in opt_in
            if index(opt_list, p) == -1
                echom 'removing ' . q
                let path = s:optpath
                echom join( systemlist('rm -rf '.path.p) )
            endif
        endfor
    else

    endif
endfunction

function! s:update_one(plug)
    echom 'updating ' . a:plug
    let cmd = 'cd ' . a:plug ." && git pull"
    let j = s:jobstart(["/bin/sh", "-c", cmd])
    return j
endfunction

function! s:update_all()
    let opt_jobs = []
    let start_jobs = []
    for path in s:read_opt_dir()
        call add(opt_jobs,  s:update_one(path))
    endfor

    while len(opt_jobs)
        for job in opt_jobs
            if job_status(job) != 'run'
                call remove(opt_jobs, index(opt_jobs, job))
            endif
        endfor
    endwhile
    for path in s:read_start_dir()
        call add(start_jobs,  s:update_one(path))
    endfor

    while len(start_jobs)
        for work in start_jobs
            if job_status(work) != 'run'
                call remove(start_jobs, index(start_jobs, work))
            endif
        endfor
    endwhile
endfunction

function! s:do_update()
    if exists('g:VimPack_Update_Frequency')
        let oneday = 24 * 60 * 60
        let today = split( system('date +%s') )[0]
        if filereadable($HOME.'/.vim/.lastupdate')
            let updatetime = readfile($HOME.'/.vim/.lastupdate')[1]
            if today > updatetime
                autocmd! VimLeavePre * call s:update_all()
                autocmd! CursorHold * echom 'updating on close'

                let nextupdate = today + (oneday * g:VimPack_Update_Frequency)
                call writefile([today], $HOME.'/.vim/.lastupdate')
                call writefile([nextupdate], $HOME.'/.vim/.lastupdate', "a")
                return
            endif
        else
            autocmd! VimLeavePre * call s:update_all()
            autocmd! CursorHold * echom 'updating on close'
            let nextupdate = today + (oneday * g:VimPack_Update_Frequency)
            call writefile([today], $HOME.'/.vim/.lastupdate')
            call writefile([nextupdate], $HOME.'/.vim/.lastupdate', "a")
            return
        endif
    else
        let g:VimPack_Update_Frequency = 30
        call s:do_update()
    endif
endfunction

function! s:install_all()
    call s:read_start_dir()
    call s:sanity_check()
    let plugs = s:start_plugs + s:opt_plugs
    let s:start_job = 'bash -c "cd '. split(&packpath, ',')[-1]
    let s:start_job = s:start_job . ' && cd $(git rev-parse --show-toplevel) && '
    for package in l:plugs
        if !isdirectory(package[0])
            echom 'cloning ' . package[0]
            let s:start_job = s:start_job . ' ' . package[1] .' ;'
        else
            echom 'found ' . package[0]
        endif
    endfor
    let s:start_job = s:start_job . ' pwd"'
    let @s = s:start_job
    echom s:start_job
    call s:jobstart(s:start_job)
    silent! helptags ALL
    echom ''
endfunction

function! pack#load()
    autocmd VimEnter *  call s:sanity_check()
    autocmd VimEnter *  call s:do_update()
    command! -nargs=+ PlugOpt call s:install_opt_plugins(<f-args>)
    command! -nargs=+ PlugStart call s:install_start_plugins(<f-args>)
    command! PlugInstall  call s:install_all()
    command! PlugClean  call s:clean_plugins()
    command! PlugUpdate  call s:update_all()
endfunction

autocmd VimEnter *  call s:sanity_check()
if exists('g:VimPack_Auto_Install')
    "autocmd BufEnter *  call s:install_all() | redraw
endif
if exists('g:VimPack_Auto_Update')
    "autocmd BufEnter *  call s:do_update()
endif
