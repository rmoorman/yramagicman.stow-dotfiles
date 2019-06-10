function! reload#Reload(target)
    let s:t = a:target
    function! s:findfile(idx, val)
        let v = split(a:val,'/')
        let file = join(v,'/')
        if index(v, s:t) > -1
            return 1
        else
            return 0
        endif
    endfunction
    let vimdir = '/' . join( split( $MYVIMRC, '/')[:-2], '/' )
    let files = split(globpath(vimdir.'/**', '*.vim'), '\n')
    call filter(files, function('s:findfile'))
    for f in files
        execute 'source ' . f
    endfor
    echom 'reloaded ' . join(files) | redraw
endfunction

function! reload#Completion(arglead, cmdline, cursorpos)
    let s:complete = a:arglead
    function! s:completions(idx, value)
        echom s:complete == a:value[:len(s:complete) - 1]
        return s:complete == a:value[:len(s:complete) - 1]
    endfunction
    let vimdir = '/' . join( split( $MYVIMRC, '/')[:-2], '/' )
    let files = split(globpath(vimdir.'/**', '*.vim'), '\n')
    function! s:filenames(key, value)
        return join( split(a:value, '/')[-1:], '/')
    endfunction
    call map(files,  function('s:filenames'))
    call add(files, split( $MYVIMRC, '/' )[-1])
    call filter(files,  function('s:completions'))
    return files
endfunction
