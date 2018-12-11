function! reload#Reload()
    let vimdir = '/' . join( split( $MYVIMRC, '/')[:-2], '/' )
    let files = split(globpath(vimdir.'/**', '*.vim'), '\n')
    for file in files
        if index(split(file,'/'), 'undo') == -1 && index(split(file,'/'), 'syntax') == -1
            silent! execute 'source ' . file
        endif
    endfor
    source $MYVIMRC
endfunction
