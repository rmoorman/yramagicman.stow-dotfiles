nnoremap <f5> :call functions#CompileJS()<cr>
setlocal makeprg=yarn\ lint
let b:path = expand('%:p')
let b:pathlist = split( b:path, '/')
if index(b:pathlist, 'command-center') >= 0
    setlocal shiftwidth=4
endif
