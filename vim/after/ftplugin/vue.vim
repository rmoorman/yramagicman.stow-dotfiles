nnoremap <f5> :call functions#CompileJS()<cr>
setlocal makeprg=npm\ run\ lint
let b:path = expand('%:p')
let b:pathlist = split( b:path, '/')
