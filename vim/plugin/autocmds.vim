augroup defaults
    autocmd!
    autocmd BufWritePost $MYVIMRC,*.vim source %
    autocmd CursorMoved $MYVIMRC,*.vim set foldmethod=marker
    autocmd BufWritePre,InsertLeave * checktime
    autocmd BufWritePre,InsertLeave * :%s/\s\+$//e
    autocmd BufWritePre * silent! :%s#\($\n\s*\)\+\%$##
    autocmd BufWritePre,InsertLeave * silent! :retab!
    autocmd BufEnter * call git#Cd()
    autocmd InsertLeave,CursorHold * call functions#Save()
    autocmd BufEnter * set cursorline
    autocmd BufLeave * set nocursorline
    autocmd FileType * set textwidth=80
    autocmd FileType vim set foldmethod=marker
    autocmd FileType mail set textwidth=0
    if !has('nvim')
        autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
        autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
        autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
        autocmd FileType python setlocal omnifunc=python3complete#Complete
        autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
        autocmd FileType clojure setlocal omnifunc=clojurecomplete#Complete
        autocmd FileType sql setlocal omnifunc=sqlcomplete#Complete
        autocmd FileType php set omnifunc=phpcomplete#CompletePHP
    endif
    autocmd BufEnter,CursorHold * checktime
    " autocmd CursorHoldI * call feedkeys("\<c-x>\<c-n>")
    autocmd WinLeave,InsertLeave * call functions#Save()
    " autocmd BufEnter *.vue source $HOME/.vim/after/ftplugin/js.vim
    autocmd BufEnter *.tpl set filetype=php
augroup end
