augroup defaults
    autocmd!
    autocmd BufWritePost $MYVIMRC,*.vim source %
    autocmd CursorMoved $MYVIMRC,*.vim set foldmethod=marker
    autocmd BufWritePre,InsertLeave * checktime
    autocmd BufWritePre,InsertLeave * :%s/\s\+$//e
    autocmd BufWritePre * silent! :%s#\($\n\s*\)\+\%$##
    autocmd BufWritePre,InsertLeave * silent! :retab!
    autocmd BufEnter * call git#Cd()
    autocmd InsertLeave * call functions#Save()
    autocmd BufEnter * set cursorline
    autocmd BufLeave * set nocursorline
    autocmd BufWritePost * redraw!
    autocmd FileType * set textwidth=80
    autocmd FileType vim set foldmethod=marker
    autocmd FileType mail set textwidth=0
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=python3complete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    autocmd FileType clojure setlocal omnifunc=clojurecomplete#Complete
    autocmd FileType sql setlocal omnifunc=sqlcomplete#Complete
    autocmd FileType php set omnifunc=phpcomplete#CompletePHP
    if exists(':ALEDisableBuffer')
        autocmd BufRead,BufEnter .env :ALEDisableBuffer
    endif
    autocmd BufEnter,CursorHold * checktime
    autocmd WinLeave,InsertLeave * call functions#Save()
    " autocmd BufEnter *.vue source $HOME/.vim/after/ftplugin/js.vim
    " autocmd QuitPre * call range(1, bufnr('$'))->filter('getbufvar(v:val, "&buftype") == "terminal"')->map('term_setkill(v:val, "hup")')
    autocmd BufWritePost dwm.config.h call functions#MakeDWM()
augroup end
