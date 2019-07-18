if ! exists('g:complete_timers')
    let g:complete_timers = 0
endif
"{{{ tab completion
function! completions#InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    elseif pumvisible()
        return "\<C-n>"
    else
        return "\<C-n>"
    endif
endfunction
"}}}
"{{{ open omni complete pop up
let g:calls = 0
function! completions#insertComplete()
    function! Showmenu(t)
        if ! pumvisible() && v:insertmode == 'i'
            silent! call feedkeys("\<c-x>\<c-o>")
        endif
        let g:calls = g:calls + 1
    endfunction
    let l:col = col('.') - 1
    if ! pumvisible() && getline('.')[l:col - 1] =~ '\k' && g:calls < 1
        call completions#clearTimer()
        let g:complete_timers  = timer_start(100, 'Showmenu', {'repeat': 1})
    endif
    if getline('.')[l:col - 1] !~ '\k'
        let g:calls = 0
    endif
endfunction
"}}}
"{{{ Bind key to switch to keyword completion
function! completions#BindCompleteKeyword(bind)
    let g:complete_keyword = a:bind
    execute 'inoremap <expr> ' . g:complete_keyword . ' completions#CompleteKeyword()'
endfunction

function! completions#CompleteKeyword()
    if pumvisible()
        return "\<c-x>\<c-i>\<c-n>"
    else
        if exists('g:complete_keword')
            return g:complete_keword
        endif
    endif
endfunction
"}}}
"{{{ Bind key to switch to tag completion
function! completions#BindCompleteTag(bind)
    let g:complete_tag = a:bind
    execute 'inoremap <expr> ' . g:complete_tag . ' completions#CompleteTag()'
endfunction

function! completions#CompleteTag()
    if pumvisible()
        return "\<c-x>\<c-]>\<c-n>"
    else
        if exists('g:complete_tag')
            return g:complete_tag
        endif
    endif
endfunction
"}}}
"{{{ Bind key to switch to file completion
function! completions#BindCompleteFile(bind)
    let g:complete_file = a:bind
    execute 'inoremap <expr> ' . g:complete_file . ' completions#CompleteFile()'
endfunction

function! completions#CompleteFile()
    if pumvisible()
        return "\<c-x>\<c-f>\<c-n>"
    else
        if exists('g:complete_file')
            return g:complete_file
        endif
    endif
endfunction
"}}}
"{{{ clear timer for completions
function! completions#clearTimer()
    if g:complete_timers > 0
        call timer_stop(g:complete_timers)
    endif
    if exists('g:complete_timers')
        let g:complete_timers = 0
    endif
endfunction
"}}}
