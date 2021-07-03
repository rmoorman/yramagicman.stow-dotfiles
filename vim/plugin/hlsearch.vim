
function! HlSearchCallback(timer)
    set nohlsearch
endfunction

function! Hlsearch()
    if exists(g:search_timer)
        call timer_stop('g:search_timer')
    endif
    set hlsearch
    let g:search_timer=timer_start(1500, 'HlSearchCallback')
endfunction
