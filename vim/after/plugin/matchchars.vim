 "{{{ autocomplete quotes and brackets
inoremap (  ()<Left>
inoremap [  []<Left>
inoremap {  {}<Left>
inoremap <  <><Left>
inoremap [<CR> [<CR>]<ESC>O
inoremap (<CR> (<CR>)<ESC>O
inoremap {<CR> {<CR>}<ESC>O
inoremap <expr> )  strpart(getline('.'), col('.')-1, 1) == ")" ? "\<Right>" : ")"
inoremap <expr> ]  strpart(getline('.'), col('.')-1, 1) == "]" ? "\<Right>": "]"
inoremap <expr> }  strpart(getline('.'), col('.')-1, 1) == "}" ? "\<Right>" : "}"
inoremap <expr> >  strpart(getline('.'), col('.')-1, 1) == ">" ? "\<Right>" : ">"
inoremap <expr> " strpart(getline('.'), col('.')-1, 1) == "\"" ? "\<Right>" : "\"\"\<Left>"
inoremap <expr> ' strpart(getline('.'), col('.')-1, 1) == "\'" ? "\<Right>" : "\'\'\<Left>"
 "}}}
 "{{{ filetype specific mappings for characters and shortcuts
augroup abbrevs
    autocmd FileType vim inoremap <buffer> " "
    autocmd FileType text,gitcommit,markdown,mail,lisp,racket,scheme inoremap <buffer> ' '
augroup end
 "}}}
 "{{{ Make backspace work nicely with autopairs
function! Backspace()
    let l:current = strpart(getline('.'), col('.')-1, 1)
    let l:prev = strpart(getline('.'), col('.')-2, 1)
    if l:current == '"' || l:current == "'" || l:current == "]" || l:current == ")" || l:current == "}" || l:current == ">"
        if l:current == l:prev
            return "\<Right>\<BS>\<BS>"
        elseif l:prev == '[' && l:current == ']'
            return "\<Right>\<BS>\<BS>"
        elseif l:prev == "{" && l:current == "}"
            return "\<Right>\<BS>\<BS>"
        elseif l:prev == "(" && l:current == ")"
            echom 'bs'
            return "\<Right>\<BS>\<BS>"
        elseif l:prev == "<" && l:current == ">"
            return "\<Right>\<BS>\<BS>"
        else
            return "\<BS>"
        endif
    else
        return "\<BS>"
    endif
endfunction
inoremap <expr> <BS> Backspace()
 "}}}
