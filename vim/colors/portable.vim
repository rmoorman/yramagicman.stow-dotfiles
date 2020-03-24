hi clear
syntax reset
set background=dark
let colors_name='portable'
hi foldcolumn ctermbg=NONE ctermfg=12
hi linenr ctermfg=12
hi cursorlinenr ctermfg=9
hi search cterm=REVERSE ctermfg=8 ctermbg=15
hi diffadd ctermfg=4 ctermbg=0
hi diffchange ctermfg=9 ctermbg=0
hi diffdelete ctermfg=1 ctermbg=0
hi difftext ctermbg=0 ctermfg=NONE
hi pmenu ctermbg=0 ctermfg=7
hi pmenusel ctermfg=0 ctermbg=7
hi statusline ctermbg=7 ctermfg=0 cterm=REVERSE
hi statuslinenc ctermbg=0 ctermfg=7
hi visual ctermbg=4 ctermfg=11
hi vertsplit ctermbg=8 ctermfg=0
hi wildmenu ctermbg=0 ctermfg=15 cterm=REVERSE
hi signcolumn ctermbg=none ctermfg=15
" hi cursorline cterm=REVERSE
hi folded ctermbg=0 ctermfg=7
hi error ctermfg=1 ctermbg=0
hi preproc ctermfg=3 ctermbg=NONE
hi matchparen cterm=none ctermfg=9 ctermbg=0 term=none
hi ExtraWhitespace cterm=underline
hi tabline ctermbg=none
hi tablinefill ctermfg=0 ctermbg=0
hi tablinesel ctermbg=none
call matchadd('ExtraWhitespace', "/\s\+$/")
