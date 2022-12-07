hi clear
syntax reset
set background=dark
let colors_name='portable'
hi Normal ctermbg=none ctermfg=15
hi Type ctermfg=12
hi VertSplit ctermbg=0 ctermfg=0
hi String ctermfg=14
hi Comment ctermfg=7
hi Error ctermfg=0
hi LineNr ctermfg=7
hi CursorLineNr ctermfg=3
hi NonText ctermbg=none ctermfg=8
hi SpecialKey ctermfg=9
hi Boolean ctermfg=10
hi Search ctermbg=2 ctermfg=0
hi CursorLine ctermbg=0 cterm=none
hi FoldColumn ctermbg=none ctermfg=7
hi Folded ctermbg=0 ctermfg=7
hi VisualNOS cterm=reverse
hi Visual ctermbg=3 ctermfg=8
hi WildMenu ctermbg=0 ctermfg=15
hi Pmenu ctermbg=0 ctermfg=15
hi PmenuSel ctermbg=0 ctermfg=15 cterm=underline
hi PmenuSbar ctermfg=4
hi Statement ctermfg=2
hi Function ctermfg=3
hi Operator ctermfg=7 cterm=bold
hi Identifier ctermfg=2 cterm=bold
hi Keyword ctermfg=4 cterm=bold
hi MatchParen ctermfg=15 ctermbg=1
hi Cursor ctermfg=1 ctermbg=1
" hi ColorColumn
hi Character ctermfg=14 cterm=underline
hi Number ctermfg=15
hi Conditional ctermfg=4 cterm=bold
hi Constant ctermfg=3
" hi Directory
" hi TabLine
" hi TabLineSel
hi Todo ctermfg=2
" hi TabLineFill
hi StatusLine ctermfg=0 ctermbg=15
hi StatusLineNC ctermfg=0 ctermbg=7
" hi ColorColumn
hi DiffAdd ctermfg=15 ctermbg=8
hi DiffChange ctermfg=1 ctermbg=none
hi DiffDelete ctermbg=none ctermfg=7 cterm=underline
hi DiffText ctermfg=0 ctermbg=7
if has("spell")
hi SpellBad cterm=underline
" hi SpellCap
" hi SpellLocal
" hi SpellRare
endif
