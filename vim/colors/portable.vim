hi clear
syntax reset
set background=dark
let colors_name='portable'
hi Boolean ctermfg=10
hi Character ctermfg=14 cterm=underline
hi Comment ctermfg=7
hi Conceal ctermbg=7 ctermfg=0
hi Conditional ctermfg=4 cterm=bold
hi Constant ctermfg=3
hi Cursor ctermfg=1 ctermbg=1
hi CursorLine ctermbg=0 cterm=none
hi CursorLineNr ctermfg=3
hi DiffAdd ctermfg=15 ctermbg=8
hi DiffChange ctermfg=1 ctermbg=none
hi DiffDelete ctermbg=none ctermfg=7 cterm=underline
hi DiffText ctermfg=0 ctermbg=7
" hi Directory
hi Error ctermfg=0
hi FoldColumn ctermbg=none ctermfg=7
hi Folded ctermbg=0 ctermfg=7
hi Function ctermfg=3
hi Identifier ctermfg=2 cterm=bold
hi Keyword ctermfg=4 cterm=bold
hi LineNr ctermfg=7
hi MatchParen ctermfg=15 ctermbg=1
hi NonText ctermbg=none ctermfg=8
hi Normal ctermbg=none ctermfg=15
hi Number ctermfg=15
hi Operator ctermfg=7 cterm=bold
hi Pmenu ctermbg=0 ctermfg=15
hi PmenuSbar ctermfg=4
hi PmenuSel ctermbg=0 ctermfg=15 cterm=underline
hi CocMenuSel ctermbg=0 ctermfg=15 cterm=underline
hi Search ctermbg=2 ctermfg=0
hi SignColumn ctermbg=none
hi SpecialKey ctermfg=9
hi Statement ctermfg=2
hi StatusLine ctermfg=0 ctermbg=15
hi StatusLineNC ctermfg=0 ctermbg=7
hi String ctermfg=14
" hi TabLine
" hi TabLineFill
" hi TabLineSel
hi Todo ctermfg=2
hi Type ctermfg=12
hi VertSplit ctermbg=0 ctermfg=0
hi Visual ctermbg=3 ctermfg=8
hi VisualNOS cterm=reverse
hi WildMenu ctermbg=0 ctermfg=15
if has("spell")
hi SpellBad cterm=underline
" hi SpellCap
" hi SpellLocal
" hi SpellRare
endif
