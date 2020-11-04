command! -nargs=1 -complete=customlist,reload#Completion Reload call reload#Reload('<args>')
command! VReload source $MYVIMRC
command! Scratch call functions#Scratch()

command! -nargs=1 -complete=command -bar -range Redir silent call redir#Redir(<q-args>, <range>, <line1>, <line2>)
