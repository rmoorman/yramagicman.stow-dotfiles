command! -nargs=1 -complete=customlist,reload#Completion Reload call reload#Reload('<args>')
command! VReload source $MYVIMRC
command! Scratch call functions#Scratch()
