command! -nargs=1 GCommit call git#CommitM('<args>')
command! -nargs=0 GCommitAllV call git#CommitAll()
command! -nargs=1 GCommitAll call git#CommitAllM('<args>')
command! -nargs=0 GCommitV call git#Commit()
command! -nargs=1 GLog call git#Log('<args>')
command! -nargs=1 GLogVerbose call git#Logv('<args>')
command! -nargs=0 GStatus call git#Status()
command! -nargs=0 GPush call git#Push()
command! -nargs=0 GPull call git#Pull()
command! -nargs=0 GAdd call git#AddBuffer()
command! -nargs=0 GAddAll call git#AddAll()
command! -nargs=1 GCheckout call git#Checkout('<args>')
command! -nargs=0 GDiff call git#Diff()
