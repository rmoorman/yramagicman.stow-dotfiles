if ! executable('fzf')
    finish
endif
unmap <leader>f
unmap <leader>b
nnoremap <leader>f :Files<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>t :Tags<cr>
cnoreabbrev ,f :Files<cr>
cnoreabbrev ,b :Buffers<cr>
cnoreabbrev ,t :Tags<cr>
cnoreabbrev f, :Files<cr>
cnoreabbrev b, :Buffers<cr>
cnoreabbrev t, :Tags<cr>
