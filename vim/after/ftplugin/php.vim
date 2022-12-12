let b:ale_linters=['phpcs', 'php']
let g:ale_php_phpcs_standard='PSR2'
function! Runtests()
    normal :wa
    normal :!./vendor/bin/phpunit ./tests/Unit/*

endfunction
nnoremap <F5> :wa<cr>:!( arclear > /dev/null 2>&1) && ./vendor/bin/phpunit $(find ./tests/Unit -type f -name '*.php')<cr>
" setlocal makeprg=./vendor/bin/phpcs\ %
setlocal list

if has('nvim')
    lua << EOF

    require("nvim-lsp-installer").setup {}
    require'lspconfig'.phpactor.setup{
    cmd = { "/home/jonathan/.local/share/nvim/lsp_servers/phpactor-source/bin/phpactor", "language-server" }
    }
    vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
EOF
endif
