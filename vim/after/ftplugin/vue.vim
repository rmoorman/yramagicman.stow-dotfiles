nnoremap <f5> :call functions#CompileJS()<cr>
setlocal makeprg=npm\ run\ lint
let b:path = expand('%:p')
let b:pathlist = split( b:path, '/')
if has('nvim')
lua << EOF

    require("nvim-lsp-installer").setup {}
    require'lspconfig'.volar.setup{
    cmd = { "/home/jonathan/.local/share/nvim/lsp_servers/volar/node_modules/.bin/vue-language-server"
    }
    }

     vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
EOF
endif
