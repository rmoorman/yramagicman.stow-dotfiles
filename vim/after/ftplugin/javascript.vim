nnoremap <f5> :call functions#CompileJS()<cr>
setlocal makeprg=npm\ run\ lint
let b:path = expand('%:p')
let b:pathlist = split( b:path, '/')

if has('nvim')
    lua << EOF
require'lspconfig'.vuels.setup{
    cmd = { "/home/jonathan/.local/share/nvim/lsp_servers/vuels/node_modules/vls/bin/vls" }
}

    --local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
    --buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
EOF
endif
