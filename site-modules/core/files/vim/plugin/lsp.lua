local lspconfig = require('lspconfig')


local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)

  -- TODO(ade): Sort out space. This will conflict with the leader key
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
end

local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
capabilities.textDocument.completion.completionItem.snippetSupport = true

local servers = { 'tsserver', 'intelephense', 'psalm', 'gopls',  'html', 'cssls' }
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup({
        on_attach = on_attach,
        capabilities = capabilities,
        init_options = {
            usePlaceholders = true,
        }
    })
end

lspconfig.emmet_ls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    filetypes = { 'html', 'typescriptreact', 'javascriptreact', 'css', 'scss' },
    init_options = {
        usePlaceholders = true,
    }
})

-- Change the diagnostic signs
vim.fn.sign_define('DiagnosticSignError', { text = '✖', texthl = 'DiagnosticSignError', numhl = 'DiagnosticSignError' })
vim.fn.sign_define('DiagnosticSignHint', { text = '➤', texthl = 'DiagnosticSignHint', numhl = 'DiagnosticSignHint' })
vim.fn.sign_define('DiagnosticSignInfo', { text =  'ℹ', texthl = 'DiagnosticSignInfo', numhl = 'DiagnosticSignInfo' })
vim.fn.sign_define('DiagnosticSignWarn', { text =  '⚠', texthl = 'DiagnosticSignWarn', numhl = 'DiagnosticSignWarn' })

