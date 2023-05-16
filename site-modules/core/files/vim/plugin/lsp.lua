local lspconfig = require "lspconfig"

local servers = {
  -- Language servers for the day to day web development, could probably think
  -- about loosing the html and css one and living with typescript, rescript
  -- and emmet
  tsserver = {},
  rescriptls = {
    cmd = {
      os.getenv "NVM_BIN" .. "/node",
      os.getenv "HOME" .. "/.config/nvim/pack/bundle/start/vim-rescript/server/out/server.js",
      "--stdio",
    },
  },
  html = {},
  cssls = {},
  clojure_lsp = {},
  emmet_ls = {
    filetypes = { "html", "typescriptreact", "javascriptreact", "css", "scss", "eruby" },
  },
  -- Ruby
  solargraph = {},
  -- Rust
  rust_analyzer = {},
  -- Lua for the vim config and plugin dev
  lua_ls = {
    settings = {
      Lua = {
        runtime = { version = "LuaJIT" },
        diagnostics = { globals = { "vim" } },
        workspace = { library = vim.api.nvim_get_runtime_file("", true) },
        telemetry = { enable = false },
      },
    },
  },
}

local on_attach = function(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local opts = { noremap = true, silent = true }

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)

  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)

  vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
  vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
    buffer = bufnr,
    callback = function ()
      -- Highlight document symbles for every file type other erb files because
      -- solargraph only supports textDocument/documentHighlight in rb files.
      local file_type = vim.api.nvim_buf_get_option(0, "filetype")
      if file_type ~= "eruby" then
        vim.lsp.buf.document_highlight()
      end
    end,
    group = "lsp_document_highlight",
    desc = "Document Highlight",
  })

  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = bufnr,
    callback = function ()
      vim.lsp.buf.clear_references()
    end,
    group = "lsp_document_highlight",
    desc = "Clear All the References",
  })

end

local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
capabilities.textDocument.completion.completionItem.snippetSupport = true

for lsp, config in pairs(servers) do
  config["on_attach"] = on_attach
  config["capabilities"] = capabilities
  config["init_options"] = { usePlaceholders = true }

  lspconfig[lsp].setup(config)
end

-- Change the diagnostic signs
vim.fn.sign_define("DiagnosticSignHint", { text = "➤", texthl = "DiagnosticSignHint", numhl = "DiagnosticSignHint" })
vim.fn.sign_define("DiagnosticSignInfo", { text = "ℹ", texthl = "DiagnosticSignInfo", numhl = "DiagnosticSignInfo" })
vim.fn.sign_define("DiagnosticSignWarn", { text = "⚠", texthl = "DiagnosticSignWarn", numhl = "DiagnosticSignWarn" })
vim.fn.sign_define(
  "DiagnosticSignError",
  { text = "✖", texthl = "DiagnosticSignError", numhl = "DiagnosticSignError" }
)

local border = 'rounded'

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {border = border})
vim.lsp.handlers["textDocument/show_line_diagnostics"] = vim.lsp.with(vim.lsp.handlers.hover, {border = border})
vim.lsp.handlers["textDocument/diagnostic"] = vim.lsp.with(vim.lsp.handlers.hover, {border = border})
vim.lsp.handlers["textDocument/diagnostics"] = vim.lsp.with(vim.lsp.handlers.hover, {border = border})
vim.diagnostic.config({
  float = {
    focusable=false,
    border = border
  }
})

vim.cmd([[set updatetime=1000]])
vim.api.nvim_create_augroup("diagnostic_float", { clear = true })
vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
  callback = function ()
    vim.diagnostic.open_float()
  end,
  group = "diagnostic_float",
  desc = "Open Diagnostic Float",
})
