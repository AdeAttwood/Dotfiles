local lspconfig = require "lspconfig"

local workspace_library = vim.api.nvim_get_runtime_file("lua", true)
table.insert(workspace_library, os.getenv "HOME" .. "/Code/src/github.com/AdeAttwood/Configz/definitions/configz.lua")

local function has_file(file)
  return vim.fn.filereadable(vim.fn.getcwd() .. "/" .. file) == 1
end

local servers = {
  -- Language servers for the day to day web development, could probably think
  -- about loosing the html and css one and living with typescript, rescript
  -- and emmet
  ts_ls = { enabled = has_file "tsconfig.json" or has_file "yarn.lock" },
  denols = { enabled = has_file "deno.json" or has_file "deno.jsonc" },
  html = {},
  cssls = {},
  marksman = {},
  nushell = {},
  clojure_lsp = {},
  emmet_ls = {
    filetypes = { "html", "typescriptreact", "javascriptreact", "css", "scss", "eruby", "liquid" },
  },
  -- Ruby
  solargraph = {
    cmd = { "bundle", "exec", "solargraph", "stdio" },
  },
  -- Rust
  rust_analyzer = {
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = {
          command = "clippy",
        },
      },
    },
  },
  -- Lua for the vim config and plugin dev
  lua_ls = {
    settings = {
      Lua = {
        telemetry = { enable = false },
        runtime = { version = "LuaJIT" },
        diagnostics = {
          globals = { "vim", "configz", "describe", "it", "assert", "spy", "stub", "before_each", "after_each" },
        },
        workspace = {
          library = workspace_library,
          checkThirdParty = false,
        },
      },
    },
  },
  csharp_ls = {
    handlers = {
      ["textDocument/definition"] = require("csharpls_extended").handler,
      ["textDocument/typeDefinition"] = require("csharpls_extended").handler,
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
    callback = function()
      -- Highlight document symbles for every file type other erb files because
      -- solargraph only supports textDocument/documentHighlight in rb files.
      local file_type = vim.api.nvim_buf_get_option(0, "filetype")
      if file_type ~= "eruby" and file_type ~= "markdown" and file_type ~= "liquid" and file_type ~= "nu" then
        vim.lsp.buf.document_highlight()
      end
    end,
    group = "lsp_document_highlight",
    desc = "Document Highlight",
  })

  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = bufnr,
    callback = function()
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

require("ionide").setup { on_attach = on_attach, capabilities = capabilities }

vim.diagnostic.config {
  float = {
    focusable = false,
    border = "rounded",
  },
  virtual_text = {
    prefix = " ",
  },
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = '',
      [vim.diagnostic.severity.INFO] = '',
      [vim.diagnostic.severity.WARN] = '',
      [vim.diagnostic.severity.HINT] = '',
    }
  }
}

vim.cmd [[set updatetime=1000]]
vim.api.nvim_create_augroup("diagnostic_float", { clear = true })
vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
  callback = function()
    vim.diagnostic.open_float()
  end,
  group = "diagnostic_float",
  desc = "Open Diagnostic Float",
})
