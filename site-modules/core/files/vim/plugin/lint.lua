local lint = require('lint')

local severities = {
  error = vim.diagnostic.severity.ERROR,
  info = vim.diagnostic.severity.WARN,
}

lint.linters.psalm = {
    cmd = 'psalm',
    stdin = false,
    args = {
        '--output-format=json',
        '--show-info=true'
    },
    -- ignore_exitcode = true,
    parser = function(output, _)
        if vim.trim(output) == '' then
            return {}
        end

        local errors = vim.json.decode(output)
        local diagnostics = {}

        for _, err in ipairs(errors or {}) do
            table.insert(diagnostics, {
                lnum = err.line_from - 1,
                end_lnum = err.line_to - 1,
                col = err.column_from - 1,
                end_col = err.column_to - 1,
                message = err.message,
                source = 'psalm',
                severity = assert(severities[err.severity], 'missing mapping for severity ' .. err.severity),
            })
        end

        return diagnostics
    end
}

local get_language_id = function()
  local file_type = vim.api.nvim_buf_get_option(0, "filetype")
  return "--language-id=" .. file_type
end

lint.linters.cspell =  {
  cmd = 'cspell',
  stdin = true,
  ignore_exitcode = true,
  args = {
    'lint',
    '--no-color',
    '--no-progress',
    '--no-summary',
    "--config=" .. os.getenv("HOME") .. "/.cspell.json",
    get_language_id,
    '--',
    'stdin'
  },
  stream = 'stdout',
  parser = require('lint.parser').from_errorformat('%f:%l:%c - %m', {
    source = 'cspell',
    severity = vim.diagnostic.severity.INFO
  })
}

lint.linters_by_ft = {
  php = {'phpcs'},
  typescript = {'eslint'},
  javascript = {'eslint'},
  typescriptreact = {'eslint'},
  javascriptreact = {'eslint'},
  lua = {'luacheck'},
}


-- Lint code with nvim-lint on save. This will lint all filetypes with cspell
-- and then any other filetypes will be linted per the config.
local lint_auto_command_group = vim.api.nvim_create_augroup("aa_lint", { clear = true })
vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
  group = lint_auto_command_group,
  desc = "Lint the buffer",
  pattern = "*",
  callback = function()
    lint.try_lint('cspell')
    lint.try_lint()
  end,
})

