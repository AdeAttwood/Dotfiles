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

lint.linters_by_ft = {
    php = {'phpcs'},
    typescript = {'eslint'},
    javascript = {'eslint'},
    typescriptreact = {'eslint'},
    javascriptreact = {'eslint'},
    lua = {'luacheck'},
}

