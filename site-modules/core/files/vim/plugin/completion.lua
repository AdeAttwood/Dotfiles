local cmp = require'cmp'
local luasnip = require'luasnip'

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
    mapping = cmp.mapping.preset.insert({
        ['<C-j>'] = cmp.get_config().mapping['<Down>'],
        ['<C-k>'] = cmp.get_config().mapping['<Up>'],
        ['<CR>'] = cmp.mapping(function(_fallback)
            if cmp.visible() then
                cmp.confirm({ select = true })
            else
                vim.api.nvim_feedkeys('\n', 'nt', false)
            end
        end, { 'i', 's' }),
        ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end, { "i", "s" }),

        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
    }),
    sources = {
        { name = "luasnip" },
        { name = 'nvim_lsp' },
        { name = 'buffer' },
        { name = 'path' },
        { name = 'orgmode' }
    },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },
})

