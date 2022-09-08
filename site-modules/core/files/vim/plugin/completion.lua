local cmp = require'cmp'
local luasnip = require'luasnip'

local icons = {
Text = "",
    Method = "",
    Function = "",
    Constructor = "",
    Field = "ﰠ",
    Variable = "",
    Class = "ﴯ",
    Interface = "",
    Module = "",
    Property = "ﰠ",
    Unit = "塞",
    Value = "",
    Enum = "",
    Keyword = "",
    Snippet = "",
    Color = "",
    File = "",
    Reference = "",
    Folder = "",
    EnumMember = "",
    Constant = "",
    Struct = "פּ",
    Event = "",
    Operator = "",
}

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
    mapping = cmp.mapping.preset.insert({
        ['<C-j>'] = cmp.get_config().mapping['<Down>'],
        ['<C-k>'] = cmp.get_config().mapping['<Up>'],
        ['<C-e>'] = cmp.mapping.abort(),
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
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    formatting = {
      fields = {   'menu', 'abbr', 'kind' },
      format = function(entry, vim_item)
        -- Give the completion menu a consistent size to stop it jumping arround
        local width = 34
        if #vim_item.abbr > width then
          vim_item.abbr = string.sub(vim_item.abbr, 1, width)
        else
          vim_item.abbr = vim_item.abbr .. string.rep(" ", width - #vim_item.abbr)
        end

        vim_item.menu = icons[vim_item.kind] or " "
        vim_item.kind = "(" .. vim_item.kind .. ")"
        return vim_item
      end,
    },
    experimental = {
      ghost_text = false,
    },
})

