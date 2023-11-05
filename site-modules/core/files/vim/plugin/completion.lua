local cmp = require'cmp'
local luasnip = require'luasnip'

require("copilot").setup({
  suggestion = { enabled = false },
  panel = { enabled = false },
})

require("copilot_cmp").setup()


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

cmp.setup({
    mapping = cmp.mapping.preset.insert({
        ['<C-j>'] = cmp.get_config().mapping['<Down>'],
        ['<C-k>'] = cmp.get_config().mapping['<Up>'],
        ['<C-l>'] = function() luasnip.jump(1) end,
        ['<C-h>'] = function() luasnip.jump(-1) end,

        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
        ["<Tab>"] = cmp.mapping(function(fallback)
            if luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end, { "i", "s" }),

        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
    }),
    sources = {
        { name = "luasnip" },
        { name = "copilot" },
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
        local width = 40
        if #vim_item.abbr > width then
          vim_item.abbr = string.sub(vim_item.abbr, 1, width)
        else
          vim_item.abbr = vim_item.abbr .. string.rep(" ", width - #vim_item.abbr)
        end

        vim_item.menu = icons[vim_item.kind] or " "
        vim_item.kind = "(" .. entry.source.name .. ")"
        return vim_item
      end,
    },
    experimental = {
      ghost_text = true,
    },
})

cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" },
  }),
})
