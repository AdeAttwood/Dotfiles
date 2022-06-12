local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local i = ls.insert_node
local f = ls.function_node
local t = ls.text_node
local d = ls.dynamic_node
local fmt = require('luasnip.extras.fmt').fmt

function p (trig, desc, snip)
    return ls.parser.parse_snippet(
        { trig = trig, dscr = desc },
        table.concat(snip, '\n')
    )
end

ls.config.setup({
    store_selection_keys="<Tab>",
    update_events="InsertLeave,TextChangedI",
})

ls.add_snippets("all", {
    p('todo', 'Todo comment', { 'TODO(${1:ade}): $0' })
})

ls.add_snippets("org", {
    p('org-header', 'Org mode header block', { 
        '#+TITLE: $0',
        '#+AUTHOR: Ade Attwood',
        '#+EMAIL: hello@adeattwood.co.uk',
        '#+DATE: $CURRENT_YEAR-$CURRENT_MONTH-${CURRENT_DATE}' 
    })
})


ls.add_snippets("php", { 
    p('#!', 'Shebang', { '#!/usr/bin/env php' }),
    p( '/**', 'Block  comment', {
        '/**', 
        ' * ${0}', 
        ' */'
    }),
    s(
      {trig = 'this', dscr = 'This shorthand'},
      fmt("$this->{}", {
          i(0),
      })
    ),
    s(
      {trig = 'ai', dscr = 'Array item'},
      fmt("'{}' => {}", {
          i(1),
          i(0),
      })
    )
})

local js_ts = {
    p('#!', 'Shebang', { '#!/usr/bin/env node' }),
    s(
        {trig = 'import', dscr = 'Import statement'},
        fmt("import {} from '{}'", {
            i(0),
            i(1)
        })
    ),

    s({trig = 'fn', dscr = 'Function'}, {
        t('function '),
        i(1),
        t('('),
        i(2),
        t(')'),
        t({' {', '\t'}),
        i(0),
        t({'', '}'})
    }),

    s({trig = 'useState', dscr = 'React useState hook'}, {
        t('const ['),
        i(1, 'state'),
        t(', '),
        f(function (args) 
            if args[1] == nil then
                return ''
            end

            return 'set' .. args[1][1]:gsub("^%l", string.upper)
        end, {1}),
        t('] = React.useState('),
        i(0),
        t(');')
    }),
    p('log', 'Console log statement', { 'console.log(${0});' })
}



ls.add_snippets("typescriptreact", js_ts)
ls.add_snippets("typescript", js_ts)
