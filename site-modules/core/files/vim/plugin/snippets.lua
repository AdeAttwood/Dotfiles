-- Add lua snippets from a yasnippet style snippet format. This is so I can
-- manage the snippet in file format rather than in json or lua. Supports
-- adding attributes that will be added to `context` of the luasnip. The body
-- of the snippets are in lsp-snippets format and will be run though the
-- `parse_snippet` function from luasnip
--
-- See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#snippet_syntax
-- See: https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md#snippets
--
local ls = require "luasnip"

-- Parse a line in the snippet. Will parse a key value attribute in the the line
-- formatted `# key: value` if a key is not found then the key will be `body`
-- and the hole line will be added to the value.
local function parse_snippet_line(line)
  -- This is the body separator for the metadata to the body of the snippet
  if line == "# --" then
    return { mode = "skip" }
  end

  local key = line:match "#%s+(.*):"
  -- If there is no key then its part of the body and it needs to be
  -- concatenated to the other lines
  if key == nil then
    return { key = "body", mode = "concat", value = line }
  end

  local value = line:match ":%s+(.*)$"

  -- Split the string on `,` if its the filetypes so we can add the snippet to
  -- multiple filetypes
  if key == "filetypes" then
    value = vim.split(value, ",")
  end

  return { key = key, value = value, mode = "set" }
end

-- Parses a hole snippet file into a snippet table that can be added as luasnip
-- snippet. Header key value attribute are parsed into a table and the body of
-- the snippet (anything after the "# --")  is added as the `body` as a table
-- of lines
local function parse_snippet_file(file_path)
  local file = io.open(file_path, "r")
  local snippet = {}
  if file == nil then
    return snippet
  end

  for line in file:lines() do
    local line_content = parse_snippet_line(line)
    if line_content.mode == "concat" then
      -- Set the key attribute if it dose not exists already we will get a nil
      -- error if we try to append to a key that is not in the snippet table
      if snippet[line_content.key] == nil then
        snippet[line_content.key] = {}
      end

      table.insert(snippet[line_content.key], line_content.value)
    elseif line_content.mode == "set" then
      snippet[line_content.key] = line_content.value
    end
  end

  return snippet
end

local snippets = {}
local paths = vim.split(vim.fn.glob(vim.fn.stdpath("config") .. "/snippets/**/*.snippet"), "\n")
for paths_index = 1, #paths do
  local file = paths[paths_index]
  local snippet = parse_snippet_file(file)

  local body = table.concat(snippet.body, "\n")
  snippet.body = nil

  if snippet.key ~= nil then
    snippet.trig = snippet.key
  end

  if snippet.description ~= nil then
    snippet.desc = snippet.description
  end

  local parsed = ls.parser.parse_snippet(snippet, body, {})
  for filetype_index = 1, #snippet.filetypes do
    local filetype = snippet.filetypes[filetype_index]
    if snippets[filetype] == nil then
      snippets[filetype] = {}
    end

    table.insert(snippets[filetype], parsed)
  end
end

for filetype, snippets_to_add in pairs(snippets) do
  ls.add_snippets(filetype, snippets_to_add)
end

ls.env_namespace("AA", {
  vars= {
    NAMESPACE = function ()
      local file_path = vim.fn.fnamemodify(vim.fn.expand("%:h"), ":~:.")
      return string.gsub(file_path, "[\\/]", ".");
    end
  }
})
