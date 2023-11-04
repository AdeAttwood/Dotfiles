local ts_utils = require "nvim-treesitter.ts_utils"

local function get_unquoted_node_text(node)
  local node_text = vim.treesitter.get_node_text(node, 0)
  local value, _ = string.gsub(node_text, '["]', "")
  return value
end

local function get_test_name_at_cursor()
  local node = ts_utils.get_node_at_cursor()
  local test_name = ""
  while node do
    if node:type() == "call_expression" then
      local named = ts_utils.get_named_children(node)
      local function_name = vim.treesitter.get_node_text(named[1], 0)
      if function_name == "describe" or function_name == "it" then
        test_name = get_unquoted_node_text(named[2]:child(1)) .. " " .. test_name
      end
    end

    node = node:parent()
  end

  local value, _ = string.gsub(test_name, "%s+$", "")
  return value
end

local function get_all_tests()
  local parser = vim.treesitter.get_parser(0, "javascript")
  local tree, _ = unpack(parser:parse())

  local function_name_query = vim.treesitter.query.parse(
    "javascript",
    [[((call_expression
        function: (identifier) @function_name
          arguments: (arguments (string) @test_name)
          (#match? @function_name "(it|describe)")))]]
  )

  local tests = {}
  local current_context = { tree }
  local prefix = {}
  for _, match, _ in function_name_query:iter_matches(tree:root(), 0) do
    local function_name_node, test_name_node = unpack(match)
    local function_name = get_unquoted_node_text(function_name_node)
    local test_name = get_unquoted_node_text(test_name_node)

    if function_name == "describe" then
      table.insert(current_context, 1, function_name_node:parent())
      table.insert(prefix, 1, test_name)
    end

    if function_name == "it" then
      if not vim.treesitter.is_ancestor(current_context[1], function_name_node) then
        table.remove(prefix, 1)
        table.remove(current_context, 1)
      end

      local test_value = test_name
      for _, v in ipairs(prefix) do
        test_value = v .. " " .. test_value
      end

      table.insert(tests, { content = test_value })
    end
  end

  return tests
end

local base_jest_command = "jest --maxWorkers=25\\% --silent"

local function run_at_cursor()
  local command = string.format("%s -t '%s' %s", base_jest_command, get_test_name_at_cursor(), vim.fn.expand "%")
  vim.cmd("Run " .. command)
end

local function run_file()
  local command = string.format("%s %s", base_jest_command, vim.fn.expand "%")
  vim.cmd("Run " .. command)
end

local function run_all()
  vim.cmd("Run " .. base_jest_command)
end

vim.api.nvim_create_user_command("Jest", run_all, { bang = true })
vim.api.nvim_create_user_command("JestFile", run_file, { bang = true })
vim.api.nvim_create_user_command("JestAtCursor", run_at_cursor, { bang = true })
vim.api.nvim_set_keymap("n", "tt", "<cmd>JestAtCursor<CR>", { nowait = true, silent = true })
vim.api.nvim_set_keymap("n", "tf", "<cmd>JestFile<CR>", { nowait = true, silent = true })
