-- Dev Case is a command line tool to search and replace text using a regular
-- expression.
-- @see https://github.com/AdeAttwood/DevCase

-- Call dev_case binary to run the search and replace on some text
--
---@param search string
---@param replace string
---@param input string
---@return string[]
local function run_dev_case(search, replace, input)
  return vim.fn.systemlist { "dev_case", "--search", search, "--replace", replace, "--input", input }
end

-- Parses the options that are passed in from the nvim_create_user_command
-- callback. This can then be shared between the preview and the actual
-- command.
local function parse_opts(opts)
  local line1 = opts.line1
  local line2 = opts.line2

  local search = opts.fargs[1]
  if not search then
    search = ""
  end

  local replace = opts.fargs[2]
  if not replace then
    replace = ""
  end

  local buf = vim.api.nvim_get_current_buf()
  local current_content = vim.api.nvim_buf_get_lines(buf, line1 - 1, line2, false)
  local new_content = current_content
  if current_content ~= nil and search ~= "" then
    new_content = run_dev_case(search, replace, table.concat(current_content, "\n"))
  end

  return { line1 = line1, line2 = line2, current_content = current_content, new_content = new_content }
end

-- The preview function for the dev_case command. This will show the user what
-- will be searched and replaced as they type.
local function dev_case_preview_callback(opts, _, _)
  vim.cmd "hi clear Substitute"

  local options = parse_opts(opts)
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, options.line1 - 1, options.line2, false, options.new_content)

  return 1
end

-- Run the dev_case command with the given options. This will basically commit
-- what they are seeing in the preview.
local function dev_case_callback(opts)
  local options = parse_opts(opts)

  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_set_lines(buf, options.line1 - 1, options.line2, false, options.new_content)
end

vim.api.nvim_create_user_command(
  "S",
  dev_case_callback,
  { nargs = "+", range = 1, preview = dev_case_preview_callback }
)
