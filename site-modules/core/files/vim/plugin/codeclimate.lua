-- Plugin to add the output of codeclimate issues into vim.
--
-- https://github.com/codeclimate/codeclimate
--
-- For this to work you must install the codeclimate cli tool and run the below
-- command to create the `/tmp/cc.json` where all the issues are stored. Once
-- this file is created vims diagnostics will be populated with codeclimate
-- issues.
--
-- $ codeclimate analyze -f json > /tmp/cc.json
--
-- If you have duplication you can run `:CodeClimateOtherLocations` with your
-- cousor on the issue and the quickfix list will be populated with the other
-- locations the duplication exists.
--

local codeclimate_namespace = vim.api.nvim_create_namespace "codeclimate"
local codeclimate_auto_command_group = vim.api.nvim_create_augroup("codeclimate", { clear = true })

-- Get the relative path of a `buffer` to the vim current working directory
local function get_relative_path(buffer)
  return vim.api.nvim_buf_get_name(buffer):sub(#vim.fn.getcwd() + 2, -1)
end

-- Get the content of a `line_number` from a `file_path`. This will read the
-- file and return content at line `n` If the file can not be opened a empty
-- string is returned.
local function read_line(file_path, line_number)
  local file = io.open(file_path, "r")
  if file == nil then
    return ""
  end

  local index = 0
  while index < line_number do
    file:read()
    index = index + 1
  end

  local line = file:read()
  file:close()

  return line
end

-- Populates the vim diagnostics with the codeclimate issues from '/tmp/cc.json'
local function codeclimate_add_diagnostics()
  local file = io.open("/tmp/cc.json", "rb")
  if not file then
    return nil
  end
  local issues = vim.json.decode(file:read "*a")

  local buffer = vim.api.nvim_win_get_buf(0)
  local buffer_name = get_relative_path(buffer)
  local diagnostics = {}

  for index = 1, #issues do
    if issues[index]["location"] ~= nil and issues[index]["location"]["path"] == buffer_name then
      local start_line = 0
      local end_line = 0

      if issues[index]["location"]["lines"] ~= nil then
        start_line = issues[index]["location"]["lines"]["begin"] - 1
        end_line = issues[index]["location"]["lines"]["end"] - 1
      elseif issues[index]["location"]["positions"] ~= nil then
        -- TODO(ade): We get column positions here so this can be implemented
        start_line = issues[index]["location"]["positions"]["begin"]["line"] - 1
        end_line = issues[index]["location"]["positions"]["end"]["line"] - 1
      else
        goto continue
      end

      -- Limit the number of lines an issue spans to x amount of lines. If we
      -- don't do this then some issues will highlight the whole file and its
      -- really off putting.
      if (end_line - start_line) > 10 then
        end_line = start_line
      end

      table.insert(diagnostics, {
        source = "Code Climate",
        lnum = start_line,
        col = 0,
        end_lnum = end_line,
        end_col = 0,
        message = issues[index]["description"],
        severity = vim.diagnostic.severity.ERROR,
        user_data = {
          other_locations = issues[index]["other_locations"],
        },
      })

      ::continue::
    end
  end

  vim.diagnostic.set(codeclimate_namespace, buffer, diagnostics)
end

local function other_to_quick_fix()
  local point = vim.api.nvim_win_get_cursor(0)
  local diagnostics = vim.diagnostic.get(0, { lnum = point[1] - 1 })
  local issues = {}

  -- TODO(ade): Add in a message to say this issues dose not have any other
  -- locations
  if #diagnostics == 0 or #diagnostics[1]["user_data"]["other_locations"] == nil then
    return
  end

  local diagnostic = diagnostics[1]
  table.insert(issues, {
    text = vim.api.nvim_buf_get_lines(diagnostic["bufnr"], diagnostic["lnum"], diagnostic["lnum"] + 1, true)[1],
    filename = get_relative_path(diagnostic["bufnr"]),
    lnum = diagnostic["lnum"] + 1,
    end_lnum = diagnostic["end_lnum"],
  })

  for other_index = 1, #diagnostic["user_data"]["other_locations"] do
    local other_location = diagnostic["user_data"]["other_locations"][other_index]
    table.insert(issues, {
      text = read_line(other_location["path"], other_location["lines"]["begin"] - 1),
      filename = other_location["path"],
      lnum = other_location["lines"]["begin"],
      end_lnum = other_location["lines"]["end"],
    })
  end

  vim.fn.setqflist({}, "r", { title = "Code Climate Other Locations", items = issues })
  vim.cmd "copen"
end

vim.api.nvim_create_autocmd("BufEnter", {
  group = codeclimate_auto_command_group,
  desc = "",
  pattern = "*",
  callback = function()
    codeclimate_add_diagnostics()
  end,
})

vim.api.nvim_create_user_command("CodeClimateOtherLocations", other_to_quick_fix, { bang = true, desc = "" })
