vim.api.nvim_create_user_command("PrrGoToLine", function()
  local content = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  local line, column = unpack(vim.api.nvim_win_get_cursor(0))

  local target_file = ""
  local target_line = 0

  for i = 1, line do
    local line_content = content[i]
    local file_match = line_content:match "> diff .* b/(.*)"
    if file_match ~= nil then
      target_file = file_match
    end

    local line_match = line_content:match "+(%d+)"
    if line_match ~= nil then
      target_line = line_match
    elseif line_content:match "> (-).*" == nil then
      target_line = target_line + 1
    end
  end

  vim.cmd("e " .. target_file)
  vim.fn.cursor(target_line - 1, column)
end, { bang = true, desc = "Open the current file at the poin in a prr buffer" })
