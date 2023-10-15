local fs = {}

--- Read the content of a file and return the content
---@param file_path string
---@return boolean
---@return string
fs.read_file = function(file_path)
  local file = io.open(file_path, "rb")
  if file == nil then
    return false, ""
  end

  local content = file:read "*all"
  file:close()

  return true, content
end

--- Write content to a file replacing the existing content if the file already
--- exists. If not then the file will be created.
---@param file_path string
---@param content string
---@return boolean
fs.write_file = function(file_path, content)
  local file = io.open(file_path, "w+")
  if file == nil then
    return false
  end

  file:write(content)
  file:close()
  return true
end

return fs
