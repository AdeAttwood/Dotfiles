local data = {}

data.lookup = function(scope, key)
  local ok, module = pcall(require, string.format("data.%s", scope))
  if not ok then
    configz.error(string.format("data.%s does not exist", scope))
    return ""
  end

  if not module[key] then
    configz.error(string.format("data.%s.%s does not exist", scope, key))
    return ""
  end

  return module[key]
end

return data
