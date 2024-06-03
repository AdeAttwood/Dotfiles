local alternate_type = {
  SPEC = "spec",
}

local alternates = {
  -- Market dojo spec files for components. We need to remove the component
  -- directory to map them to the correct folder map.
  ["app/javascript/components/(.*)%.(jsx?)$"] = {
    { type = alternate_type.SPEC, file = "spec/react/%1.spec.%2", default = true },
  },
  -- Market dojo spec files for other javascript stuff
  ["app/javascript/(.*)%.(jsx?)$"] = {
    { type = alternate_type.SPEC, file = "spec/react/%1.spec.%2", default = true },
  },
  -- Map js/ts files to spec files from the application dir
  ["app/(.*)%.([jt]sx?)$"] = {
    { type = alternate_type.SPEC, file = "spec/%1.test.%2" },
    { type = alternate_type.SPEC, file = "test/%1.test.%2" },
    { type = alternate_type.SPEC, file = "tests/%1.test.%2" },
    { type = alternate_type.SPEC, file = "spec/%1.spec.%2" },
    { type = alternate_type.SPEC, file = "tests/%1.spec.%2", default = true },
  },
  -- Map js/ts files to spec files from the source dir
  ["src/(.*)%.([jt]sx?)$"] = {
    { type = alternate_type.SPEC, file = "spec/%1.test.%2" },
    { type = alternate_type.SPEC, file = "test/%1.test.%2" },
    { type = alternate_type.SPEC, file = "tests/%1.test.%2" },
    { type = alternate_type.SPEC, file = "spec/%1.spec.%2" },
    { type = alternate_type.SPEC, file = "tests/%1.spec.%2", default = true },
  },

  ["(.*)%.lua$"] = {
    { type = alternate_type.SPEC, file = "spec/%1_spec.lua", default = true },
  },
}

local function file_exists(filename)
  local file = io.open(filename, "r")
  if file then
    io.close(file)
    return true
  else
    return false
  end
end

local function find_alt(
  _, --[[ type ]]
  file_name
)
  local default_alternate = nil

  for key, match_list in pairs(alternates) do
    for _, match in ipairs(match_list) do
      local alternate, found = string.gsub(file_name, key, match.file)
      if found > 0 then
        if file_exists(alternate) then
          return alternate
        elseif match.default then
          default_alternate = alternate
        end
      end
    end

    -- Return the first default match we find.
    if default_alternate ~= nil then
      return default_alternate
    end
  end

  -- Not alternative found
  return nil
end

vim.api.nvim_create_user_command("GoToSpec", function()
  local buffer_name = vim.fn.fnamemodify(vim.fn.expand "%", ":p:~:.")
  local spec = find_alt(nil, buffer_name)
  if spec ~= nil then
    vim.cmd("edit " .. spec)
    return
  end

  print("ERR: No alternate file found for " .. buffer_name)
end, { bang = true, desc = "Go to the spec file for the current buffer" })

vim.keymap.set("n", "gs", ":GoToSpec<CR>", { silent = true })
