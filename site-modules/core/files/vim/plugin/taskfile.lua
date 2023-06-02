local libivy = require "ivy.libivy"

local get_tasks = function()
  local task_output, _ = vim.fn.system { "task", "-j", "--list" }
  local output = vim.json.decode(task_output)

  return output.tasks
end

vim.api.nvim_create_user_command("IvyTaskfile", function()
  local task_list = get_tasks()
  vim.ivy.run("Taskfile", function(input)
    local tasks = {}
    for _, task in ipairs(task_list) do
      local content = task.name .. " » " .. task.desc
      local score = libivy.ivy_match(input, content)
      if score > -200 then
        table.insert(tasks, { content = content, score = score })
      end
    end

    table.sort(tasks, function(a, b)
      return a.score < b.score
    end)

    return tasks
  end, function(item)
    local name, _ = unpack(vim.split(item, " » "))
    vim.fn.jobstart({ "task", name }, {
      on_stdout = function(_, data)
        print(table.concat(data, "\n"))
      end,
      on_exit = function(_, data)
        if data ~= 0 then
          print("Task " .. name .. " failed")
        else
          print("Task " .. name .. " succeeded")
        end
      end,
    })
  end)
end, {})
