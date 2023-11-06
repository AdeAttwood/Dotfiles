-- The Ollama plugin provides a simple way to interact with an Ollama language
-- model from within Neovim.
--
-- See: https://github.com/jmorganca/ollama
--
-- To use the Ollama plugin, use the `:Ollama` command to initalise the plugin.
-- This will open up a new tab with a prompt and results buffer so you can have
-- an interactive conversation with a Ollama language model.
--
-- From inside the prompt buffer you can use the `:OllamaSend` command or the
-- key mapping <leader>s in insert mode. This will send the prompt off to the
-- model and stream the results into the results buffer.
--
-- This plugin uses regular buffers and tabs so you can use them like any
-- other. For switching between your code and the prompt you can use `gt` for
-- switching between tabs. To close up the Ollama prompt you can use `tabc`
-- from the prompt to close the tab and reopen it with `:Ollama` to start a new
-- conversation context.
--
local ollama_api_url = "http://localhost:11434"
local ollama_model = "codellama"
local ollama_context = {}

local function find_buffer_by_name(name)
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    local buf_name = vim.api.nvim_buf_get_name(buf)
    if buf_name == name then
      return buf
    end
  end

  return -1
end

local function process_line(results_buffer, line)
  local ok, chunk = pcall(vim.json.decode, line)
  if not ok then
    return false
  end

  if chunk.response and chunk.response == "\n" then
    vim.api.nvim_buf_set_text(results_buffer, -1, -1, -1, -1, { "", "" })
  elseif chunk.response then
    vim.api.nvim_buf_set_text(results_buffer, -1, -1, -1, -1, { chunk.response })
  end

  if chunk.done then
    ollama_context = chunk.context
  end

  return true
end

local function ollama_send()
  local prompt_buffer = vim.api.nvim_get_current_buf()
  local results_buffer = find_buffer_by_name "/tmp/ollama-response.md"
  -- TODO(AdeAttwood): Validate we are in the correct buffer before we send any prompts
  -- local name = vim.api.nvim_buf_get_name(prompt_buffer)

  -- Grab your input prompt for later
  local prompt_lines = vim.api.nvim_buf_get_lines(prompt_buffer, 0, -1, false)

  -- Update the results buffer with your prompt and then start off the response.
  vim.api.nvim_buf_set_text(results_buffer, -1, -1, -1, -1, prompt_lines)
  local ollama_prompt = { "", "", string.format("<<< Ollama %s", ollama_model), "" }
  vim.api.nvim_buf_set_text(results_buffer, -1, -1, -1, -1, ollama_prompt)

  -- Clear the prompt buffer so it's ready for the next prompt
  vim.api.nvim_buf_set_lines(prompt_buffer, 0, -1, false, {})

  local buffer = ""
  local job_id = vim.fn.jobstart(string.format("curl -X POST %s/api/generate --data-binary @-", ollama_api_url), {
    on_stdout = function(_, data_array, _)
      for _, data in ipairs(data_array) do
        -- Buffer the output of the curl process, we will not always get this
        -- at the end of a line. This will then not be valid json and we will
        -- need to concat it with the previous chunk, to make up the hole line
        -- in the ndjson response.
        buffer = buffer .. data
        if vim.endswith(buffer, "}") then
          process_line(results_buffer, buffer)
          buffer = ""
        end
      end
    end,
    on_exit = function()
      vim.api.nvim_buf_set_text(results_buffer, -1, -1, -1, -1, { "", "", ">>> You", "", "" })
    end,
  })

  -- Send the json curl body as stdin to the curl process. This is so we don't
  -- have to worry about all the shell escaping the json body in the jobstart
  -- command.
  --
  -- See: https://github.com/jmorganca/ollama/blob/main/docs/api.md#generate-a-completion
  vim.fn.chansend(
    job_id,
    vim.json.encode {
      model = ollama_model,
      context = ollama_context,
      prompt = table.concat(prompt_lines, "\n"),
    }
  )

  -- Close stdin so the curl process knows we are done sending it data.
  vim.fn.chanclose(job_id, "stdin")
end

local function ollama_init()
  -- Reset the context so we get a new convo
  ollama_context = {}

  -- Open the response buffer and add the first part of the response
  vim.cmd [[tab new /tmp/ollama-response.md]]
  vim.api.nvim_buf_set_text(0, -1, -1, -1, -1, { ">>> You", "", "" })

  -- Set up the propt buffer read for the user to start chatting
  vim.cmd [[botright split /tmp/ollama-prompt.md | resize 14]]
  vim.api.nvim_buf_create_user_command(0, "OllamaSend", ollama_send, { bang = true })
  vim.api.nvim_buf_set_keymap(0, "n", "<leader>s", ":OllamaSend<CR>", {})
end

vim.api.nvim_create_user_command("Ollama", ollama_init, { bang = true })
