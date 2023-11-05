local git = require "lib.git"

local nvim_dir = os.getenv "HOME" .. "/.config/nvim"
local nvim_plugin_dir_start = nvim_dir .. "/pack/bundle/start"
local nvim_plugin_dir_opt = nvim_dir .. "/pack/bundle/opt"

if not configz.is_directory(nvim_dir) then
  configz.directory(nvim_dir)
end

-- Link the main init file
if not configz.is_file(nvim_dir .. "/init.vim") then
  configz.link(nvim_dir .. "/init.vim", {
    source = os.getenv "PWD" .. "/site-modules/core/files/vim/init.vim",
  })
end

-- Link all of the configs into the nvim config directory
local dirs_to_link = { "after", "ftplugin", "snippets", "plugin", "spell" }
for _, dir in ipairs(dirs_to_link) do
  local dir_to_link = nvim_dir .. "/" .. dir
  if not configz.is_directory(dir_to_link) then
    configz.link(dir_to_link, {
      source = os.getenv "PWD" .. "/site-modules/core/files/vim/" .. dir_to_link,
    })
  end
end

local start_plugins = {
  ["auto-pairs"] = { url = "https://github.com/jiangmiao/auto-pairs.git" },
  ["base16-vim"] = { url = "https://github.com/tinted-theming/base16-vim.git", revision = "main" },
  ["cmp_luasnip"] = { url = "https://github.com/saadparwaiz1/cmp_luasnip.git" },
  ["cmp-buffer"] = { url = "https://github.com/hrsh7th/cmp-buffer.git", revision = "main" },
  ["cmp-nvim-lsp"] = { url = "https://github.com/hrsh7th/cmp-nvim-lsp.git", revision = "main" },
  ["cmp-path"] = { url = "https://github.com/hrsh7th/cmp-path.git", revision = "main" },
  ["Comment.nvim"] = { url = "https://github.com/numToStr/Comment.nvim.git" },
  ["ferret"] = { url = "https://github.com/wincent/ferret.git" },
  ["indent-line"] = { url = "https://github.com/Yggdroot/indentLine.git" },
  ["LuaSnip"] = { url = "https://github.com/L3MON4D3/LuaSnip.git" },
  ["nvim-cmp"] = { url = "https://github.com/hrsh7th/nvim-cmp.git", revision = "main" },
  ["nvim-lint"] = { url = "https://github.com/mfussenegger/nvim-lint.git" },
  ["nvim-lspconfig"] = { url = "https://github.com/neovim/nvim-lspconfig.git" },
  ["nvim-treesitter"] = { url = "https://github.com/nvim-treesitter/nvim-treesitter.git" },
  ["orgmode"] = { url = "https://github.com/nvim-orgmode/orgmode.git" },
  ["vim-puppet"] = { url = "https://github.com/rodjek/vim-puppet.git" },
  ["vim-surround"] = { url = "https://github.com/tpope/vim-surround.git" },
  ["vim-tmux-navigator"] = { url = "https://github.com/christoomey/vim-tmux-navigator.git" },
  ["vim-fugitive"] = { url = "https://github.com/tpope/vim-fugitive.git" },
  ["vim-rhubarb"] = { url = "https://github.com/tpope/vim-rhubarb.git" },
  ["conjure"] = { url = "https://github.com/Olical/conjure.git" },
  ["cmp-cmdline"] = { url = "https://github.com/hrsh7th/cmp-cmdline.git", revision = "main" },
  ["copilot"] = { url = "https://github.com/zbirenbaum/copilot.lua.git" },
  ["copilot-cmp"] = { url = "https://github.com/zbirenbaum/copilot-cmp.git" },
}

local opt_plugins = {
  ["command-t"] = { url = "https://github.com/wincent/command-t.git" },
}

-- Install all of the plugins I want to start at boot
for plugin, config in pairs(start_plugins) do
  git.repo {
    src = config.url,
    target = nvim_plugin_dir_start .. "/" .. plugin,
    version = config.revision or "master",
  }
end

-- Install all of the plugins I want to start manually
for plugin, config in pairs(opt_plugins) do
  git.repo {
    src = config.url,
    target = nvim_plugin_dir_opt .. "/" .. plugin,
    version = config.revision or "master",
  }
end
