local v_cache = require "lib.v-cache"

local git_sha = "adc02c2976a3d90263b2c7ea7b10ed88ccdd06b7";

if v_cache.is_installed("fira-code", git_sha) then
  return
end

local weights = { "Light", "Regular", "Medium", "SemiBold", "Bold", "Retina" }

for _, value in ipairs(weights) do
  configz.download(os.getenv("HOME") .. "/.local/share/fonts/FiraCodeNerdFont-" .. value .. ".ttf", {
    url = "https://github.com/ryanoasis/nerd-fonts/raw/" .. git_sha .. "/patched-fonts/FiraCode/" .. value .. "/FiraCodeNerdFont-" .. value .. ".ttf"
  })

  configz.download(os.getenv("HOME") .. "/.local/share/fonts/FiraCodeNerdFontMono-" .. value .. ".ttf", {
    url = "https://github.com/ryanoasis/nerd-fonts/raw/" .. git_sha .. "/patched-fonts/FiraCode/" .. value .. "/FiraCodeNerdFontMono-" .. value .. ".ttf"
  })
end

v_cache.install("fira-code", git_sha)
