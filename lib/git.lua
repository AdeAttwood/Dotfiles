local git = {}

---@class GitRepoConfig
---@field src string The source url to the repo
---@field target string The target directory where you want the repo to be cloned
---@field version string The git revision. Can be a sha, branch or tag

--- Tracks a git repo keeping it up to date with a revision. Will clone the
--- repo if its already cloned. Will then checkout the required revision.
---@param config GitRepoConfig
git.repo = function (config)
  assert(config.src ~= nil, "Git repo must have a source")
  assert(config.target ~= nil, "Git repo must have a target")
  assert(config.version ~= nil, "Git repo must have a version")

  if not configz.is_directory(config.target) then
    configz.run(string.format("git clone %s %s", config.src, config.target))
  end

  configz.run(string.format("cd %s && git pull && git checkout %s", config.target, config.version))
end

return git
