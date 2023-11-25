-- Links all of the bin files into `~/.local/bin`
configz.directory(os.getenv("HOME") .. "/.local/bin")
configz.run(string.format("ln -sf %s/site-modules/core/files/bin/* %s/.local/bin", os.getenv "PWD", os.getenv "HOME"))
