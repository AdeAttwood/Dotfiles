-- TODO(ade): Set up org capture into the website node dir
-- See https://github.com/AdeAttwood/Dotfiles/blob/86bf86d2010d7ec7b579e7b1c06632d5955f0a3c/site-modules/core/files/emacs/src/org.el#L107

require('orgmode').setup({
    org_agenda_files = {'~/Code/src/github.com/AdeAttwood/Website/data/*'},
})

-- Dont folt heading on load.
vim.cmd[[autocmd FileType org setlocal nofoldenable]]

require('orgmode').setup_ts_grammar()
