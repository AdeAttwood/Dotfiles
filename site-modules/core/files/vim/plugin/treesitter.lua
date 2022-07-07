require'nvim-treesitter.configs'.setup {
    indent = { enable = true },
    rainbow = { enable = true },
    highlight = {
        -- `false` will disable the whole extension
        enable = true,
        additional_vim_regex_highlighting = true,
    },
    ensure_installed = { 
        "typescript", "javascript", "tsx", "php", "html", "go", "org"
    },
}