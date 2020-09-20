function aa#emmet#init()
    "
    " Don't install emmet globally we want to only use it on selected languages
    "
    let g:user_emmet_install_global = 0

    "
    " Set Ctrl-E emmet trigger
    "
    "let g:user_emmet_leader_key='<C-E>'

    "
    " Add the emmet package
    "
    packadd emmet-vim

    "
    " Install the package
    "
    EmmetInstall
endfunction
