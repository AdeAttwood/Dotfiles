function! aa#languagetool#init()
    let g:languagetool_jar = "~/.local/share/LanguageTool-4.6/languagetool-commandline.jar"
    let g:languagetool_lang = 'en-GB'

    packadd vim-LanguageTool
endfunction
