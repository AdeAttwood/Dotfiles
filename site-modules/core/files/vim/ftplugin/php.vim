"
" Set flods to be indent on php class files
"
if &ft == "php.class"
    call aa#fold#set_fold('indent')
else
    call aa#emmet#init()
endif
