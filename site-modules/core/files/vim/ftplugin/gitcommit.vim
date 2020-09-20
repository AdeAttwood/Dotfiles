"
" Git commit message specific settings
"
"

"
" Enable spell by default
"
setlocal spell

"
" Make diff folds the default fold for commits. When running `git commit -v`
" all the file diffs will be folded
"
call aa#fold#set_fold('diff')
