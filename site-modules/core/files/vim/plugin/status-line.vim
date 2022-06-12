" The most basic status line ever
function GitStatus()
    return trim(system("git diff --shortstat 2> /dev/null | awk '{print \"+\"$4\" -\"$6}'"))
endfunction

function GitBranch()
    return trim(system("git rev-parse --abbrev-ref HEAD 2> /dev/null || echo 'no-git'"))
endfunction

set statusline=\ %y%r\ %m%f:%l:%c%=[%{GitBranch()}]\ %{GitStatus()}\ 
