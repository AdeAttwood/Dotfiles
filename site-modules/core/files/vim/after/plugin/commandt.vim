

function! s:GotoOrOpen(command_and_args) abort
  let l:command_and_args = split(a:command_and_args, '\v^\w+ \zs')
  let l:command = l:command_and_args[0]
  let l:file = l:command_and_args[1]

  "
  " Fix for switching to terminals in neo vim
  "
  if match(l:file, 'term:/') >= 0
    execute 'buffer ' . substitute(l:file, 'term:/', 'term://.//', '')
    return
  endif

  if buffer_exists(l:file) && match(l:command, 'edit') >= 0
    execute 'buffer ' . l:file
  else
    execute l:command . l:file
  endif
endfunction

command! -nargs=+ CommandTOpen call s:GotoOrOpen(<q-args>)

let g:CommandTAcceptSelectionCommand = 'CommandTOpen edit'
let g:CommandTAcceptSelectionTabCommand = 'CommandTOpen tabe'
let g:CommandTAcceptSelectionSplitCommand = 'CommandTOpen sp'
let g:CommandTAcceptSelectionVSplitCommand = 'CommandTOpen vs'
