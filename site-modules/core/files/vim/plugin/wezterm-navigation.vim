" Maps <C-h/j/k/l> to switch vim splits in the given direction. If there are
" no more windows in that direction, forwards the operation to wezterm.
" Additionally, <C-\> toggles between last active vim splits/wezterm panes.

if exists("g:loaded_wezterm_navigator") || &cp || v:version < 700
  finish
endif
let g:loaded_wezterm_navigator = 1

function! s:DirectionToKey(direction)
    if a:direction ==# 'Up'
        return 'k'
    elseif a:direction ==# 'Down'
        return 'j'
    elseif a:direction ==# 'Left'
        return 'h'
    elseif a:direction ==# 'Right'
        return 'l'
    else
        return 'p'
    endif
endfunction

function! s:VimNavigate(direction)
  try
    execute 'wincmd ' . s:DirectionToKey(a:direction)
  catch
    echohl ErrorMsg | echo 'E11: Invalid in command-line window; <CR> executes, CTRL-C quits: wincmd k' | echohl None
  endtry
endfunction

if !get(g:, 'wezterm_navigator_no_mappings', 0)
  nnoremap <silent> <c-h> :<C-U>WestermNavigateLeft<cr>
  nnoremap <silent> <c-j> :<C-U>WestermNavigateDown<cr>
  nnoremap <silent> <c-k> :<C-U>WestermNavigateUp<cr>
  nnoremap <silent> <c-l> :<C-U>WestermNavigateRight<cr>
  nnoremap <silent> <c-\> :<C-U>WestermNavigatePrevious<cr>
endif

if empty($WEZTERM_PANE)
  command! WestermNavigateLeft call s:VimNavigate('h')
  command! WestermNavigateDown call s:VimNavigate('j')
  command! WestermNavigateUp call s:VimNavigate('k')
  command! WestermNavigateRight call s:VimNavigate('l')
  command! WestermNavigatePrevious call s:VimNavigate('p')
  finish
endif

command! WestermNavigateLeft call s:WeztermAwareNavigate('Left')
command! WestermNavigateDown call s:WeztermAwareNavigate('Down')
command! WestermNavigateUp call s:WeztermAwareNavigate('Up')
command! WestermNavigateRight call s:WeztermAwareNavigate('Right')
command! WestermNavigatePrevious call s:WeztermAwareNavigate('Prev')

let s:pane_position_from_direction = {'h': 'left', 'j': 'bottom', 'k': 'top', 'l': 'right'}

function! s:weztermOrTmateExecutable()
  return "wezterm"
endfunction

function! s:weztermSocket()
  " The socket path is the first value in the comma-separated list of $wezterm.
  return split($wezterm, ',')[0]
endfunction

let s:wezterm_is_last_pane = 0
augroup wezterm_navigator
  au!
  autocmd WinEnter * let s:wezterm_is_last_pane = 0
augroup END

function! s:NeedsVitalityRedraw()
  return exists('g:loaded_vitality') && v:version < 704 && !has("patch481")
endfunction

function! s:ShouldForwardNavigationBackToWezterm(wezterm_last_pane, at_tab_page_edge)
endfunction

function! s:ShouldForwardNavigationBackToWezterm(wezterm_last_pane, at_tab_page_edge)
  return a:wezterm_last_pane || a:at_tab_page_edge
endfunction

function! s:WeztermCommand(args)
  let cmd = 'wezterm cli ' . a:args
  let l:x=&shellcmdflag
  let retval=system(cmd)
  let &shellcmdflag=l:x
  return retval
endfunction

function! s:WeztermAwareNavigate(direction)
  let nr = winnr()
  let wezterm_last_pane = (a:direction == 'p' && s:wezterm_is_last_pane)
  if !wezterm_last_pane
    call s:VimNavigate(a:direction)
  endif
  let at_tab_page_edge = (nr == winnr())
  " Forward the switch panes command to wezterm if:
  " a) we're toggling between the last wezterm pane;
  " b) we tried switching windows in vim but it didn't have effect.
  if s:ShouldForwardNavigationBackToWezterm(wezterm_last_pane, at_tab_page_edge)
    let args = 'activate-pane-direction ' . shellescape(a:direction)

    silent call s:WeztermCommand(args)
    if s:NeedsVitalityRedraw()
      redraw!
    endif
    let s:wezterm_is_last_pane = 1
  else
    let s:wezterm_is_last_pane = 0
  endif
endfunction
