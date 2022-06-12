" Open a file the same way you use :edit. This function will give you a prompt
" for a file with the default value of the directory the current file is in.
function s:open()
    let file_name = expand(input("Open file: ", expand("%:h") . "/", "file"))
    execute 'edit ' . file_name
endfunction

command! Open call s:open()

" Rename the current file on disk and remove all instances of it in vim and
" then switch to the new buffer.
function s:aa_rename()
    let file_name = expand(input("New file name: ", expand("%:h") . "/", "file"))
    if isdirectory(file_name) || filereadable(file_name)
        echoerr "File '" . file_name . "' already exists unable to rename the file"
        return
    endif

    " Rename the current file to the new name
    let current_file = expand("%")
    call rename(current_file, file_name)
    " Remove the old buffer and remove it from the alternate file list and the
    " jump list so you cant get back to this buffer with <C-o> or <C-^>
    execute "bwipeout " . current_file
    " Open the new file as a buffer
    execute "edit " . file_name
endfunction

command! Rename call s:aa_rename()

" Copy the current buffer. This is the same as `saveas` with some checks
" around the new name of the file to prevent overriteing files that already
" exist.
function s:aa_copy()
    let file_name = expand(input("New file name: ", expand("%:h") . "/", "file"))
    if isdirectory(file_name) || filereadable(file_name)
        echoerr "File '" . file_name . "' already exists unable to rename the file"
        return
    endif

    " Rename the current file to the new name. This is run with `silent` to
    " stop the write message from getting put in the status line.
    silent execute "saveas " . file_name
endfunction

command! Copy call s:aa_copy()
