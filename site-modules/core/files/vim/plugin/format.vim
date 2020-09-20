if exists('g:vim_formatter_loaded')
  finish
end

let g:vim_formatter_loaded = 1

"
" Set the global formatters definitions variable
"
if !exists('g:formatters')
    let g:formatters = {}
endif

"
" Get the prettier config for the current project
"
if filereadable('./.prettierrc.js')
    let s:prettier_config = '.prettierrc.js'
elseif filereadable('./.prettierrc')
    let s:prettier_config = '.prettierrc'
else
    let s:prettier_config = '~/.dotfiles/.prettierrc.js'
endif

"
" Set the base prettier command for formatting. This can be used on most of
" the file formats
"
let s:base_prettier_command = 'prettier --config ' . s:prettier_config . ' --stdin --stdin-filepath /tmp/formatted.'

"
" Typescript formatter definition
"
let g:formatters['typescript'] = s:base_prettier_command . 'ts'
let g:formatters['typescript.jsx'] = s:base_prettier_command . 'tsx'

"
" Javascript formatter definition
"
let g:formatters['javascript'] = s:base_prettier_command . 'js'
let g:formatters['javascript.jsx'] = s:base_prettier_command . 'jsx'

"
" HTML formatter definition
"
let g:formatters['html'] = s:base_prettier_command . 'html'

"
" Styling formatter definition
"
let g:formatters['css'] = s:base_prettier_command . 'css'
let g:formatters['scss'] = s:base_prettier_command . 'scss'
let g:formatters['less'] = s:base_prettier_command . 'less'

"
" JSON formatter definition
"
let g:formatters['json'] = s:base_prettier_command . 'json'

"
" YAML formatter definition
"
let g:formatters['yaml'] = s:base_prettier_command . 'yml'

"
" Markdown formatter definition
"
let g:formatters['markdown'] = s:base_prettier_command . 'md'

"
" Puppet formatter definition
"
let g:formatters['puppet'] = 'cat - > /tmp/format.pp && puppet-lint --no-autoloader_layout-check --fix /tmp/format.pp >/dev/null 2>&1 && cat /tmp/format.pp'

"
" PHP formatter definition
"
" This will use the `ruleset.xml` in the root of the project if one can be
" found. If not then it will fall back to use psr2 coding standards
"
if filereadable('./ruleset.xml')
    let g:formatters['php'] = 'phpcbf --standard=./ruleset.xml -'
    let g:formatters['php.class'] = 'phpcbf --standard=./ruleset.xml -'
else
    let g:formatters['php'] = 'phpcbf --standard=psr2 -'
    let g:formatters['php.class'] = 'phpcbf --standard=psr2 -'
endif

"
" Define if you want the plugin to format on save
"
if !exists('g:formatter_on_save')
    let g:formatter_on_save = 1
endif

"
" Set the plugin to debug mode
"
if !exists('g:formatter_debug')
    let g:formatter_debug = 1
endif

"
" Variable for enabling and disabling the formatters
"
if !exists('g:formatter_enabled')
    let g:formatter_enabled = 1
endif

"
" Format the current buffers text biased on the file type and the formatters
" defined
"
function s:format()
    "
    " Exit is formatting has been disabled
    "
    if !g:formatter_enabled
        return
    endif

    "
    " Trim white space before formatting
    "
    %s/\s\+$//e

    "
    " Format the buffers content
    "
    let l:lines = s:format_text(join(getline(1, '$'), "\n"), &filetype)

    "
    " Replace the lines of the formatted text with the text in the buffer
    "
    call aa#buffer#Overwrite(1, line('$'), lines)
endfunction

"
" Formats defined lines biased on the type defined
"
" a:lines
"   The text you want to format
"
" a:type
"   The type formatter to use this will defined in the `g:formatters` variable
"
function s:format_text(lines, type)
    let l:lines = a:lines

    "
    " Just return the lines if there is no formatters defined
    "
    if !exists("g:formatters['" . a:type . "']")
        if g:formatter_debug == 1
            echo '[formatter] No formatter defined for ' . a:type
        endif

        return split(lines, "\n")
    endif

    "
    " Run the formatter command and return the output of the text
    "
    let l:command = g:formatters[a:type]
    let l:formatted_lines = split(system(command, lines), "\n")

    "
    " Return original lines if formatter command returns an error
    "
    if v:shell_error > 1
        echoerr '[formatter] ' . join(formatted_lines, "\n")
        return split(lines, "\n")
    endif

    return formatted_lines
endfunction

"
" Defined the autocmd if the plugin is set to format on save
"
if g:formatter_on_save == 1
    autocmd BufWritePre * call s:format()
endif
