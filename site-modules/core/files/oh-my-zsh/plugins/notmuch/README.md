# Notmuch oh my zsh plugin

| Command | Description                                               |
| ----    | ----                                                      |
| nm      | Alias for notmuch                                         |
| nmc     | Alias for notmuch count                                   |
| nmm     | Moves a notmuch search term into a specified folder       |
| nmnew   | Prints out all of the new mail useing the tag of "unread" |
| nms     | Alias for notmuch search                                  |
| nmt     | Alias for  notmuch tag                                    |

## Converting the docs

To convert the docs you need `pandoc` installed and out can use this command

~~~ zsh
find ./docs -name "*.md" -exec sh -c 'pandoc -s -t man "${0}" -o "./man/$(basename ${0%.md})"' {} \;
~~~

