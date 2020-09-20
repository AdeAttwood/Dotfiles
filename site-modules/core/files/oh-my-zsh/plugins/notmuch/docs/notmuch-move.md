---
title:  NOTMUCH_OH_MY_ZSH

section: 1

header: notmuch-move

footer: Versoin 1.0.0

date: 2019-01-01

author:
    - Ade Attwood <code@adeattwood.co.uk>

lang: en-GB
---

# NAME

notmuch-move — Move a notmuch search into a new directory

# SYNOPSIS

notmuch-move \<dir\> \<search-term\>

# DESCRIPTION

Moves all of the emails found by a search term into a new directory

## Aguments

1, The path to move the emails to

.., The notmuch search term

## Options

-h, --help

:   Loads this man page

# EXAMPLES

notmuch-move ~/mail/local/INBOX.Trash from:junk@email.com and tag:to-delete

# SEE ALSO

**notmuch-search(1)**, **notmuch-search-terms(7)**
