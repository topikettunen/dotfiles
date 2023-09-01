global-set-key ")" self-insert-command
global-set-key "\^h" delete-backward-char
global-set-key "\^\." find-tag

set-fill-column 80

auto-execute *.c c-mode
auto-execute *.h c-mode

auto-execute .c auto-indent-mode
auto-execute .h auto-indent-mode

backup-to-home-directory
