2009-07-15, 2009-08-13

This set of files are for generating the ergoemacs keybindings.
(for explanation please see: http://xahlee.org/emacs/ergonomic_emacs_keybinding.html )

The files in this dir is used to generate the final output files:
ergonomic_keybinding_dvorak.el
ergonomic_keybinding_qwerty.el

The reason they are broken apart into several files is because many chunks of codes are shared. For example, comment header, version history, function definitions.

The purpose of each of the files in this dir should be obvious from their names or simple look at their content.

The only interesting one is: make_file.el
This is a elisp program that actually join all the other files and creates the final files.
Take a look there, it's fairly simple.

To create a ergoemacs binding for a new keyboard layout such as Colemak, just copy one of
kbd_qwerty to kbd_colemak, then modify the file.
To generate it, modify make_file.el to create a new section for colemak.

Xah
∑ http://xahlee.org/

☄