# Emacs Notepad XML
## Features
- Syntax Highlighting
- Comment syntax
- Auto-completion (TODO)

## Usage
- Load the notepad-convert.el file
- Set up your config file as follows
```
 (setq xml-file <source>)
  (setq base-table <syntax-table>)
  (setq comment-string <comment delimiter>)
  (setq base-mode <mode>)
```

The base table is the syntax table for the most similar language which emacs supports. The table for a given language is usually named '<language>-mode-syntax-table

For instance if the XML file is for the Groovy language, which is similar to Java, then the file would look like:
```
(setq xml-file "~/files/groovy,xml")
(setq base-table 'java-mode-syntax-table)
(setq comment-string "//")
(setq base-mode 'java-mode)
```
(NOTE: The quotes before variable names are important)


- Call the function notepadxml-convert using M-x notepadxml-convert
- When prompted, enter the path of the config file you defined above
- When prompted, choose the path for the resulting mode definition file (make sure it's somewhere in your load-path)
- Note the filename, and `require` that in your .emacs file. For instance, with the above example you would add `(require 'groovy-mode)`

## Customizing Syntax Highlighting
In the user config file, you can define a variable called `conversion`. It takes the form of an associative list where the `car` is the name of the XML attribute containing those words, and the `cdr` is the font-lock face to be used. You may redefine them as you choose, based on the language.

## Autocomplete
Working on generating company backends, based on https://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html
