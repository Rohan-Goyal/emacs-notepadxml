;;;  -*- lexical-binding: t -*-

;; See what I did with the gmatscript.el file?
;; It's basically an automated tool to create an elisp script file like that (syntaxt highlighting only for now, eventually auto-complete) from a notepad xml file
(require 'xml)
(require 'radix-tree)


;; We need to mark comments as such in the syntax table. Also string delimiters.
;; We also need to develop the lists of special words based on the keywords.
;; We also need to set the styles for delimiters
;; In the long run, autocomplete should use the words defined.
;; TODO: Comprehensive notepad delimiter spec
(defun write (obj file)
  (write-region (prin1-to-string obj) nil file))

(defun mode-data (head)
                                        ;Head is the root of the XML file.
                                        ; Returns list containing the metadata to determine modename and file extensions
  (setq children (xml-node-children head))
  (setq children-clean (car (cdr children)))
  (setq data (nth 1 children-clean))
  data
  )

(defun mode-name (data)
  (setq name (cdr (nth 0 data)))
  (setq name (concat (downcase name) "-mode"))
  name
  )

(defun file-extensions (data)
  (setq extension (split-string (cdr (nth 1 data))))
  (setq new ())
  (dolist (elem extension)
    (push (concat "\\." elem "\\'") new))
  new
  )

(defun find-valid-nodes (head)
  (setq valid-nodes ())
  (defun find-nodes (node)
    "Walk an xml tree as parsed, to find the node who's name matches Keywords or Words"
    (when (listp node)
      (cond
       (
        (string-match-p (regexp-opt '("Keywords" "Words")) (xml-get-attribute node 'name))
        (push node valid-nodes))
       )
      (mapc 'find-nodes (xml-node-children node))))
  (find-nodes head)
  valid-nodes
  )

(defun test-node (node)
  (cl-search "Keywords" (xml-get-attribute node 'name)))

(defun table-from-nodes (nodelist) ;In this case, we expect nodelist to be the valid-nodes we defined earlier
  "Returns a hashtable where the keys are \"Keyword1\",etc. and the vals are lists of words"
  (setq nodes (reverse nodelist)) ;
  (setq adict (make-hash-table :test 'equal))
  (setq nodelist-clean (seq-filter (lambda (y) (> (length y) 2)) nodes)) ;Remove the "empty" sets of words
  (dolist (kw nodelist-clean)
    (puthash  (cdr (car (nth 1 kw))) (split-string (nth 2 kw)) adict) ; Splits the long string into a list of words, and uses an unholy mix of CDRs and CARs to get the name of the requisite element
    )
  adict
  )

(defun font-defaults-from-hash (hash conversion-table)
  (setq defaults ())
  (maphash (lambda (k v)
             (push `(,(regexp-opt v 'words) . ,(cdr (assoc k conversion-table))) defaults)
             )
           hash
           )
  defaults ; List of pairs. Each pair is like (messy-regex. font-lock-whatever-face)
  )

(defun new-syntax-table (comment-start base) ;Base is the table from which to inherit. Comment start can be a char or string
  (if (stringp comment-start)
      (setq comment-char (string-to-char comment-start))
    (setq comment-char comment-start))

  (let ( (synTable (make-syntax-table base)))
    (modify-syntax-entry comment-char "<" synTable)
    (modify-syntax-entry ?\n ">" synTable) ; Modifies the comment syntax

    synTable
    )
  )




;; Just for reference. Use the notepad-conf.el file to configure these variables. These only provide defaults to avoid ugly errors
(defun user-config ()
  (setq xml-file "~/tmp/userDefinedLanguages-master/UDLs/Groovy_byGyrm.xml")
  (setq base-table 'python-mode-syntax-table)
  (setq comment-string "#") ; Eventually, get this from the XML, but it's nontrivial
  (setq base-mode 'python-mode)
  (setq conversion
                                        ; An assoc-list of the form (kw1 . font-lock-whatever-face)
        '(
          ("Keywords1" . font-lock-type-face)
          ("Keywords2" . font-lock-constant-face)
          ("Keywords3" . font-lock-builtin-face)
          ("Keywords4" . font-lock-keyword-face)
          ("Keywords5" . font-lock-reference-face)
          ("Keywords6" . font-lock-warning-face)
          ("Keywords7" . font-lock-type-face)
          ("Keywords8" . font-lock-constant-face)

          ("Words1" . font-lock-type-face)
          ("Words2" . font-lock-constant-face)
          ("Words3" . font-lock-builtin-face)
          ("Words4" . font-lock-keyword-face)
          ("Words5" . font-lock-reference-face)
          ("Words6" . font-lock-warning-face)
          ("Words7" . font-lock-type-face)
          ("Words8" . font-lock-constant-face)
                                        ; Notepad uses two different syntaxes. So this accounts for both.
          )))

(defun process (filename conversion-table)
  (setq xmltree (xml-parse-file filename))

  (setq name (mode-name (mode-data (car xmltree))))
  (setq extensions (file-extensions (mode-data (car xmltree))))

  (setq table (table-from-nodes (find-valid-nodes (car xmltree))))

  (setq font-defaults (font-defaults-from-hash table conversion))
                                        ; Generates and saves fontlock-defaults.

  (setq words (apply #'append (hash-table-values table)))
  (setq radix (seq-reduce (lambda (acc it) (radix-tree-insert acc it t)) words radix-tree-empty))
  (write radix (concat "~/.emacs.d/notepad/" name "-radix.el"))
                                        ; Generate a radix tree, so that the file ./company-custom.el can use it to produce a company-backend.

  ;;(setq autolists '(progn))
  (dolist (ext extensions)
    (setq autolists (append '(progn) `((add-to-list 'auto-mode-alist '(,ext . ,(make-symbol name)) t))))
    ) ;; Wrap the autolists in a progn, so the template can execute them all without issues
  )

(defun conf-interface ()
  (read-file-name "Select the config file from which to generate the language" "." "notepad-conf.el"))

(defun choose-outfile (default-name)
  (read-file-name "Where should the resulting mode definition go?" "~/.emacs.d/notepad" default-name nil default-name nil))


                                        ; TODO: Implement the autoloads
(defun notepadxml-convert ()
  (interactive)
  (user-config) ;Set defaults.
  (load-file (conf-interface))
                                        ; Load the key variables from the user configuration

  (process xml-file conversion) ;Write the radixes, set the tables, variables, etc.

                                        ;What follows is a template of sorts for the actual *-mode.el file.
  (setq template `(progn
                 ;;;###autoload
                    (define-derived-mode ,(make-symbol name) ,base-mode ,name
                      (set-syntax-table ;Directly inserting the table would be a disaster for size and readability
                       (let ( (synTable (make-syntax-table ,base-table)))
                         (modify-syntax-entry ,(string-to-char comment-string) "<" synTable)
                         (modify-syntax-entry ?\n ">" synTable) ; Modifies the comment syntax

                         synTable
                         ))
                      (setq font-lock-defaults '(,font-defaults)))
                 ;;;###autoload
                    ,autolists
                    (provide (quote ,(make-symbol name)))
                    )
        )
  (write template (choose-outfile (concat name ".el"))))



(provide 'notepadxml-convert)

" The idea is that the user chooses a file in their load-path to write the template to"

"
TODO:
- Read user configuration
- Manage the autoload thing. Maybe it can just ask to autoload instead of adding the comment somehow? So once the file is written to the load-path, simply have the user add (require 'whatever) to their .emacs
"

"
User-defined vars:
- Notepad file
- Base synTable
- Base mode
- Comment symbol
- Conversion table

Conversion factors:
A set can either be a keyword, a type, a builtin, a const, or a reference. Func/var names also exist if necessary.
So the table is user-defined: They set each keyword set to one of the above options.
So it becomes an assoc-list, between the string \"Keywords1\" and the variable 'font-lock-whatever-face.
"

"
How the user configures their thing:
Filename, basetable, basemode, comment-string can all be set in a simple ELISP assoc-list elsewhere. So we specify a template, write that to a file, and ask them for the conf location when the script is called. Based on the vals read in the conf, generate the output \"*-mode.el\" file

Conversion-table can also be defined in it's own file, based on the default. So write the default to a file, distribute that, and let them change it. When the program is run, it asks which file to read the conversiontable from.
"

"
Nums, operators, delimiters can be fairly standard (maybe just inherit from prog-mode). Ignore indentation since that's tricky

We let the user define a mode and syntable to inherit (default to one of the C-modes)

For auto-complete, we use https://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html
So once we have the hash, we basically iterate over it to get all the keywords and store them in a list. Then we use seq-reduce magic to pass them to a radixtree. We write this radixtree to a file.
"

"
Imagining the interface:
User calls this function. We ask them for a config file, which sets the conversion table and also all other factors as assoclists. They give us the file path. We read that file, evaluate all the setqs, and then run our /main/ function.
Then, they select the output dir, and we call the main function. This outputs the *-mode.el file to the requisite dir. So basically the interactivity we need is to let the users pick a file. Alternatively, we can just output it to somewhere in their load-path.
"
