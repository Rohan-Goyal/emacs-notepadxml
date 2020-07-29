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
  (write-region (format "%S" obj) nil file))

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


                                        ; User-defined variables
                                        ; TODO: Wrap these in their own funcs: One for user-config, one for executing, and one for writing the template. Then call them all successively
(setq file "/home/rohan/tmp/userDefinedLanguages-master/UDLs/Groovy_byGyrm.xml")
; ~/tmp/userDefinedLanguages-master/UDLs/Awk_byVitaliyDovgan.xml")
(setq base-table 'python-mode-syntax-table)
(setq comment-string "#") ; Eventually, get this from the XML, but it's nontrivial
(setq base-mode 'python-mode)

(setq conversion-table-default
                                        ; An assoc-list of the form (kw1 . font-lock-whatever-face)
                                        ; And so on
                                        ; How it is handled
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
       ))
"
How we're handling the conversion table:
We create a barebones, unintelligent default. It has keys corresponding to both Keywords and Words versions of the XML. So Keywords1: \"\" and Words1: \"\" map to the same thing\"
"


                                        ; Main process

(setq xmltree (xml-parse-file file))
;(write xmltree "./notepad-xml.el")

(setq name (mode-name (mode-data (car xmltree))))
(setq file-extensions (extensions (mode-data (car xmltree))))

(setq valid-nodes (find-valid-nodes (car xmltree)))
;(write valid-nodes "./validnodes.el")

(setq table (table-from-nodes valid-nodes))
;(write table "./table.el")

(setq font-defaults (font-defaults-from-hash table conversion-table-default))
;(write font-defaults "./fontlock.el") ; Generates and saves fontlock-defaults.

(setq words (apply #'append (hash-table-values table)))
(setq radix (seq-reduce (lambda (acc it) (radix-tree-insert acc it t)) words radix-tree-empty))
(write radix "./radix.el")
                                        ; Generate a radix tree, so that the file ./company-custom.el can use it to produce a company-backend.

(setq my-table (new-syntax-table comment-string (symbol-value base-table)))
;(write my-table "./syntax-table.el") ; Not strictly necessary


                                        ; What follows is a template of sorts for the actual *-mode.el file. Should be quasiquoted and written to afile.
; TODO: Implement the autoloads
(setq autolists '(progn))
(dolist (ext file-extensions)
  (setq autolists (append autolists `((add-to-list 'auto-mode-alist '(,ext . ,(make-symbol name)) t))))
  )
; Wrap the autolists in a progn, so the template can execute them all without issues

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

(write template (concat "./" name ".el"))

"
TODO:
- Read user configuration
- Manage the autoload thing. Maybe it can just ask to autoload instead of adding the comment somehow?
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

