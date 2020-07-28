;;;  -*- lexical-binding: t -*-

;; See what I did with the gmatscript.el file?
;; It's basically an automated tool to create an elisp script file like that (syntaxt highlighting only for now, eventually auto-complete) from a notepad xml file
(require 'xml)


(defun write (obj file)
  (write-region (format "%S" obj) nil file))

(defvar file "./notepad++.xml")
(defvar xmltree (xml-parse-file file))
(write xmltree "./notepad-xml.el")


;; We need to mark comments as such in the syntax table. Also string delimiters.
;; We also need to develop the lists of special words based on the keywords.
;; We also need to set the styles for delimiters
;; In the long run, autocomplete should use the words defined.
;; TODO: Comprehensive notepad delimiter spec

(setq valid-nodes ())
(defun find-nodes (node)
  "Walk an xml tree as parsed, to find the node who's name matches Keywords or Words"
  (when (listp node)
    (cond
     (
      (or
       (cl-search "Keywords" (xml-get-attribute node 'name))
       (cl-search "Words" (xml-get-attribute node 'name)))
      (push node valid-nodes))
     )
    (mapc 'find-nodes (xml-node-children node))))

(defun test-node (node)
  (cl-search "Keywords" (xml-get-attribute node 'name)))

(find-nodes (car xmltree))
(write valid-nodes "./validnodes.el")


(defun table-from-nodes (_nodelist) ;In this case, we expect nodelist to be the valid-nodes we defined earlier
  "Returns a hashtable where the keys are \"Keyword1\",etc. and the vals are lists of words"
  (setq nodelist (reverse _nodelist)) ;
  (setq adict (make-hash-table :test 'equal))
  (setq nodelist-clean (seq-filter (lambda (y) (> (length y) 2)) nodelist)) ;Remove the "empty" sets of words
  (dolist (kw nodelist-clean)
    (puthash  (cdr (car (nth 1 kw))) (split-string (nth 2 kw)) adict)
    (message "helloworld"))
  adict
  )

(defvar conversion-table) ;;Converts from name Keywords1 to objects, Keywords2 to functions, etc. Manually defined, TODO

(setq table (table-from-nodes valid-nodes))
(write table "./table.el")


(defun regexes-from-hash (hash)
  (defvar regexes ())
  (maphash (lambda (k v)
             (push (regexp-opt v 'words) regexes)
             )
           table
           )
  regexes
  )
(setq regexes (regexes-from-hash table))
(setq keys (hash-table-keys table))

(write regexes "regex.el") ; Generates and saves regexes. Now, we use these regexes, the conversion table, and the keys list, to set the font-lock pairs


"
Nums, operators, delimiters can be fairly standard (maybe just inherit from prog-mode). Ignore indentation since that's tricky

We let the user define a mode and syntable to inherit (default to one of the C-modes)

For auto-complete, we use https://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html
So once we have the hash, we basically iterate over it to get all the keywords and store them in a list. Then we use seq-reduce magic to pass them to a radixtree. We write this radixtree to a file.
"




