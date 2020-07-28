;;;  -*- lexical-binding: t -*-

;; See what I did with the gmatscript.el file?
;; It's basically an automated tool to create an elisp script file like that (syntaxt highlighting only for now, eventually auto-complete) from a notepad xml file
(require 'xml)


(defun write (obj file)
  (write-region (format "%S" obj) nil file))

(defvar file "/opt/GMAT/R2018a/extras/notepad++.xml")
(defvar xmltree (xml-parse-file file))
(write xmltree "./notepad-xml.el")


;; We need to mark comments as such in the syntax table. Also string delimiters.
;; We also need to develop the lists of special words based on the keywords.
;; We also need to set the styles for delimiters
;; In the long run, autocomplete should use the words defined.
;; TODO: Comprehensive notepad delimiter spec

(setq valid-nodes ())
(defun find-nodes (node)
  "Walk an xml tree as parsed, to find the node whose attribute returns true for the predicate test. Eg: Find a node such that it's attribute name contains the string 'Keywords1' would be (find-node tree 'name' (lambda (str) ))"
  ;; Tree-head refers to the root node of the xml rep.
  (when (listp node)
    (cond
     ((or ((cl-search "Keywords" (xml-get-attribute node 'name))) (cl-search "Words" (xml-get-attribute node 'name)))
      (push node valid-nodes))
     )
    (mapc 'find-nodes (xml-node-children node))))

(defun test-node (node)
  (cl-search "Keywords" (xml-get-attribute node 'name)))

(find-nodes (car xmltree))
(write-region (format "%S" valid-nodes) nil "./validnodes.el")


(defun table-from-nodes (_nodelist) ;In this case, we expect nodelist to be the valid-nodes we defined earlier
  (setq nodelist (reverse _nodelist))
  (setq adict (make-hash-table :test 'equal))
  (setq nodelist-clean (seq-filter (lambda (y) (> (length y) 2)) nodelist)) ;Remove the "empty" sets of words
  (dolist (kw nodelist-clean)
    (puthash  (cdr (car (nth 1 kw))) (split-string (nth 2 kw)) adict)
    (message "helloworld"))
  adict
  )

(defvar conversion-table) ;;Converts from name Keywords1 to objects, Keywords2 to functions, etc. Manually defined

(setq table (table-from-nodes valid-nodes))
(write table "./table.el")


"Plan: Find every node whose name attr contains \"Keyword\". Shove them all in a hashtable where the name (keywords1, etc.) is the key and the val is a list of words. After that, have a hashtable which maps it so that keywords1 is funcs, 2 is objs, etc. (or other way round).
DONE-ish


Nums, operators, delimiters can be fairly standard (maybe just inherit from prog-mode). Ignore indentation since that's tricky

We let the user define a mode and syntable to inherit (default to one of the C-modes)

For auto-complete, we use https://justinhj.github.io/2018/10/24/radix-trees-dash-and-company-mode.html
So once we have the hash, we basically iterate over it to get all the keywords and store them in a list. Then we use seq-reduce magic to pass them to a radixtree. We write this radixtree to a file.

Key problem is to generate a hash from xml. That needs solving.
"


"
Generate hashtable/assoc_list from XML:"
