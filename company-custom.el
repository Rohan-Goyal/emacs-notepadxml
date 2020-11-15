;;;  -*- lexical-binding: t -*-
(require 'company)
(require 'radix-tree)
(require 'f)

(defun tree-from-file (path)
  (car (read-from-string (f-read path))))

(defun print (expr)
  (message (format "%S" expr)))

(defun radix-tree-keys (subtree prefix)
  (let (keys '())
    (radix-tree-iter-mappings (radix-tree-subtree subtree prefix)
                              (lambda (key val)
                                (setq keys (cons (concat prefix key) keys)))
                              )
    keys))

(defun get-candidates (prefix)
  "Given a prefix return a list of matching words that begin with it"
  (when (> (length prefix) 2)
    (radix-tree-keys company-custom-dictionary--words-tree prefix)))

;; (defun company-cisco-ios (command &optional arg &rest ignored)
;;   "Company mode backend for a custom dictionary stored as a radix tree."
;;   (case command
;;     ('init
;;      (unless (boundp 'company-custom-dictionary--words-tree)
;;        (setq company-custom-dictionary--words-tree (tree-from-file "cisco-ios-mode-radix.el")))
;;      )
;;     ('prefix
;;      (company-grab-word))
;;     ('candidates
;;      (radix-tree-keys company-custom-dictionary--words-tree arg))
;;     ('keep-prefix)))

;; Push the mode to the list of company backends
                                        ;(unless (member 'company-cisco company-backends)
                                        ;(push 'company-cisco-ios company-backends)
                                        ;)

;; (defun generate-backend-deprecated (modename pathname)
;;   (setq name (concat "company-" modename))
;;   (setq name (make-symbol name))
;;   (setq dictname (make-symbol (concat modename "-dictionary")))
;;   (setq custom-backend
;;         `(defun ,name (command &optional arg &rest ignored)
;;            (case command
;;              ('init
;;               (unless (boundp ,dictname)
;;                 (setq  ,dictname (tree-from-file ,pathname))
;;                 )
;;               )
;;              ('prefix
;;               (company-grab-word)
;;               )
;;              ('candidates
;;               (radix-tree-keys ,dictname arg)
;;               )
;;              ('keep-prefix)))
;;         )
;;   (eval custom-backend) ;Now the actual function is defined
;;                                         ; So "name" itself is a symbol, which points to a symbol, which points to a function. So as far as I can tell, calling name resolves to the function name, which has a symbol of company-whatever and a value of the above function. Lisp is... interesting.

;;                                         ; Name is a symbol which points to the symbol company-whatever as its value. The symbol company-whatever points to the custom-backend-generated function as its function-value
;;                                         ; Dictname is a symbol which points to whatever-dictionary, which in turn is a symbol pointing to the radix-tree value.
;;   (unless (member name company-backends)
;;     (push name company-backends))
;;   )

(defun create-backend-func (modename pathname)
  (let ((name (make-symbol (concat "company-" modename)))
        (dictname (make-symbol (concat modename "-dictionary"))))
    `(defun ,name (command &optional arg &rest ignored)
       (case command
         ('init
          (unless (boundp ,dictname)
            (setq  ,dictname (tree-from-file ,pathname))
            )
          )
         ('prefix
          (company-grab-word)
          )
         ('candidates
          (radix-tree-keys ,dictname arg)
          )
         ('keep-prefix)))
    ))

(defun gen-backend (mode path)
  (let ((func (eval (create-backend-func mode path))))
    (unless (member func company-backends)
      (push func company-backends)
      (print func))
    ))



;(gen-backend "cisco-ios" "/home/rohan/experiments/emacs-notepadxml/cisco-ios-mode-radix.el")
                                        ;(company-cisco-ios)
(defun auto-gen-backend (lang)
 (gen-backend lang (concat "~/.emacs.d/notepad/" lang "-mode-radix.el" )))

                                        ;(company-cisco-ios)
"Function is inexpensive, as long as the radix is built. So what we can do is simply add a line in .emacs that hooks a given major mode to automatically generating the language for that major mode"

;(provide 'generate-backend)
(provide 'auto-gen-backend)
(provide 'gen-backend)
