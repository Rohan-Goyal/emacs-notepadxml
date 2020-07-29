(require 'company)

(defun tree-from-file(file-path)
  (save-excursion
    (let* ((buffer (find-file file-path))
           (tree (read buffer)))
      (kill-buffer buffer)
      tree)))

(defun radix-tree-keys(subtree prefix)
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


(defun company-custom-dictionary (command &optional arg &rest ignored)
  "Company mode backend for a custom dictionary stored as a radix tree."
  (case command
    ('init
     (unless (boundp 'company-custom-dictionary--words-tree)
         (setq company-custom-dictionary--words-tree (tree-from-file "radix.el"))))
    ('prefix
     (company-grab-word))
    ('candidates
     (radix-tree-keys company-custom-dictionary--words-tree arg))
    ('keep-prefix)))
(provide 'company-custom-dictionary)

;; Push the mode to the list of company backends
(push 'company-groovy company-backends)
