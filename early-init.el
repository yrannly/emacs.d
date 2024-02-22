(defun tressa/require (file)
  (load (expand-file-name (symbol-name file) user-emacs-directory) t t))

(tressa/require 'builtin.el)
