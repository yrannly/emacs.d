;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup wsl-path nil
  "Proper handling of windows filenames."
  :prefix "wsl-path-"
  :group 'files)


(defvar wsl-path-prefix "/mnt/"
  "Prefix for the WSL mount points.")

(defun resolve-file-ignoring-case (file dir)
  "Find a entry in DIR whose name matches FILE to within case."
  (cl-find (downcase file) (directory-files dir)
           :key 'downcase
           :test 'string-equal))

(defun resolve-path-ignoring-case (path)
  "Attempt to find a file ignoring case.  PATH must be an simple
absolute path like one returned from SUBSTITUTE-IN-FILE-NAME.
The file must exist for this to be meaningful, otherwise it will
simply return whatever you input."
  (if (file-exists-p path)
      path
    (let* ((filename "/"))
      (cl-loop for file in (split-string path "/" t)
               for guess = (concat filename file)
               do
               (if (file-exists-p guess)
                   (setf filename (concat guess "/"))
                 (let ((real-file (resolve-file-ignoring-case file filename)))
                   (if real-file
                       (setf filename (concat filename real-file "/"))
                     ;; If all else fails, leave it unchanged
                     (setf filename (concat filename file "/")))))
               finally (return
                        ;; Remove trailing slash if not on input
                        (if (equal ?/ (aref path (- (length path) 1)))
                            filename
                          (substring filename 0 (- (length filename) 1))))))))

(defun wsl-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defconst wsl-path-style1-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)\\\\")
(defconst wsl-path-style2-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)/")

;; We cannot assume that NAME matched wsl-path-style1-regexp nor
;; wsl-path-style2-regexp because this function could be called with
;; either argument to `expand-file-name', but only one argument to
;; `expand-file-name' may have matched a regexp.  For example,
;; `(expand-file-name ".." "c:/")' will trigger `(wsl-path-convert-file-name
;; "..")' and `(wsl-path-convert-file-name "c:/")' to be called.
(defun wsl-path-convert-file-name (name)
  "Convert file NAME to WSL style.
`x:/' to `/mnt/x/'."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation
                          'substitute-in-file-name)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation 'substitute-in-file-name))
    (cond ((string-match wsl-path-style1-regexp name)
           (let ((filename
                  (replace-match (concat wsl-path-prefix
                                         (downcase (substring (match-string 2 name) 0 1)))
                                 t nil name 2)))
             (while (string-match "\\\\" filename)
               (setq filename
                     (replace-match "/" t nil filename)))
             (resolve-path-ignoring-case
              (substitute-in-file-name filename))))
          ((string-match wsl-path-style2-regexp name)
           (resolve-path-ignoring-case
            (substitute-in-file-name
             (replace-match (concat wsl-path-prefix
                                    (downcase (substring (match-string 2 name) 0 1)))
                            t nil name 2))))

          (t name))))

;; (string-match wsl-path-style2-regexp "/sd/c:/xpd/file.txt")
;; (wsl-path-convert-file-name "sd/c:/xpd/file.txt")
;; (wsl-path-convert-file-name "~/path/c:/sds/")
;; (wsl-path-convert-file-name "/c:/sds/")
;; (wsl-path-convert-file-name "/sd/c:\\sds\\")

(defun wsl-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on WSL NAME with ARGS.
Map Windows sytle name to the WSL-style \"/[A-Za-z]/\" and call
OPERATION with the mapped filename\(s). NAME must have the format looks like
\"^/[A-Za-z]:/\" or \"^[A-Za-z]:\\\"  here. Note that at least the first
element of ARGS could be a filename too \(then it must have the same syntax
like NAME!) which must be converted \(e.g. `expand-file-name' can be called
with two filenames)."
  (wsl-path-run-real-handler
   operation
   (cons (wsl-path-convert-file-name name)
		 (if (stringp (car args))
			 (cons (wsl-path-convert-file-name (car args))
				   (cdr args))
		   args))))

(add-to-list 'file-name-handler-alist
             (cons wsl-path-style1-regexp
                   'wsl-path-map-drive-hook-function))
(add-to-list 'file-name-handler-alist
             (cons wsl-path-style2-regexp
                   'wsl-path-map-drive-hook-function))

;; browse
(setq browse-url-chrome-program "/mnt/d/apps/firefox/firefox.exe"
      browse-url-browser-function 'browse-url-chrome)
;; clipboard
(setq interprogram-cut-function #'(lambda (text)
                                    (let* ((process-connection-type nil)
                                           (proc (start-process "xclip" nil
                                                                "/mnt/d/apps/win32yank.exe" "-i")))
                                      (process-send-string proc text)
                                      (process-send-eof proc)))
      interprogram-paste-function #'(lambda nil (shell-command-to-string "/mnt/d/apps/win32yank.exe -o --lf")))
