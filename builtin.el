;; disable-modes
(line-number-mode -1)
(menu-bar-mode -1)

;; disable with test
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))

(defun tressa/var (&optional path)
  (let ((var (if (getenv "XDG_DATA_HOME")
                 (concat (getenv "XDG_DATA_HOME") "emacs")
               (concat user-emacs-directory "var"))))
    (if path
	(expand-file-name path var)
      var)))

;; core
(setq bidi-inhibit-bpa t
      create-lockfiles nil
      gc-cons-threshold (* 100 1024 1024)
      large-hscroll-threshold 1000
      locale-coding-system 'utf-8-unix
      long-line-threshold 1000
      read-process-output-max (* 1024 1024)
      ring-bell-function 'ignore
      use-short-answers t)
(setq-default bidi-display-reordering nil)
(set-default-coding-systems 'utf-8-unix)
(when (eq system-type 'windows-nt)
  (setq file-name-coding-system 'gbk))
(let ((dw 1920)
      (dh 1080)
      (fw 993)
      (fh 720))
  (setq default-frame-alist
        `((left . ,(- (/ dw 2) (/ fw 2)))
          (top . ,(- (/ dh 2) (/ fh 2)))
          (height . 40)
          (width . 120))))
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; no-litter
(setq
 auto-save-list-file-prefix (tressa/var "auto-save-list/saves-")
 custom-file (tressa/var "custom.el")
 desktop-path `(,(tressa/var))
 org-persist-directory (tressa/var "org-persist")
 package-user-dir (tressa/var "elpa")
 recentf-save-file (tressa/var "recentf")
 savehist-file (tressa/var "history")
 tramp-auto-save-directory (tressa/var "tramp/auto-save/")
 tramp-persistency-file-name (tressa/var "tramp/persistency.el")
 transient-history-file (tressa/var "transient/history.el")
 transient-levels-file (tressa/var "transient/levels.el")
 transient-values-file (tressa/var "transient/values.el")
 url-configuration-directory (tressa/var "url/"))

;; files
(setq auto-save-default nil
      backup-inhibited t
      make-backup-files nil)

;; package
(setq package-initialized nil
      package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; recentf
(recentf-mode)
(setq recentf-max-saved-items 100)

;; shell
(setq explicit-shell-file-name "/bin/bash")

;; simple
(set-default 'indent-tabs-mode nil)

;; startup
(setq inhibit-startup-screen t)

;; syntax
(setq syntax-wholeline-max 1000)

;; vc
(setq vc-follow-symlinks t)

;; start modes
(column-number-mode)
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(savehist-mode)

(create-fontset-from-fontset-spec "-*-Cascadia Mono-normal-r-*-*-14-*-*-*-c-*-fontset-custom")
(set-fontset-font "fontset-custom" 'han (font-spec :family "LXGW Neo Xihei") nil 'prepend)
(add-to-list 'default-frame-alist '(font . "fontset-custom"))
