(defun tressa/require (file)
  (load (expand-file-name (symbol-name file) user-emacs-directory) t t))

;; disable-modes
(line-number-mode -1)
(menu-bar-mode -1)
(show-paren-mode -1)

;; disable with test
(if (functionp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (functionp 'scroll-bar-mode)
    (scroll-bar-mode -1))

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
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
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
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; cus-deit.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq auth-sources `(,(expand-file-name ".authinfo" user-emacs-directory)))

;; files
(setq auto-save-default nil
      backup-inhibited t
      make-backup-files nil)

;; frame
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;; message
(setq message-directory "~/.cache/Mail")

;; org
(with-eval-after-load 'org
  (setq org-hide-emphasis-markers t
        org-pretty-entities t))

;; package
(setq package-initialized nil
      package-check-signature nil
      package-archives '(("gnu"    . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                         ("nongnu" . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
                         ("melpa"  . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")))

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

;; treesit
(setq c-ts-mode-indent-offset 4)

;; vc
(setq vc-follow-symlinks t)

;; windmove
(windmove-default-keybindings)

;; xref
(setq xref-search-program 'ripgrep)

;; start modes
(column-number-mode)
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(savehist-mode)

(when (functionp 'create-fontset-from-fontset-spec)
  (create-fontset-from-fontset-spec "-*-Cascadia Mono-normal-r-*-*-14-*-*-*-c-*-fontset-custom")
  (set-fontset-font "fontset-custom" 'han (font-spec :family "LXGW Neo Xihei") nil 'prepend)
  (add-to-list 'default-frame-alist '(font . "fontset-custom")))
