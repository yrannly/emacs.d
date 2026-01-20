;; -*- lexical-binding: t; -*-

;;; pack
(require 'use-package-ensure)
(setq use-package-always-defer t
      use-package-always-ensure t)

(use-package avy
  :bind
  (:map mode-specific-map
   ("a" . avy-goto-char)))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package consult
  :bind
  (("C-s" . consult-line)
   :map mode-specific-map
   ("cd" . consult-flymake)
   ("cf" . consult-fd)
   ("cg" . consult-ripgrep)))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

(use-package deadgrep
  :bind
  (:map mode-specific-map
   ("d" . deadgrep)))

(use-package embark
  :bind
  (:map mode-specific-map
   ("." . embark-act)))

(use-package eshell
  :init
  (require 'esh-mode)
  :bind
  (:map eshell-mode-map
        ("C-r" . consult-history)))

(use-package expand-region
  :bind
  (:map mode-specific-map
   ("e'" . er/mark-inside-quotes)
   ("ea'" . er/mark-outside-quotes)
   ("e]" . er/mark-inside-pairs)
   ("ea]" . er/mark-outside-pairs)))

(use-package magit)

(use-package marginalia
  :init
  (marginalia-mode))

(use-package markdown-ts-mode
  :mode
  ("\\.md\\'" . markdown-ts-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :init
  (require 'treesit-auto))

(use-package vertico
  :init
  (vertico-mode))

(use-package wgrep)

(use-package xclip
  :init
  (xclip-mode))

;;; site
(keymap-global-unset "C-@")
(keymap-global-unset "C-M-\\")
(keymap-global-unset "C-SPC")
(keymap-global-unset "C-x C-;")
(keymap-set mode-specific-map "f" 'indent-region)
(keymap-set mode-specific-map "r" 'recentf-open)
(keymap-set mode-specific-map "s" 'set-mark-command)

(when (string-match "WSL2" operating-system-release)
  (tressa/require 'wsl))

(with-eval-after-load 'gnus
  (setq
   gnus-always-read-dribble-file t
   gnus-home-directory "~/.cache/gnus"
   gnus-directory (expand-file-name "News" gnus-home-directory)
   gnus-select-method '(nnimap "qq"
                               (nnimap-address "imap.qq.com")
                               (nnimap-server-port 993)
                               (nnimap-stream ssl))))
