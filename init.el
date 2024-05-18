;;; pack
(require 'use-package-ensure)
(setq use-package-always-defer t
      use-package-always-ensure t)

(use-package avy
  :bind
  (:map mode-specific-map
   ("a" . avy-goto-char)))

(use-package company
  :init
  (global-company-mode))

(use-package consult
  :bind
  (("C-s" . consult-line)
   :map mode-specific-map
   ("cd" . consult-flymake)
   ("cf" . consult-fd)))

(use-package deadgrep
  :bind
  (:map mode-specific-map
   ("d" . deadgrep)))

(use-package expand-region
  :bind
  (:map mode-specific-map
   ("e'" . er/mark-inside-quotes)
   ("ea'" . er/mark-outside-quotes)
   ("e]" . er/mark-inside-pairs)
   ("ea]" . er/mark-outside-pairs)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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
