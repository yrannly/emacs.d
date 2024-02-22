(keymap-global-unset "C-@")
(keymap-global-unset "C-M-\\")
(keymap-global-unset "C-SPC")
(keymap-global-unset "C-x C-;")
(keymap-set mode-specific-map "f" 'indent-region)
(keymap-set mode-specific-map "r" 'recentf-open)
(keymap-set mode-specific-map "s" 'set-mark-command)
(keymap-set mode-specific-map "w b" 'windmove-left)
(keymap-set mode-specific-map "w f" 'windmove-right)
(keymap-set mode-specific-map "w n" 'windmove-down)
(keymap-set mode-specific-map "w p" 'windmove-up)

(when (string-match "WSL2" operating-system-release)
  (setq interprogram-cut-function #'(lambda (text)
                                      (let* ((process-connection-type nil)
                                             (proc (start-process "xclip" nil
                                                                  "/mnt/c/Windows/System32/clip.exe")))
                                        (process-send-string proc text)
                                        (process-send-eof proc)))))

