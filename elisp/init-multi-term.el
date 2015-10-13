;;term

;;; Code:
(require 'multi-term)
(setq multi-term-program "/bin/bash")

(setq multi-term-program shell-file-name)
(add-hook 'term-mode-hook '(lambda ()
			     (define-key term-raw-map "\C-y" 'term-paste)
			     (define-key term-raw-map "\C-q" 'move-beginning-of-line)
			     (define-key term-raw-map "\C-f" 'forward-char)
			     (define-key term-raw-map "\C-b" 'backward-char)			     
			     (define-key term-raw-map "\C-t" 'set-mark-command)			     
			     (define-key term-raw-map (kbd "ESC") 'term-send-raw)
			     (define-key term-raw-map [delete] 'term-send-raw)
                             (define-key term-raw-map "\C-z"
                               (lookup-key (current-global-map) "\C-z"))))
(global-set-key (kbd "C-c n") 'multi-term-next)
(global-set-key (kbd "C-c p") 'multi-term-prev)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

(require 'ucs-normalize) 
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(setq system-uses-terminfo nil)
