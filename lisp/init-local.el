;;; set val
(setq auto-save-file-name-transforms  (quote ((".*" "/Users/mac/.emacs.d/autosavefile/" t))))
(setq desktop-save-mode nil)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;;; scheme
(setq scheme-program-name "/Applications/MIT-Scheme.app/Contents/Resources/mit-scheme")
(add-hook (derived-mode-hook-name 'scheme-mode) 'sanityinc/lisp-setup)
;; (setenv "MITSCHEME_LIBRARY_PATH"
;;         "/Applications/MIT-Scheme.app/Contents/Resources")

;;; markdown
(setq markdown-command "markdown_py")
(add-to-list 'auto-mode-alist (cons "\\.mdown\\'" 'markdown-mode))

;;; json
(setq json-reformat:pretty-string? t)

;;; ace custom
(defun ace-select-jump-mode (&optional prefix)
  "if not activate mark, activate mark, then call `ace-jump-mode'"
  (interactive "p")
  (if (not mark-active) (push-mark-command nil))
  (ace-jump-mode prefix))
(global-set-key (kbd "C-:") 'ace-select-jump-mode)

;;; yasnippet
(setq yas-prompt-functions '(yas-ido-prompt))

;;; php mode
(defun php-mode-yas-hook ()
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (unless yas-global-mode (yas-minor-mode-on)))
(add-hook 'php-mode-hook 'php-mode-yas-hook)
;; custom package
(require-package 'evil)
(require-package 'evil-visualstar)
(require-package 'evil-surround)
(require-package 'evil-matchit)
(evil-mode 1)
(global-evil-surround-mode 1)
(global-evil-matchit-mode 1)
(global-evil-visualstar-mode 1)
(setq evil-default-state 'emacs)
(define-key evil-visual-state-map "s" 'evil-surround-region)

;;; global key bind
;; global bind move to hjkl
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-j") 'next-line)
(global-set-key (kbd "M-k") 'previous-line)
(global-set-key (kbd "M-l") 'forward-char)

;; bind origin M-hjkl to M-KL, M-j has bind to C-M-j
;;(global-set-key (kbd "M-K") 'kill-sentence)
;;(global-set-key (kbd "M-L") 'downcase-word)

(provide 'init-local)
