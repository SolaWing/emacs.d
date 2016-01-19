;;; set val
(setq auto-save-file-name-transforms  (quote ((".*" "~/.emacs.d/autosavefile/" t))))
(setq desktop-save-mode nil)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(whitespace-cleanup-mode -1)

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
(defun yas/expand-or-complete ()
  "try expand yas at current point, or call yas-insert"
  (interactive)

  (if (use-region-p) (yas/visual-insert)
    (yas-insert))
  )

(defun yas/visual-insert ()
  "visual delete and call yas insert"
  (let ((yas-selected-text (and (use-region-p)
                                (delete-and-extract-region (region-beginning) (region-end)))))
    (when (evil-visual-state-p) (evil-insert 1))
    (yas-insert)))

(after-load "yasnippet"
    ;; yasnippet keymaps
    (define-key yas-minor-mode-map (kbd "M-'") 'yas/expand-or-complete)
  ;; yasnippet bugs, which cause yasnippet stop work. possible order is:
  ;; set previous-field of snippet, previous-snippet commit and turn to point.
  ;; snippet call yas--advance-end cause invalid marker error!
  (setq yas--snippet-revive nil)
  (advice-add 'yas--commit-snippet :before (lambda (snippet) "check dead previous field"
                                             (when (and (yas--snippet-previous-active-field snippet)
                                                        (consp (yas--field-end (yas--snippet-previous-active-field snippet))))
                                               (setf (yas--snippet-previous-active-field snippet) nil)
                                               ;; (message "clear dead field" args)
                                               )))
  )

(require-package 'ycmd)
(require-package 'company-ycmd)
(setq-default
 ycmd-server-command (list "python" (expand-file-name "~/.vim/bundle/YouCompleteMe/third_party/ycmd/ycmd/"))
 ycmd-global-config (expand-file-name "~/.vim/bundle/YouCompleteMe/.ycm_extra_conf.py")
 )
(defun ycmd/force-semantic-complete ()
  "Force semantic complete with company"
  (interactive)
  (let ((ycmd-force-semantic-completion 't))
    (company-cancel)
    (company-begin-backend 'company-ycmd nil)
    )
  )

(with-eval-after-load "company-ycmd"
  (define-key ycmd-mode-map (kbd "M-TAB") 'ycmd/force-semantic-complete)
  ;; extract candidates infos
  (defun company-ycmd--objc-param (prefix signature)
    (when (and prefix signature)
      (let ((match (s-match (format "%s\\(\\(?:\\^[^(]*\\)?([^)]*)\\|\\w+\\)" prefix) signature)))
        (cadr match)
        )))
  (defun company-ycmd--construct-candidate-objc (candidate)
    "function to construct completion objc string from a CANDIDATE."
    (company-ycmd--with-destructured-candidate candidate
      (let ((param (company-ycmd--objc-param insertion-text menu-text)))
        (propertize insertion-text
                    'return_type extra-menu-info
                    'kind kind
                    'doc detailed-info
                    'meta detailed-info
                    'param param))))

  (advice-add 'company-ycmd--get-construct-candidate-fn :before-until
              (lambda () "check if objc-mode first"
                (when (eq major-mode 'objc-mode)
                  #'company-ycmd--construct-candidate-objc)))

  ;; post completion, add param
  (defun company-template--add-objc-param (param)
    (let ((templ (if (string-match "([^)]+)" param)
                     (progn (let ((mb (+ 1 (match-beginning 0)))
                                  (me (- (match-end 0) 1))
                                  (ct 2)
                                  str-in-paren)
                              (setq str-in-paren (substring param mb me))
                              (setq str-in-paren (replace-regexp-in-string "[^[:blank:],][^,]*"
                                                                           (lambda (str)
                                                                             (prog1 (format "${%d:%s}" ct str)
                                                                               (setq ct (+ ct 1)))
                                                                             ) str-in-paren 't 't))
                              (concat "${1:" (substring param 0 mb) str-in-paren (substring param me) "}")
                              ))
                   (concat "${1:" param "}"))
                 ))
      ;; (message "templ %s " templ)
      (yas-expand-snippet templ)
      ))
  (defun company-ycmd--objc-post-completion (candidate)
    "Insert function arguments after completion for CANDIDATE."
    (--when-let (and (eq major-mode 'objc-mode)
                     company-ycmd-insert-arguments
                     (get-text-property 0 'param candidate))
      (company-template--add-objc-param it)
      't
      ))
  (advice-add 'company-ycmd--post-completion :before-until
              #'company-ycmd--objc-post-completion)
  )

(with-eval-after-load "company"
  (setq
   company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
   company-selection-wrap-around 't
   ))

;;; objc mode
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-hook 'objc-mode-hook 'myobjc/config)
(defun myobjc/config ()
  "custom config for objc-mode"
  (company-mode 1)
  (ycmd-mode 1)
  (c-set-style "linux")
  (setq c-basic-offset 4 tab-width 8)
  (setq company-backends '(company-ycmd))
  )

;;; php mode
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
