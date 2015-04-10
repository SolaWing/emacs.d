(when *is-a-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq default-input-method "MacOSX")
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-v") 'whole-line-or-region-yank)
  (global-set-key (kbd "s-c") 'whole-line-or-region-copy-region-as-kill)
  (global-set-key (kbd "s-x") 'whole-line-or-region-kill-region)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-<backspace>") 'kill-whole-line)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-m") 'suspend-frame)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-C-f") 'toggle-frame-fullscreen)
  )


(provide 'init-osx-keys)
