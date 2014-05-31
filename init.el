;;; init.el --- startup

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; DISABLE TOOL BAR, MENU BAR
(tool-bar-mode -1)
(menu-bar-mode -1)

;; MISC KEYBOARD RELATED THINGS
(normal-erase-is-backspace-mode 0)
(set-keyboard-coding-system nil)

;; DELETE KEY MAPS
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-line 0)) )
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)

;; MAP F1 TO MAN PAGE
(global-set-key  [(f1)]  (lambda () (interactive) (manual-entry (current-word))))

;; LINE NUMBERS AND COLUMN NUMBERS
(global-linum-mode 1)
;;(custom-set-variables '(linum-format (quote "%2d ")))
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))
(column-number-mode)


;; SET DEFAULT C INDENT STYLE
(setq-default c-default-style "linux")
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

;;; Packages

;; SEMANTIC COMPLETION
(add-hook 'after-init-hook 'global-company-mode)

;; FLYCHECK SETTINGS
(add-hook 'after-init-hook 'global-flycheck-mode)

;; MARKDOWN MODE
(autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; AUTOSAVE AND BACKUP SETTINGS
(add-to-list 'load-path "~/.emacs.d")

;;; Commentary: 

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil))

(make-directory "~/.emacs.d/autosaves/" t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
