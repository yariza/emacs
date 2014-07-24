

;;; Init.el --- startup

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; disable toolbar, scrollbar, splash screen
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;; split across vertically by default
(setq split-height-threshold nil)
(setq split-width-threshold 80)

;; interactively do things
(require 'ido)
(ido-mode t)

;; ido-supercharged imenu
    (defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; c-mode line commenting
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
										comment-end   "")))

;; bind M-/ to comment region or line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
	(if (region-active-p)
		(setq beg (region-beginning) end (region-end))
	  (setq beg (line-beginning-position) end (line-end-position)))
	(comment-or-uncomment-region beg end)
	))
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)

;; pbcopy and pbcut
(defun pbcopy ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
  (setq deactivate-mark t)
)

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end))
)

(global-set-key (kbd "C-c x") 'pbcut)
(global-set-key (kbd "C-c c") 'pbcopy)

;; autopair
(require 'autopair)
(autopair-global-mode)

;; keyboard backspace
(normal-erase-is-backspace-mode 0)
(set-keyboard-coding-system nil)

;; C-w is yank region if region active, delete word if not
(defun yank-or-delete-word ()
  "Yanks region if region active, and deletes word if not."
  (interactive)
  (if (region-active-p)
	  (kill-region (region-beginning) (region-end))
	(backward-kill-word 1))
)
(global-set-key (kbd "C-w") 'yank-or-delete-word)

;; DELETE KEY MAPS
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-line 0)) )

;; diminish modelines
(require 'diminish)
(eval-after-load 'flycheck
  '(diminish 'flycheck-mode))
(eval-after-load 'company
  '(diminish 'company-mode))
(eval-after-load "autopair"
  '(diminish 'autopair-mode))
(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))
(eval-after-load 'undo-tree
  '(diminish 'undo-tree-mode))

;; company backends
(setq company-backends '(company-elisp 
                         company-gtags
                         company-dabbrev-code
                         company-keywords
                         company-files 
                         company-dabbrev))

;; LINE NUMBERS AND COLUMN NUMBERS
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))
(column-number-mode)

;; shell mode - <f1>
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(global-set-key [f1] 'shell)

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

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

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

(provide 'init)
;;; init.el ends here

