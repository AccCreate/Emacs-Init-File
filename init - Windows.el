;;; package --- Emacs Init File -*-
;;; Commentary: Dont bother.

"Melpa"
(require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)

"Toggle Full Screen on Startup"
(add-to-list 'default-frame-alist '(fullscreen . maximized))

"Remove menu/tool bar"
(tool-bar-mode -1)
(menu-bar-mode -1) 

"Evil Mode"
(require 'evil)
(evil-mode 1)

"Evil EasyMotion"
;;(evilem-default-keybindings "SPC")
;;
;; ace jump mode major function
;; 
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

"Line Numbers"
(require 'linum-relative)
;; (linum-relative-global-mode)
;; (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
(add-hook 'prog-mode-hook 'linum-relative-mode)
(add-hook 'org-mode-hook 'linum-relative-mode)
(setq linum-relative-current-symbol "")


"Evil Tab Mode"
(global-evil-tabs-mode t)

"NeoTree (NerdTree)"
(require 'neotree)
(setq neo-smart-open t)
(global-set-key [f8] 'neotree-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-rename-node)
(evil-define-key 'normal neotree-mode-map (kbd "D") 'neotree-delete-node)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-create-node)

"Unicode fonts"
(require 'unicode-fonts)
(unicode-fonts-setup)

"Change font size"
(set-frame-font "Courier New-14" nil t)

"Airline Theme Bottom Bar"
(require 'airline-themes)
(setq-default custom-safe-themes t)
(load-theme 'airline-powerlineish t)

"Vim CtrlP functionality"
(require 'ido)
'(ido-enable-flex-matching t)

"Darkorai Theme (Similar to Monokai Theme)"
(load-theme 'darkokai t)

"Evil Commentary mode"
(evil-commentary-mode)

"Read xkcd from Emacs"
"Control + Z quits Evil Mode"
(require 'xkcd)

"Org mode caputure quick notes note post it"
(define-key global-map (kbd "C-c c") 'org-add-note)

;; "Fly Check Syntax"
;; (require 'flycheck)
;; (global-flycheck-mode)
(global-flycheck-mode -1)

"Yasnippets (similar to UltiSnips)"
(require 'yasnippet)
(yas-global-mode 1)

"Helm emacs"
(require 'helm-config)
(require 'helm)
(set-face-attribute 'helm-selection nil :background "#ccff00")
(global-set-key (kbd "M-x") #'helm-M-x)

"Evil Org Mode"
(require 'org-evil)

"Org Mode"
(require 'org)
(define-key org-mode-map (kbd "<backtab>") nil)
(define-key org-mode-map (kbd "<S-iso-lefttab>") nil)

"Evil Mode custom Hotkeys"
(evil-ex-define-cmd "tabenew" 'elscreen-create)
(evil-ex-define-cmd "source" 'eval-buffer)
(evil-ex-define-cmd "load-file" 'load-file)
(define-key evil-normal-state-map (kbd "C-S-t") 'elscreen-create)

"Org Mode Babel for ipython"
(setq python-shell-interpreter "python")

"Python Elpy Development"
(elpy-enable)
;; (setq python-shell-completion-native-enable nil)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

"Ein mode"
(require 'ein)

"Org Mode command"
(evil-ex-define-cmd "org" 'org-mode)
(evil-ex-define-cmd "org-tree" 'org-show-subtree)
;; active Babel languages
;; #+BEGIN_SRC browser :out demo.png

"Case insensitive search"
(setq case-fold-search t)  
(setq completion-ignore-case  t)

"Vinegar like helm"
(define-key evil-normal-state-map "-" 'helm-find-files)
(define-key evil-normal-state-map "ESC" 'helm-keyboard-quit)
(define-key evil-normal-state-map (kbd "S-<up>") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "S-<down>") 'evil-scroll-down)

"Tramp edit remote files on Emacs under Windows"
(require 'tramp)
(set-default 'tramp-auto-save-directory "C:\\Users\\Paul Seo.PAUL\\AppData\\Local\\Temp")
(set-default 'tramp-default-method "plink")

"Github Magit"
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

"Control Key to change tabs"
;; (define-key evil-normal-state-map (kbd "C-`") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-normal-state-map (kbd "M-1") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-normal-state-map (kbd "C-1") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-normal-state-map (kbd "M-2") (lambda() (interactive) (elscreen-goto 1)))
(define-key evil-normal-state-map (kbd "C-2") (lambda() (interactive) (elscreen-goto 1)))
(define-key evil-normal-state-map (kbd "M-3") (lambda() (interactive) (elscreen-goto 2)))
(define-key evil-normal-state-map (kbd "C-3") (lambda() (interactive) (elscreen-goto 2)))
(define-key evil-normal-state-map (kbd "M-4") (lambda() (interactive) (elscreen-goto 3)))
(define-key evil-normal-state-map (kbd "C-4") (lambda() (interactive) (elscreen-goto 3)))
(define-key evil-normal-state-map (kbd "M-5") (lambda() (interactive) (elscreen-goto 4)))
(define-key evil-normal-state-map (kbd "C-5") (lambda() (interactive) (elscreen-goto 4)))
(define-key evil-normal-state-map (kbd "M-6") (lambda() (interactive) (elscreen-goto 5)))
(define-key evil-normal-state-map (kbd "C-6") (lambda() (interactive) (elscreen-goto 5)))
(define-key evil-normal-state-map (kbd "M-7") (lambda() (interactive) (elscreen-goto 6)))
(define-key evil-normal-state-map (kbd "C-7") (lambda() (interactive) (elscreen-goto 6)))
(define-key evil-normal-state-map (kbd "M-8") (lambda() (interactive) (elscreen-goto 7)))
(define-key evil-normal-state-map (kbd "C-8") (lambda() (interactive) (elscreen-goto 7)))
(define-key evil-normal-state-map (kbd "M-9") (lambda() (interactive) (elscreen-goto 8)))
(define-key evil-normal-state-map (kbd "C-9") (lambda() (interactive) (elscreen-goto 8)))
(define-key evil-normal-state-map (kbd "M-0") (lambda() (interactive) (elscreen-goto 9)))
(define-key evil-normal-state-map (kbd "C-0") (lambda() (interactive) (elscreen-goto 9)))
;; (define-key evil-insert-state-map (kbd "C-`") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-insert-state-map (kbd "C-1") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-insert-state-map (kbd "C-2") (lambda() (interactive) (elscreen-goto 1)))
(define-key evil-insert-state-map (kbd "C-3") (lambda() (interactive) (elscreen-goto 2)))
(define-key evil-insert-state-map (kbd "C-4") (lambda() (interactive) (elscreen-goto 3)))
(define-key evil-insert-state-map (kbd "C-5") (lambda() (interactive) (elscreen-goto 4)))
(define-key evil-insert-state-map (kbd "C-6") (lambda() (interactive) (elscreen-goto 5)))
(define-key evil-insert-state-map (kbd "C-7") (lambda() (interactive) (elscreen-goto 6)))
(define-key evil-insert-state-map (kbd "C-8") (lambda() (interactive) (elscreen-goto 7)))
(define-key evil-insert-state-map (kbd "C-9") (lambda() (interactive) (elscreen-goto 8)))
(define-key evil-insert-state-map (kbd "C-0") (lambda() (interactive) (elscreen-goto 9)))
(define-key evil-insert-state-map (kbd "M-1") (lambda() (interactive) (elscreen-goto 0)))
(define-key evil-insert-state-map (kbd "M-2") (lambda() (interactive) (elscreen-goto 1)))
(define-key evil-insert-state-map (kbd "M-3") (lambda() (interactive) (elscreen-goto 2)))
(define-key evil-insert-state-map (kbd "M-4") (lambda() (interactive) (elscreen-goto 3)))
(define-key evil-insert-state-map (kbd "M-5") (lambda() (interactive) (elscreen-goto 4)))
(define-key evil-insert-state-map (kbd "M-6") (lambda() (interactive) (elscreen-goto 5)))
(define-key evil-insert-state-map (kbd "M-7") (lambda() (interactive) (elscreen-goto 6)))
(define-key evil-insert-state-map (kbd "M-8") (lambda() (interactive) (elscreen-goto 7)))
(define-key evil-insert-state-map (kbd "M-9") (lambda() (interactive) (elscreen-goto 8)))
(define-key evil-insert-state-map (kbd "M-0") (lambda() (interactive) (elscreen-goto 9)))

"Elscreen next buffer screen list"
;; (define-key evil-normal-state-map (kbd "<tab>") 'ibuffer)
;; (define-key evil-normal-state-map (kbd "<backtab>") 'ibuffer)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key evil-normal-state-map (kbd "<tab>") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "K") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "J") 'evil-prev-buffer)

"NeoTree icon theme"
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


"C-i init file"
(defun find-init-file()
  "Get init file"
  (interactive)
  (find-file user-init-file))
(define-key evil-normal-state-map (kbd "C-i") 'find-init-file)

"File Name Copy"
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(define-key evil-normal-state-map (kbd "<f12>") 'my-put-file-name-on-clipboard)

"Google"
(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

"Evil Easy Navigation"
(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-w <right>") 'evil-window-right)
     (define-key evil-normal-state-map (kbd "C-w <left>") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-w <up>") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-w <down>") 'evil-window-down)
     ))
(define-key evil-normal-state-map (kbd "C-n") 'evil-window-new)

"Vim Style: Insert date in insert mode"
  (defun insert-standard-date ()
    "Inserts standard date time string." 
    (interactive)
    (insert (format-time-string "%c")))
(define-key evil-insert-state-map (kbd "<f3>") 'insert-standard-date)
(define-key evil-normal-state-map (kbd "<f3>") 'insert-standard-date)

"Multiple Cursors"
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(require 'company)


"Org mode enable wrap"
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)


"URL"
(defun url()
  (interactive)
  (browse-url)
  )


"Use ibuffer instead of buffer"
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq evil-emacs-state-modes (delq 'ibuffer-mode evil-emacs-state-modes))

"Buffer Listing command"
(evil-ex-define-cmd "bl" 'ibuffer)
(evil-ex-define-cmd "ls" 'ibuffer)
(evil-ex-define-cmd "bd" 'kill-buffer-and-window)

"Org Mode Artist"
(global-set-key (kbd "C-<f1>") (lambda()
			       (interactive)
			       (show-all)
			       (artist-mode)))

"Autocomplete list"
;; (ac-config-default)
"automcompletion org mode"
(load-file "~/.emacs.d/autocompletionOrg.el")
(add-hook 'some-mode-hook 'ac-ispell-ac-setup)
(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))
(setq company-dabbrev-downcase 0)
(setq company-idle-delay t)
(setq company-selection-wrap-around t)
(setq company-minimum-prefix-length 3)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)
(setq company-tooltip-limit 5)
(setq company-require-match 'never)
;; (setq company-auto-complete t)
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
  (let ((map company-active-map))
    (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key map (kbd "<backtab>") 'company-select-previous)
    (define-key map (kbd "RET") 'nil))

 (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))

  (global-set-key [tab] 'tab-indent-or-complete)
;; (defun tab-indent-or-complete ()
;;     (interactive)
;;     (if (minibufferp)
;;         (minibuffer-complete)
;;       (if (or (not yas/minor-mode)
;;               (null (do-yas-expand)))
;;           (if (check-expansion)
;;              (company-complete-common)
;; 	    (indent-for-tab-command))
;; 	)))
;; (global-set-key [tab] 'tab-indent-or-complete)

"Weather"
(require 'wttrin)
(setq wttrin-default-cities '("New York"))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))
(defun weather ()
  (interactive)
  (wttrin nil)
  )

"Todo Org Mode more options"
(setq org-todo-keywords
  '((sequence "TODO"
       "PROCESS"
      ;; "DONE"
      "DEFERRED"
      )))

  (setq org-todo-keyword-faces
    '(
      ("TODO" :background "red1" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
       ("PROCESS" :background "orange" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
      ("DEFERRED" :background "gold" :foreground "black" :weight bold :box (:line-width 2 :style released-button))
      ;; ("DONE" :background "forest green" :weight bold :box (:line-width 2 :style released-button))
      ))

"Tags for Org Mode"
(define-key evil-insert-state-map (kbd "M-q") 'org-set-tags-command)

"Dictionary"
(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c D") 'define-word)

"Tramp"
(setq tramp-default-method "plink")

"No messages buffer"
;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)
;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)
"No Splash Screen Start"
(setq inhibit-splash-screen t)
;;(switch-to-buffer "**")

;; Undo later
;;(delete-other-windows)


;; Makes *scratch* empty.
;; (setq initial-scratch-message "")

;;Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
 (if (get-buffer "*scratch*")
     (kill-buffer "*scratch*")))
(remove-scratch-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ace-jump-buffer python-mode jedi-direx elpy unicode-fonts all-the-icons-dired elscreen-buffer-group dired+ async-await ac-ispell ace-jump-mode jedi pdf-tools spaceline-all-the-icons all-the-icons yasnippet xkcd wttrin ssh org-evil org-babel-eval-in-repl neotree multiple-cursors magit linum-relative helm flycheck evil-tabs evil-easymotion evil-commentary elscreen-separate-buffer-list ein darkokai-theme company-anaconda babel airline-themes)))
 '(python-indent-offset 5)
 '(python-shell-interpreter "python"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
