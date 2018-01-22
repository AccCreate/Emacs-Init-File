;;; package --- Emacs Init File -*-
;;; Commentary: Dont bother.

"Melpa"
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                      ("marmalade" . "http://marmalade-repo.org/packages/")
                      ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

"Toggle Full Screen on Startup"
(add-to-list 'default-frame-alist '(fullscreen . maximized))

"Remove menu/tool bar"
(tool-bar-mode -1)
(menu-bar-mode -1) 

"Evil Mode"
(require 'evil)
(evil-mode 1)

"Caps and email update"
(shell-command "sh /home/paul/.emacs.d/capsEmacs.sh")

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
;;(global-relative-line-numbers-mode +1)
;; (linum-relative-global-mode)
;; (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
(add-hook 'prog-mode-hook 'linum-relative-mode)
(add-hook 'org-mode-hook 'linum-relative-mode)
;; (setq relative-line-numbers-motion-function 'forward-visible-line)
(setq linum-relative-current-symbol "")
(set-face-attribute 'linum nil :height 100)

"Fix Linum line number size to ratio"
(require 'linum)
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
		      (ceiling (* (if (boundp 'text-scale-mode-step)
				      (expt text-scale-mode-step
					    text-scale-mode-amount) 1)
				  (if (car (window-margins))
				      (car (window-margins)) 1)
				  ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)


"Move lines (Transpose) up and down"
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(provide 'move-text)


(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

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

"Airline Theme Bottom Bar"
(require 'airline-themes)
(setq-default custom-safe-themes t)
(load-theme 'airline-powerlineish t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-global-modes (quote (not eshell-mode comint-mode erc-mode rcirc-mode)))
 '(custom-safe-themes
   (quote
    ("003a9aa9e4acb50001a006cfde61a6c3012d373c4763b48ceb9d523ceba66829" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" default)))
 '(doc-view-continuous t)
 '(eclim-eclipse-dirs (quote ("/home/paul/eclipse/java-oxygen/eclipse")))
 '(eclim-executable "/home/paul/eclipse/java-oxygen/eclipse/eclim")
 '(eclimd-autostart-with-default-workspace t)
 '(eclimd-default-workspace "~/Documents/GitHub")
 '(js2-strict-missing-semi-warning nil)
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-header notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))
 '(notmuch-saved-searches
   (quote
    ((:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
     (:name "all mail" :query "*" :key
	    [ignore]
	    :sort-order newest-first))))
 '(notmuch-search-oldest-first nil)
 '(org-agenda-files (quote ("/home/paul/Documents/journal/journal.org")))
 '(package-selected-packages
   (quote
    (smooth-scroll bm eterm-256color hideshowvis origami company-tern xref-js2 xkcd wttrin use-package transpose-frame tabbar spaceline-all-the-icons smart-compile shell-toggle popwin pdf-tools org-journal org-evil org-capture-pop-frame org-ac ob-translate ob-ipython ob-browser neotree multi-term meghanada markdown-mode magit linum-relative js2-refactor jedi jabber ipython helm evil-tabs evil-easymotion evil-commentary elscreen-buffer-group ein dired-hacks-utils dired+ define-word darkokai-theme company-emacs-eclim airline-themes ace-jump-mode ac-ispell ac-emacs-eclim)))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#ffac4a" :weight semi-bold :height 0.7))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#63de5d" :weight semi-bold :height 0.7))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#06d8ff" :weight semi-bold :height 0.7))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#E6DB74" :weight semi-bold :height 0.7))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#53f2dc" :weight semi-bold :height 0.7)))))

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

"Fly Check Syntax"
(require 'flycheck)
(global-flycheck-mode)

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
(define-key org-mode-map (kbd "<S-tab>") nil)
(define-key org-mode-map (kbd "<backtab>") nil)
(define-key org-mode-map (kbd "<S-iso-lefttab>") nil)

"Org mode headlines same font"
(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 0.8)))

(add-hook 'org-mode-hook 'my/org-mode-hook)

"Evil Mode custom Hotkeys"
(evil-ex-define-cmd "tabenew" 'elscreen-create)
(evil-ex-define-cmd "source" 'eval-buffer)
(evil-ex-define-cmd "load-file" 'load-file)
(define-key evil-normal-state-map (kbd "C-S-t") 'elscreen-create)
(define-key evil-normal-state-map (kbd "M-t") 'elscreen-create)
(define-key evil-normal-state-map (kbd "M-w") 'elscreen-kill-screen-and-buffers)

"Org Mode Babel for ipython"
(setq python-shell-interpreter "python3")

"Increase font Decrease font of text"
(global-set-key (kbd "M-+") #'text-scale-increase)
(global-set-key (kbd "M--") #'text-scale-decrease)
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

"Ein mode"
(require 'ein)

"Org Mode command"
(evil-ex-define-cmd "org" 'org-mode)
(evil-ex-define-cmd "org-tree" 'org-show-subtree)
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (java . t)
   (C . t)
   (shell . t)
   (translate . t)
   (ipython . t)
   (browser . t)
   ))
;; #+BEGIN_SRC browser :out demo.png
(add-to-list 'exec-path "/opt/local/bin")
(setenv "PATH" (mapconcat 'identity exec-path ":"))

"Case insensitive search"
(setq case-fold-search t)  
(setq completion-ignore-case  t)

"Vinegar like helm"
(define-key evil-normal-state-map "-" 'helm-find-files)
;; (define-key evil-normal-state-map "<escape>" 'helm-keyboard-quit)
(define-key evil-normal-state-map (kbd "S-<up>") 'evil-scroll-up)
(define-key evil-normal-state-map (kbd "S-<down>") 'evil-scroll-down)

"Org Journal"
(setq org-agenda-files '(
			 "/home/paul/Documents/journal/journal.org"
			 "/home/paul/Documents/journal/diary.org"))
(setq org-agenda-custom-commands
      '(("a" "Simple agenda view"
         ((agenda "")
          ;; (alltodo "")
	  ))))
(evil-ex-define-cmd "agenda" 'org-agenda)
(define-key evil-normal-state-map (kbd "C-a") 'org-agenda)
(define-key evil-normal-state-map (kbd "C-A") 'org-agenda)

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
(defun my-next-buffer ()
  "next-buffer, only skip *Messages*"
  (interactive)
  (evil-next-buffer)
  (when (string= "*Messages*" (buffer-name))
    (kill-buffer "*Messages*")
    (evil-next-buffer))
  (when (string="*helm M-x*" (buffer-name))
    (evil-next-buffer))
  (when (string="*Ibuffer*" (buffer-name))
    (kill-buffer "*Ibuffer*")
    (evil-next-buffer))
  (when (string="*Help*" (buffer-name))
    (kill-buffer "*Help*")
    (evil-next-buffer))
  (when (string="*shell*" (buffer-name))
    (kill-buffer "*shell*")
    (evil-next-buffer))
  (when (string="*helm M-x*" (buffer-name))
    (evil-next-buffer))
  )
(defun my-prev-buffer ()
  "next-buffer, only skip *Messages*"
  (interactive)
  (evil-prev-buffer)
  ;; (while (or (string="*Messages*" (buffer-name)) (string="*Help*" (buffer-name)) (string="*helm M-x*" (buffer-name)) (string="*Help*" (buffer-name)))
  ;;   (if (string="*Messages*" (buffer-name))
  ;; 	(kill-buffer "*Messages*"))
  ;;   (if (string="*helm M-x*" (buffer-name))
  ;; 	(kill-buffer "*helm M-x*"))
  ;;   (if (string="*Help*" (buffer-name))
  ;; 	(kill-buffer "*Help*"))
  ;;   (evil-prev-buffer)
  ;;   )		   
  
  (when (string= "*Messages*" (buffer-name))
    (kill-buffer "*Messages*")
    (evil-prev-buffer))
  (when (string="*helm M-x*" (buffer-name))
    (evil-prev-buffer))
  (when (string="*Ibuffer*" (buffer-name))
    (kill-buffer "*Ibuffer*")
    (evil-prev-buffer))
  (when (string="*Help*" (buffer-name))
    (kill-buffer "*Help*")
    (evil-prev-buffer))
  (when (string="*shell*" (buffer-name))
    (kill-buffer "*shell*")
    (evil-prev-buffer))
  (when (string="*helm M-x*" (buffer-name))
   (evil-prev-buffer))

  )

(global-set-key [remap evil-next-buffer] 'my-next-buffer)
(global-set-key [remap evil-prev-buffer] 'my-prev-buffer)
(define-key evil-normal-state-map (kbd "K") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "J") 'evil-prev-buffer)


"NeoTree icon theme"
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

"zsh screen multiterm"
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(evil-ex-define-cmd "zsh" 'multi-term)
"Disable Yasnippet for zsh"
(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

"C-i init file"
(defun find-init-file()
  "Get init file"
  (interactive)
  (find-file user-init-file))
(define-key evil-normal-state-map (kbd "C-i") 'find-init-file)

"<f12> key finds org journal file"
(define-key evil-normal-state-map (kbd "<C-f12>")  (lambda() (interactive) (find-file "/home/paul/Documents/journal/journal.org")))
(global-set-key (kbd "<C-f12>") (lambda() (interactive) (find-file "/home/paul/Documents/journal/journal.org")))

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

"Zsh defined on Emacs"
(defun zsh()
  (interactive)
  (multi-term)
  )

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

"Eclim Java Eclipse"
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

"Notmuch"
(autoload 'notmuch "notmuch" "notmuch mail" t)

"Org mode enable wrap"
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

"Email hotkey"
(evil-ex-define-cmd "gmail" 'notmuch)
(evil-ex-define-cmd "email" 'notmuch)

(defun eUpdate()
      (interactive)
      (eshell)
      (insert "sh /home/paul/.emacs.d/emailEmacs.sh")
      (eshell-send-input)
      )
(evil-ex-define-cmd "eUpdate" 'eUpdate)

(load-file "~/.emacs.d/notmuch-setup.el")

"URL"
(defun url()
  (interactive)
  (browse-url)
  )

"smtpmail"
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "dhseo1011@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)
(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))


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

"Javascript js2"
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

"Javascript autocompletion TERN company"
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
                           
;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)


"Fold Collapse Function"
(add-hook 'prog-mode-hook #'hs-minor-mode)
(defun toggle-fold ()
  (interactive)
  (save-excursion)
  (end-of-line)
  (hs-toggle-hiding))
(define-key evil-normal-state-map (kbd "<C-tab>") 'toggle-fold)
(define-key evil-insert-state-map (kbd "<C-tab>") 'toggle-fold)
(add-hook 'prog-mode-hook #'hideshowvis-enable)

;; "Origami folding collapse function shrink fold"
;; (require 'origami)
;; (global-origami-mode 1)

"Scroll conservatively with line by line scrolling"

"Bookmark current line bm"
(require 'bm)
"F1 overrides the help function" 
(define-key evil-normal-state-map (kbd "<f1>") 'bm-toggle)
(define-key evil-insert-state-map (kbd "<f1>") 'bm-toggle)
(define-key evil-normal-state-map (kbd "<f2>") 'bm-previous)
(define-key evil-insert-state-map (kbd "<f2>") 'bm-previous)
(define-key evil-normal-state-map (kbd "<f3>") 'bm-next)
(define-key evil-insert-state-map (kbd "<f3>") 'bm-next)


"No messages buffer"
;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*Shell Command Output*")
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
(find-file "/home/paul/Documents/journal/journal.org") 
(delete-other-windows)
;; Makes *scratch* empty.
;; (setq initial-scratch-message "")

;;Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
 (if (get-buffer "*scratch*")
     (kill-buffer "*scratch*")))
(remove-scratch-buffer)
