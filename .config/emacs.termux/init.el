(setq user-init-file (or load-file-name
			 (buffer-file-name)))
(setq user-emacs-directory (file-name-directory
			    user-init-file))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;;
;; system --------
;;

;; will it put misc cache in .emacs.d?
;;  maybe good if sync?
;;  Puts stuff in home dir, plus .bak thrown around
(use-package no-littering)
  ;; :ensure t)

;; keep custom settings in temp file
;; (setq custom-file
;;   (if (boundp 'server-socket-dir)
;;       (expand-file-name "custom.el" server-socket-dir)
;;     (expand-file-name
;;      (format "emacs-custom-%s.el" (user-uid))
;;      temporary-file-directory)))
;; (setq custom-file
;;   (expand-file-name "custom.el"
;;     user-emacs-directory))
(setq custom-file (make-temp-file "custom.el"))
(load custom-file t)

(use-package keyfreq
  ;; :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun find-config ()
  "Edit init.el"
  (interactive)
  (find-file
   (expand-file-name "config.org" user-emacs-directory)))

(global-set-key (kbd "C-c q") 'find-config)

(global-set-key (kbd "C-c v") 'magit)

(use-package recentf)
  ;; :ensure t)

;; shorten y-n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; default <escape> command
(global-set-key (kbd "<escape>")
		'keyboard-escape-quit)

;; shows available prefix command options
(use-package which-key
  :init (which-key-mode)
  ;; :diminish which-key-mode ;; not installed?
  ;; :ensure t
  :config
  (setq which-key-idle-delay 0.3))

;; line break at space instead of "\"
(global-visual-line-mode 1)

;; Collection of Ridiculously Useful eXtensions
;;  (auto-aligns elisp!)
(use-package crux
  ;; :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)
	 ("M-o" . crux-other-window-or-switch-buffer)
	 ("C-k" . crux-smart-kill-line)
	 )
  :config
  ;; (global-set-key [remap kill-line]
  ;;		  'crux-smart-kill-line)
  (global-set-key (kbd "C-c c c")
		  'crux-cleanup-buffer-or-region))

;; god mode
(use-package god-mode
  ;; :ensure t
  :disabled
  :bind (("C-z" . god-local-mode)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window))
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (setq god-mode-enable-function-key-translation nil)
  ;;(global-set-key (kbd "C-z") #'god-local-mode)
  ;; isearch integ?
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (define-key god-local-mode-map (kbd "i")
    #'god-local-mode)
  (defun my-god-mode-update-mode-line ()
    (cond
     (god-local-mode
      (set-face-attribute 'mode-line nil
			  :foreground "#604000"
			  :background "#fff29a")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#3f3000"
			  :background "#fff3da"))
     (t
      (set-face-attribute 'mode-line nil
			  :foreground "#0a0a0a"
			  :background "#d7d7d7")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#404148"
			  :background "#efefef"))))

  (add-hook 'post-command-hook #'my-god-mode-update-mode-line)
  (god-mode))

;; meow
;;  On PC, meow-motion-overwrite-define-key set to make t move-down
(use-package meow
  ;; :ensure t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout
	  meow-cheatsheet-layout-dvorak)
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-till)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-save)
     '("X" . meow-sync-grab)
     '("y" . meow-yank)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (defun my-meow-mode-update-mode-line ()
    (cond
     (meow-normal-mode
      (set-face-attribute 'mode-line nil
			  :foreground "#604000"
			  :background "#fff29a")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#3f3000"
			  :background "#fff3da"))
     (t
      (set-face-attribute 'mode-line nil
			  :foreground "#0a0a0a"
			  :background "#d7d7d7")
      (set-face-attribute 'mode-line-inactive nil
			  :foreground "#404148"
			  :background "#efefef"))))

  (add-hook 'post-command-hook
	    #'my-meow-mode-update-mode-line)
  (meow-setup)
  (meow-global-mode 1))

;; jump to any char or line
(use-package avy)
  ;; :ensure t)

;; window navigation shortcuts
(use-package ace-window
  ;; :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; expands the region around the cursor semantically
(use-package expand-region
  ;; :ensure t
  :bind ("C-=" . er/expand-region))

;; completion framework that uses minibuffer
(use-package ivy
  ;; :ensure t
  :config
  ;; disable "^" in prompt
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode t))

;; ivy enhanced common emacs commands
(use-package counsel
  ;; :ensure t
  :bind (("M-x" . counsel-M-x)))

;; sorting and filtering library and ivy sort
(use-package prescient)
;;   :ensure t
(use-package ivy-prescient
  ;; :ensure t
  :config
  (ivy-prescient-mode t))

;; ivy enhanced isearch
(use-package swiper
  ;; :ensure t
  :bind (("M-s" . counsel-grep-or-swiper)))

;;
;; appearance --------
;;

;; what are the available terminal fonts?
;; (set-frame-font "Operator Mono 12" nil t)

(use-package solarized-theme
  ;; :ensure t
  :disabled
  :config
  (load-theme 'solarized-light t))

(load-theme 'modus-vivendi t)

(use-package feebleline
  ;; :ensure t
  :disabled
  :config
  (setq feebleline-msg-functions
	'((feebleline-line-number
	   :post "" :fmt "%5s")
	  (feebleline-column-number
	   :pre ":" :fmt "%-2s")
	  (feebleline-file-directory
	   :face feebleline-dir-face :post "")
	  (feebleline-file-or-buffer-name
	   :face font-lock-keyword-face
	   :post "")
	  (feebleline-file-modified-star
	   :face font-lock-warning-face
	   :post "")
	  (feebleline-git-branch
	   :face feebleline-git-face
	   :pre " : ")
	  (feebleline-project-name
	   :align right)))
  (feebleline-mode 1))

;; I'm activating mini-modeline after smart-mode-line
(use-package mini-modeline
  ;; :ensure t
  :disabled
  :config
  (mini-modeline-mode t))

;;
;; org-mode
;;

(defun mi/org-mode-setup ()
  )

(use-package org
  ;; :ensure t
  :defer t
  :hook (org-mode . mi/org-mode-setup)
  :config
  (setq org-directory
	(expand-file-name "org/" user-emacs-directory)))


;; (setq org-startup-indented 'f)
;; (setq org-directory "~/org")
;; (setq org-special-ctrl-a/e 't)
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; (define-key global-map "\C-cc" 'org-capture)
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;; (setq org-src-fontify-natively 't)
;; (setq org-src-tab-acts-natively t)
;; (setq org-src-window-setup 'current-window)

;; (let*
;;     ((base-font-color
;;       (face-foreground 'default nil 'default))
;;      (headline
;;       `(:foreground ,base-font-color)))
;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline))))
;;    `(org-level-7 ((t (,@headline))))
;;    `(org-level-6 ((t (,@headline))))
;;    `(org-level-5 ((t (,@headline))))
;;    `(org-level-4 ((t (,@headline))))
;;    `(org-level-3 ((t (,@headline :height 1.3))))
;;    `(org-level-2 ((t (,@headline :height 1.3))))
;;    `(org-level-1 ((t (,@headline :height 1.3 ))))
;;    `(org-document-title ((t (,@headline :height 1))))))

(use-package org-auto-tangle
  ;; :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

;;(use-package tree-sitter
;;  :ensure t)

;;
;; programs
;;

(use-package magit
  ;; :ensure t
  :commands magit)

(use-package vterm
  ;; :ensure t
  :commands vterm)

(use-package exwm
  ;; :ensure t
  :config
  (exwm-enable))
