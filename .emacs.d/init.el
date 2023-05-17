;; The default is 800 kilobytes (measured in bytes)
(setq gc-cons-threshold (* 50 1000 1000))

(defun ri/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ri/display-startup-time)

(setq native-comp-async-report-warnings-errors 'silent)

;; Package sources ----
(require 'package) ; package management functions (autoloaded?)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize) ; initialize package system and prep to be used

;; if package-archive-contents is empty (fresh install), ----
;;   run package-refresh-contents.
(unless package-archive-contents
  (package-refresh-contents))

;; non-Linux setup use-package ----
;; if use-package isn't installed or new update, then package-install it
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; setup use-package ----
(require 'use-package)
(setq use-package-always-ensure t) ;; no need to add :ensure t on every package that needs it
                                        ;(setq use-package-always-defer t) ;; explicitly state which to ensure, might break, save first
(setq use-package-verbose t)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; UNNECESSARY CHANGE, CHANGE BACK!
(setq user-emacs-directory ;; should be directory of init.el or Emacs.org
      (file-name-directory (or load-file-name (buffer-file-name))))
(setq user-init-file ;; init.el in user-emacs-directory
      (concat user-emacs-directory "init.el"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

;; no-littering
(use-package no-littering)

;; keep autosaves in emacs dir
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backup/" user-emacs-directory))))

(setq ri/exwm-enabled (and (eq window-system 'x)
                           (seq-contains command-line-args "--start-exwm")))

(when ri/exwm-enabled
  (require 'ri-desktop))

(setq ri/is-guix-system (and (eq system-type 'gnu/linux)
                             (require 'f)
                             (string-equal (f-read "/etc/issue")
                                           "\nThis is the GNU system.  Welcome.\n")))

(cond ((eq ri/is-guix-system t)
       (load-file (expand-file-name "desktop.el" user-emacs-directory))
       (start-process-shell-command "setxkbmap dv" nil "setxkbmap -layout 'us' -variant 'dvorak' -option ctrl:nocaps")))

;; disable startup screen
(setq inhibit-startup-message nil)

;; disable ui
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode 1)     ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(menu-bar-mode -1)   ; disable menu bar

;; disable bell
(setq ring-bell-function 'ignore) ; TURN OFF ONCE AND FOR ALL?
;; (setq ring-bell-function 'silent) ; TURN OFF ONCE AND FOR ALL?

;; enable mode line flash bell
;; (use-package mode-line-bell
;; :if (ring-bell-function 'ignore)
;; :config
;; (mode-line-bell-mode))

;; add line numbers
(global-display-line-numbers-mode t)
(column-number-mode) ; (columns on modeline)

;; line number mode exceptions
(dolist (mode '(org-mode-hook
                dired-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                image-minor-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; default font (modeline, minibuffer, default for applications, etc)
(set-face-attribute 'default nil :font "Fira Code" :height 110)
;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 115)

;; fixed pitch font (code blocks, property, startup, etc (can add more))
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)

;; variable pitch font (toc links, regular text in org, etc...)
;; how about Iosveka instead?
;; (bullets are configured in org-fonts)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120 :weight 'regular)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; (change to nil) make things like yes or no prompts dialogue boxes

;; maximize windows by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set frame transparency
(defun ri/enable-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90))))

(defun ri/disable-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100))))

;; shorten y-n prompt
(defalias 'yes-or-no-p 'y-or-n-p)

;; all-the-icons
;; note: on a new machine, must run M-x all-the-icons-install-fonts
;; create a script to automatatically detect whether this has been run?
(use-package all-the-icons)

;; nerd-fonts (used by doom-modeline by default)
;; note: on a new machine, must run M-x nerd-icons-install-fonts
(use-package nerd-icons)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil) ; hide pane to see what packages were updated
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "15:00"))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; ESC to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "<escape>") #'god-mode-all)
;; (global-set-key (kbd "<escape>") #'god-local-mode)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
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
   '("<escape>" . ignore)
   '("t" . previous-line)
   '("p" . "H-t"))
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
   '(":" . meow-goto-line) ;; moved from "Q"
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
   '("F" . meow-search) ;; moved from "s"
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   ;; H Directional key moved to the bottom
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   ;; '("m" . meow-mark-word) ;; swap with w, next-word
   ;; '("M" . meow-mark-symbol) ;; swap with w, next-symbol
   '("m" . meow-next-word) ;; moved from "w", mark-word
   '("M" . meow-next-symbol) ;; moved from "W", mark-symbol
   ;; N Directional key moved to the bottom
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . kill-this-buffer)
   ;; '("Q" . meow-goto-line) ;; move to " : "
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;; '("s" . meow-search) ;; move to F, replace with directional keys
   ;; S Directional key moved to the bottom
   ;; T Directional key moved to the bottom
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   ;; '("w" . meow-next-word) ;; swap with m, mark-word/symbol
   ;; '("W" . meow-next-symbol)
   '("w" . meow-mark-word) ;; moved from "m", mark-word
   '("W" . meow-mark-symbol) ;; moved from "M", mark-symbol
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . View-scroll-half-page-forward) ;; new keys
   '("?" . View-scroll-half-page-backward) ;; new keys
   '("<escape>" .  keyboard-escape-quit)

   ;; Directional keys:

   ;; is this swap in h and p really better?

   '("h" . meow-left)
   '("H" . meow-left-expand)
   ;; '("h" . meow-prev)
   ;; '("H" . meow-prev-expand)

   '("t" . meow-prev)
   '("T" . meow-prev-expand)
   ;; '("t" . meow-left)
   ;; '("T" . meow-left-expand)

   '("n" . meow-next)
   '("N" . meow-next-expand)

   '("s" . meow-right) ;; Directional, s is ->
   '("S" . meow-right-expand)
   ))

(use-package meow
  :config
  (meow-setup)
  ;; free-keys is very useful!
  (define-key meow-insert-state-keymap (kbd "C-g")  #'meow-insert-exit)
  ;; start up applications in insert mode
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))

  (meow-global-mode 1))

(use-package god-mode
  :disabled
  :commands god-mode
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (setq god-mode-enable-function-key-translation nil)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-below)
  (global-set-key (kbd "C-x C-3") #'split-window-right)
  (global-set-key (kbd "C-x C-0") #'delete-window)
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
  ;; (add-to-list 'god-exempt-major-modes 'dired-mode)
  (god-mode))

;; evil-mode exclude
(defun ri/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; evil-mode
(use-package evil
  :disabled
  :commands evil-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (add-hook 'evil-mode-hook 'ri/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join) ; wowie

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; -- haven't set up visual line mode yet
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;;hook
;; have these programs be in emacs-mode (C-z)
;;(evil-mode-hook . mi/evil-hook)

;; evil collections
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; To change to undo-tree, update evil-undo-system above.
;; undo-tree for evil-undo
;; (use-package undo-tree
;;   :after evil
;;   :init
;;   (global-undo-tree-mode 1)
;;   :config
;;   (setq undo-tree-history-directory-alist
;;    '(("." . (concat user-emacs-directory "var/undo-tree-his/")))))

;; Whenever you C-/ it does the default undo. Not undo-fu-undo. So, maybe
;;  bind C-/ to undo-fu-undo without overwriting? but maybe it's ok...

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :config
  (undo-fu-session-global-mode t))

;; general.el
(use-package general
  :config
  (general-create-definer ri/leader-keys
    :prefix "C-c"))

(ri/leader-keys
  "t" '(:ignore t :which-key "toggles"))

(ri/leader-keys
  "s" '(:ignore t :which-key "special"))

(ri/leader-keys
  "q"  '(:ignore t :which-key "quit/session")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

(ri/leader-keys
  "-l" 'ri/run-librewolf
  "-p" 'ri/run-keepassxc
  "-d" 'ri/run-discord)

;; hydra (fast, transient keybinds)
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("f" nil "finished" :exit t))

(ri/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; maybe replace the first bind with replace kill line with crux?
(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line)
  :config
  ;; (global-set-key [remap kill-line] 'crux-smart-kill-line)
  (ri/leader-keys
    "mc" 'crux-cleanup-buffer-or-region))

(use-package expand-region
  :commands expand-region)

(use-package avy
  :bind ("C-:" . 'avy-goto-char)
  :commands avy)

(use-package free-keys
  :commands free-keys)

(ri/leader-keys
  "sF" 'free-keys)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-M-g 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-M-g 1 M-v"))

(use-package ace-window
  :config
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?g ?c ?r))
  (defvar aw-dispatch-alist
    '((?d aw-delete-window "Delete Window")
      (?1 delete-other-windows "Delete Other Windows")
      (?s aw-split-window-horz "Split Horz Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?, aw-split-window-fair "Split Fair Window")
      (?o aw-flip-window "Other Window")
      (?w aw-swap-window "Swap Windows")
      (?m aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?b aw-switch-buffer-in-window "Select Buffer")
      (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")
  (global-set-key (kbd "M-o") 'ace-window))

;; replace evil-direction w/ package
(ri/leader-keys
  "w"  '(:ignore t :which-key "window")
  "wv" '(split-window-right :which-key "v-split")
  "ws" '(split-window-below :which-key "h-split")
  "wd" '(delete-window :which-key "close window")
  "wc" '(delete-window :which-key "close window")
  "ww" '(evil-window-next :which-key "next-window")
  "wW" '(evil-window-prev :which-key "prev-window")
  "wh" '(evil-window-left :which-key "window-left")
  "wj" '(evil-window-down :which-key "window-down")
  "wk" '(evil-window-up :which-key "window-up")
  "wl" '(evil-window-right :which-key "window-right")
  "wH" '(evil-window-move-far-left :which-key "move left")
  "wJ" '(evil-window-move-very-bottom :which-key "move down")
  "wK" '(evil-window-move-very-top :which-key "move up")
  "wL" '(evil-window-move-far-right :which-key "move right")
  "wa" '(hydra-window-adjust/body :which-key "window-ratio-adjust")
  "wi" '(:ignore t :which-key "minibuffer")
  "wie" 'minibuffer-keyboard-quit
  "wio" 'switch-to-minibuffer)

(defhydra hydra-window-adjust (:timeout 5)
  "adjust window split ratio"
  ("h" shrink-window-horizontally "left")
  ("j" enlarge-window "down")
  ("k" shrink-window "up")
  ("l" enlarge-window-horizontally "right")
  ("c" balance-windows "balance")
  ("<enter>" nil "finished" :exit t)
  ("f" nil "finished" :exit t))

(ri/leader-keys
  "k" 'kill-this-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" 'meow-last-buffer
  "bb" 'counsel-switch-buffer
  "br" 'read-only-mode)

;; ivy
(use-package ivy
  :diminish ; hide ivy minor-mode on modeline
  :bind (("C-s" . swiper) ;; fuzzy search tool
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (message "Ivy got loaded!")
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; counsel (enhanced standard emacs commands)
(use-package counsel
  :bind (;; ("M-x" . counsel-M-x)
         ;; ("C-x b" . counsel-ibuffer)
         ;; ("C-x C-f" . counsel-find-file)
         ("C-M-j" . 'counsel-switch-buffer)
         ("s-c" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (message "Counsel loaded!")
  (counsel-mode 1))

;; adds ivy completion regex and order commands by last used
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; doom-themes
;; recommended: henna, palenight, snazzy
(use-package doom-themes
  :bind (("C-h T" . ri/load-theme-and-font-setup))
  :init
  ;; (load-theme 'modus-vivendi t)
  (load-theme 'ef-dark t)
  ;; (load-theme 'ef-duo-dark t)
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-laserwave t)
  ;; (load-theme 'doom-snazzy t)
  )

(use-package ef-themes)

(use-package catppuccin-theme)

(defun ri/load-theme-and-font-setup ()
  (interactive)
  (counsel-load-theme)
  (ri/org-font-setup))

(ri/leader-keys
  "st" '(ri/load-theme-and-font-setup :which-key "choose theme"))

;; doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-dracula-brighter-modeline nil)
  ;; (doom-modeline-height 45) ; 40?
  (doom-modeline-height 20)
  (doom-modeline-hud nil))

(use-package mini-modeline
  :disabled
  ;; :custom
  ;; (mini-modeline-r-format)
  ;; :config
  ;; (mini-modeline-mode t)
  )

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package modus-themes
;;   :custom
;;   ;(modus-themes-mode-line '(borderless))
;;   :config
;;   (load-theme 'modus-vivendi t))

;; which-key (lists keybinds)
;; (add links above source blocks later)
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; helpful (improves help menu)
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind ;; change the function of the command
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  ("C-h M" . which-key-show-major-mode)
  ("C-h H" . helpful-at-point))

(ri/leader-keys
  "sh" 'helpful-at-point
  "sv" 'describe-variable
  "sf" 'describe-function
  "sk" 'describe-key
  "sM" 'which-key-show-major-mode
  "sm" 'describe-mode
  "sR" 'info-display-manual
  "sP" 'describe-package)

(defun ri/org-font-setup ()
  (interactive)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    ;; font for bullets
    (set-face-attribute (car face) nil :font "Fira Code" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; org
(defun ri/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . ri/org-mode-setup)
  :custom
  (org-directory "~/org")
  (org-ellipsis " ▼ ")
  (org-hide-emphasis-markers t) ; hide formatting chars (* / ~ = etc)
  (doom-modeline-enable-word-count t)
  :config
  ;; show message when loading (not necessary?)
  (message "Org Mode loaded!")

  ;; --- org-agenda ------
  (setq org-deadline-warning-days 14
        org-agenda-start-with-log-mode t ; enable log-mode by def
        org-log-done 'time
        org-log-into-drawer t) ; ?

  ;; --- agenda files ------
  (setq org-agenda-files
        '("~/org/agenda/agenda.org"
          "~/org/agenda/work.org"))
  ;; "~/org/agenda/habits.org"))

  ;; --- todo keywords ------
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; --- org-refile ------
  ;; (add target locations for org-refile)
  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("work.org" :maxlevel . 1)))
  ;; save org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; --- org-habit ------
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; --- commonly known tasks to appear when counsel-org-tag ------
  ;; org-set-tags-command ?
  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; --- Custom Agenda Views! ------
  ;; (easier with org-ql)
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  ;; --- Org Capture Templates! ------
  ;; (basically quickly add new entries mindlessly)
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/org/agenda/agenda.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/org/agenda/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)

          ("jm" "Meeting" entry
           (file+olp+datetree "~/org/agenda/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/org/agenda/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/org/agenda/metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  ;; --- set up org-fonts ------
  (ri/org-font-setup))

;; misc
(ri/leader-keys
  "o"  '(:ignore t :which-key "org")
  "ox" '(eval-last-sexp :which-key "eval-last-sexp")
  "oX" '(eval-region :which-key "eval-region"))

;; keybinds! -----

;; mostly just an example
(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

;; org-agenda leader keybinds (create a separate section?
(ri/leader-keys
  "oa"  '(:ignore t :which-key "org-agenda")
  "oaa" '(org-agenda :whihc-key "agenda-commands")
  "oas" '(org-agenda-list :which-key "agenda-schedule")
  "oat" '(org-todo-list :which-key "todo-list")
  "oac" '(org-capture :which-key "org-capture")
  "oar" '(org-refile :which-key "org-refile")) ; put refile in org-mode-map?

(ri/leader-keys
  "md"  '(:ignore t :which-key "date/schedule")
  "mds" 'org-schedule
  "mdd" 'org-deadline
  "mdt" 'org-time-stamp
  "mt" '(org-todo :which-key "todo state set")
  "mq" '(org-set-tags-command :which-key "set tags menu")
  "mQ" '(counsel-org-tag :which-key "set tags list menu")
  "mp" '(org-set-property :which-key "set property")
  "me" '(org-set-effort :which-key "set effort"))
                                        ; C-c org schedule and deadline and time-stamp and org-tags, etc
                                        ; for tag multi-add alt-enter!

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
                                        ;(org-bullets-bullet-list '("⁖" "◉" "○" "✸" "✿")))
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; replace list hyphen with dot
                                        ;(font-lock-add-keywords 'org-mode
                                        ;                        '(("^ *\\([-]\\) "
                                        ;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; visual-fill-mode (padding)
(defun ri/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ri/org-mode-visual-fill)
  :config
  (setq visual-fill-column-enable-sensible-window-split nil))

;; --- org-journal ------
(use-package org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        ;; org-journal-date-format "%B %d, %Y (%A) "
        ;; org-journal-file-format "%Y-%m-%d.org"
        ))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("un" . "src conf-unix")))

;; org-babel (tangle n stuff)
;; Automatically tangle our Emacs.org config file when we save it
(defun ri/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;;                                  ^ Formerly user-emacs-directory (now .cache/emacs/)
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ri/org-babel-tangle-config)))

(ri/leader-keys
  "ob"  '(:ignore t :which-key "org-babel")
  "obt" '(org-babel-tangle :which-key "tangle")
  "obe" '(org-babel-execute-src-block :which-key "org-babel-execute-src-block"))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; breadcrumb automatically enables...
;; also "file symbols" is already default args...
(defun ri/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . ri/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (message "lsp-mode loaded!"))

;; change these later...
;; prefix is the keys that come before?!?!
;; maybe i just don't have the right packages to make lsp-mode-map
                                        ;(ri/ctrl-c-keys
;; (general-define-key
;;   :keymaps 'lsp-mode-map
;;   :prefix lsp-keymap-prefix
;;   "l"   '(:ignore t :which-key "lsp")
;;   "lg"  '(:ignore t :which-key "find")
;;   "lgd" 'lsp-find-definition
;;   "lgr" 'lsp-find-references))

;; ;; maybe
;; ;
                                        ; can't define same keys twice? naw.
;; ok what the heck
;; (ri/leader-keys
;;   :keymaps 'lsp-mode-map
;;   "ml"  '(:ignore t :which-key "lsp-find")
;;   "mgd" '(lsp-find-definition :wk "definition")
;;   "mgr" '(lsp-find-references :wk "references")
;;   "mrr" '(lsp-rename :wk "rename all"))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom ;; defcustom !!
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(ri/leader-keys
  "ml" 'org-lint)

;; rust-analyzer required. gnu guix package?
(use-package rustic
                                        ;:ensure t ;; no need *
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rustic-format-on-save nil)

  ;; lsp-mode ----
  (lsp-rust-analyzer-cargo-watch-command "check")
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "never") ; skip_trivial
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil) ; def nil
  (lsp-rust-analyzer-display-closure-return-type-hints nil)
  (lsp-rust-analyzer-display-parameter-hints nil) ; def nil
  (lsp-rust-analyzer-display-reborrow-hints "never")

  ;; lsp-ui ----
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t))

(use-package python-mode
                                        ;:ensure t ;; no need *
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package company
  :hook (lsp-mode . company-mode)
  :bind
  ((:map company-active-map
         ("<tab>" . company-complete-selection))
   (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
   (:map company-search-map
         ("C-t" . company-select-previous-or-abort)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  ;; fixes evil-normal and cancel company autocomplete when escape
  ;; doesn't work if escape hit very quickly
  :if (featurep 'evil-mode)
  :config
  (add-hook 'company-mode-hook
            (lambda ()
              (add-hook 'evil-normal-state-entry-hook
                        (lambda ()
                          (company-abort))))))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; projectile
;; (project management)
;; (bound to C-p)
;; (dir-locals are pretty cool)
;; (learn more about projectile for better project management)
(use-package projectile
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ;; by default auto
  ;; :bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code/code")
    (setq projectile-project-search-path '("~/Code/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
;; (more options in M-o... already installed?)
;; (counsel-projectile-rg + M-o for a massive search in project)
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; magit
;; (add several links...)
;; (magit-status is C-x g)
;; (tab to see diff of files)
;; (hunks, "?" for all commands, C-c C-k to quit commit, push to remote, ssh?)
;; (learn more about magit...)
(use-package magit
  :commands magit-status
  ;; :bind (:map magit-status-mode-map
  ;;             ("p" . magit-tag)
  ;;             ("t" . magit-section-backward))
  :custom
  ;; what does this do? fullscreen?
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(ri/leader-keys
  "v"  '(:ignore t :which-key "magit")
  "vv" '(magit-status :which-key "magit")) ; (same as magit)

;; forge
;; (run forge-pull in a repo to pull down)
;; (pull down all issues, pull-reqs, etc)
;; (need to create a token first, then put in .authinfo)
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package aggressive-indent
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package evil-nerd-commenter
  :custom
  (evil-want-keybinding nil)
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  ;; vv already set vv
  ;;(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

(use-package shell-pop
  :commands shell-pop
  :custom
  (shell-pop-universal-key nil)
  (shell-pop-default-directory "/home/mio")
  (shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell)))))
  (shell-pop-term-shell "/bin/zsh")
  (shell-pop-window-size 40)
  (shell-pop-window-position "bottom"))

;; eshell config
(defun ri/configure-eshell ()
  ;; Save command history when commands are entered.
  ;;   Commands usually don't save until close, so if crashes, loses all progress.
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;; fixes the issue with cursor going to the beginning... fixed?
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

;; themes
(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . ri/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "ssh")))

  (eshell-git-prompt-use-theme 'powerline))

(ri/leader-keys
  "at" '(vterm :which-key "vterm")
  "ae" '(eshell :which-key "eshell"))

(ri/leader-keys
  "f"  '(:ignore t :which-key "files")
  "fr" '(counsel-recentf :which-key "recent files")
  "ff" '(find-file :which-key "find-file")
  "fp" '(lambda () (interactive)
          (find-file (expand-file-name "~/.dotfiles/.emacs.d/"))
          :which-key "open Emacs.org"))

;; dired
(use-package dired
  :ensure nil ; make sure use-package doesn't try to install it.
  :commands (dired dired-jump) ; defer loading of this config until a command is executed.
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (delete-by-moving-to-trash t)
  ;;(dired-compress-files-alist) ; add more file types to compression.
  :if (featurep 'evil-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "f" 'dired-create-empty-file))
;;     ^ Might not work if using two dired windows! (dired-up-directory, dired-find-file)

;; HAS TO COME AFTER dired because using ":after dired"
;; Reuses the current Dired buffer to visit a directory without
;; creating a new buffer.
(use-package dired-single
  :after dired)

(defun ri/all-the-icons-dired-helper ()
  (interactive)
  (if (equal dirvish-override-dired-mode nil)
      (all-the-icons-dired-mode)))

(use-package all-the-icons-dired
  :hook (dired-mode . ri/all-the-icons-dired-helper))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions
        '(("mkv" . "mpv")
          ("png" . "feh"))))

;; ----custom-function-----------

;; >>> Consider replacing dired hide dotfiles with dired-x dired omit files?

;; if enabled, when my-dired-mode-hook is run, re-enable dired-hide-dotfiles-mode
(defvar ri/dired-hide-dotfiles-mode--persist 1)

(defun ri/dired-hide-dotfiles-mode--toggle ()
  ;; when run, toggles dired-hide-dotfiles-mode and assistant var
  (interactive)
  (if dired-hide-dotfiles-mode
      (dired-hide-dotfiles-mode 0)
    (dired-hide-dotfiles-mode 1))
  (setq ri/dired-hide-dotfiles-mode--persist dired-hide-dotfiles-mode))

;; ------------------------------

(use-package dired-hide-dotfiles
  ;; :commands (dired dired-jump)
  :config
  ;; Does this even work?!?!?!?!?!?
  (define-key dired-mode-map (kbd "z") 'ri/dired-hide-dotfiles-mode--toggle)
  :if (featurep 'evil-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'ri/dired-hide-dotfiles-mode--toggle))

(defun my-dired-mode-hook ()
  (if ri/dired-hide-dotfiles-mode--persist
      (dired-hide-dotfiles-mode)))
                                        ;
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(ri/leader-keys
  "d"  '(:ignore t :which-key "dired")
  "dd" 'dired
  "dj" 'dired-jump
  "dh" 'ri/dired-hide-dotfiles-mode--toggle)

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :commands (dired dired-jump)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                            "Home")
     ("o" "~/org/"                        "Org")
     ("d" "~/Downloads/"                  "Downloads")
     ("e" "/home/mio/.dotfiles/.emacs.d/" "Emacs user directory")
     ("p" "~/Pictures/"                   "Pictures")
     ("c" "~/Code/"                       "Code")
     ("m" "/mnt/"                         "Drives")
     ("t" "~/.local/share/Trash/files/"   "Trash")))
  :config
  ;; (define-key dirvish-mode-map (kbd "z") 'ri/dired-hide-dotfiles-mode--toggle)
  (setq dired-listing-switches
        ;; "-ahgo --group-directories-first"
        "-l --almost-all --human-readable --group-directories-first --no-group") ; AhoG
  (setq dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (setq delete-by-moving-to-trash t)
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  :bind
  ("C-c d D" . dirvish)
  ("C-c d f" . dirvish-fd)
  (:map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
        ("h" . dired-up-directory)
        ("r" . dired-sort-toggle-or-edit)
        ("s" . dired-open-file)
        ("z" . 'ri/dired-hide-dotfiles-mode--toggle)

        ("a"   . dirvish-quick-access)))

;; ("f"   . dirvish-file-info-menu)
;; ("y"   . dirvish-yank-menu)
;; ("N"   . dirvish-narrow)
;; ("^"   . dirvish-history-last)
;; ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;; ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;; ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;; ("TAB" . dirvish-subtree-toggle)
;; ("M-f" . dirvish-history-go-forward)
;; ("M-b" . dirvish-history-go-backward)
;; ("M-l" . dirvish-ls-switches-menu)
;; ("M-m" . dirvish-mark-menu)
;; ("M-t" . dirvish-layout-toggle)
;; ("M-s" . dirvish-setup-menu)
;; ("M-e" . dirvish-emerge-menu)
;; ("M-j" . dirvish-fd-jump)))

;; doesn't work... what is tramp?
(use-package sudo-edit
  :custom
  (sudo-edit-local-method "doas")
  :config
  (global-set-key (kbd "C-c C-r") 'sudo-edit))

(use-package dired-toggle-sudo
  :commands (dired dired-jump)
  :bind
  (:map dired-mode-map
        ("C-c C-s" . dired-toggle-sudo)))

;; rss
;; maybe don't need, phone is enough?
;; maybe syncthing and import from database?
;; dont use commands elfeed, scan at startup?
(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '("http://pragmaticemacs.com/feed/")
        ))

(ri/leader-keys
  "ar" 'elfeed)

;; eww is shite, also SPC and h trigger prefix. w3 browser?
;; disable cookies, or delete history after closing?
(setq browse-url-browser-function 'eww-browse-url)

(use-package eww
  :bind (:map eww-mode-map
              ("n" . next-line) ;; orig next-url
              ("]" . eww-next-url)
              ("[" . eww-previous-url)
              ("T" . eww-top-url)
              ("t" . nil)
              ("H-t" . previous-line))) ;; (physically "p")

(use-package gnus
  :commands gnus
  :config
  (setq user-full-name '"aili")
  (setq user-mail-address '"yourname@email.invalid")
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods '((nntp "news.gmane.io")
                                        ;(nntp "news.alt.religion.emacs")
                                        ;(nntp "gnu.emacs.sex")
                                       ))

  (setq gnus-directory (concat user-emacs-directory "News/")
        gnus-startup-file (concat user-emacs-directory "News/.newsrc")
        message-directory (concat user-emacs-directory "Mail/")))

  ;;(setq gnus-secondary-select-methods '((nntp "alt.religion.emacs")))

;; matrix client
(use-package ement
  :commands ement)

(use-package jabber
   :commands jabber)

;; erc
;; make erc start after startup?
;; good channels: ##furry #transchat-social
(use-package erc
  :commands erc)

(use-package mastodon
  :commands mastodon)

;; Keep customization settings in a temporary file (does this even work?)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; org binding on M-t so make all t key bindings translate to p ?

;; also god mode

;(global-set-key (kbd "C-h") 'backward-kill-word)
(global-set-key (kbd "C-t") 'previous-line)

(global-set-key (kbd "C-u") ctl-x-map)
;; (global-set-key (kbd "C-z") 'universal-argument)
;; (global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-g") 'universal-argument)

;; make gc pauses faster by decreaseing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
