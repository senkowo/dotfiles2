;; The default is 800 kilobytes (measured in bytes)
(setq gc-cons-threshold (* 50 1000 1000))

(defun ri/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'ri/display-startup-time)

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
(unless (package-installed-p 'use-package) ; -p function (predicate: true/nil) is this installed?
   (package-install 'use-package)) ; install use-package using package-install if not installed.

;; setup use-package ----
(require 'use-package) 
(setq use-package-always-ensure t) ;; no need to add :ensure t on every package that needs it

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "14:00"))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; no-littering
(use-package no-littering)

;; keep autosaves in emacs dir
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; disable startup screen
(setq inhibit-startup-message nil) 

;; disable ui 
(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode 1)     ; disable tooltips
(set-fringe-mode 10) ; give some breathing room
(menu-bar-mode -1)   ; disable menu bar

;; add line numbers
(global-display-line-numbers-mode t)
(column-number-mode) ; (columns on modeline)

;; line number mode exceptions
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed t) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box t) ;; (change to nil) Disable dialog boxes since they weren't working in Mac OSX

;; disable bell
(setq ring-bell-function 'ignore) ; TURN OFF ONCE AND FOR ALL?
(setq visual-bell 1)

;; default font (modeline, minibuffer, default for applications, etc)
(set-face-attribute 'default nil :font "Fira Code" :height 110)
;(set-face-attribute 'default nil :font "JetBrains Mono" :height 115) 

;; fixed pitch font (code blocks, property, startup, etc (can add more))
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)

;; variable pitch font (toc links, regular text in org, etc...)
;; how about Iosveka instead? 
;; (bullets are configured in org-fonts)
(set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120 :weight 'regular)

;; all-the-icons
;; note: on a new machine, must run M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; ESC to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
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
  ;:hook
  ;; have these programs be in emacs-mode (C-z)
  ;(evil-mode-hook . mi/evil-hook)

;; evil collections
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; undo-tree for evil-undo
(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.gnu/undo"))))

;; general.el
(use-package general
  :config
  (general-evil-setup t)

  ;; the definer can be called to add new keybinds.
  ;; far, far better than using a bunch of
  ;;   global-set-key or define-key.
  ;; (keymaps can be swapped with states)
  (general-create-definer ri/leader-keys
    :states '(normal insert visual emacs)
    ;:keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; ;; the modes under keymaps can be put under states, right?
  ;; (general-create-definer ri/leader-keys-mode-map
  ;;   :states '(normal insert visual emacs)
  ;;   :prefix "SPC"
  ;;   :global-prefix "C-SPC")

  (general-create-definer ri/ctrl-c-keys
    :prefix "C-c"))

(ri/leader-keys
  "t" '(:ignore t :which-key "toggles"))

(ri/leader-keys
  "q"  '(:ignore t :which-key "quit/session")
  "qq" '(save-buffers-kill-terminal :which-key "quit emacs"))

;; hydra (fast, transient keybinds)
(use-package hydra)

(defhydra hydra-text-scale (:timeout 5) 
  "scale text"
  ("j" text-scale-decrease "out")
  ("k" text-scale-increase "in")
  ("f" nil "finished" :exit t))

(ri/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(ri/leader-keys
  "w"  '(:ignore t :which-key "window")
  "wv" '(evil-window-vsplit :which-key "v-split")
  "ws" '(evil-window-split :which-key "h-split")
  "wd" '(evil-window-delete :which-key "close window")
  "wc" '(evil-window-delete :which-key "close window")
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
  "b" '(:ignore t :which-key "buffer")
  "bk" '(kill-this-buffer :which-key "kill buffer")
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" 'evil-switch-to-windows-last-buffer
  "bb" 'counsel-switch-buffer)

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
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel 
(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; ivy completion regex and order by last used
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
  :init (load-theme 'doom-snazzy t))

(defun ri/load-theme-and-font-setup ()
  (interactive)
  (counsel-load-theme)
  (ri/org-font-setup))

(ri/leader-keys
  "ht" '(ri/load-theme-and-font-setup :which-key "choose theme"))

;; doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 35))

;; Set frame transparency and maximize windows by default. 
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key (lists keybinds)
;; (add links above source blocks later)
(use-package which-key
  :init (which-key-mode) 
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; helpful (improves help menu)
(use-package helpful
 :custom
 (counsel-describe-function-function #'helpful-callable)
 (counsel-describe-variable-function #'helpful-variable)
 :bind ;; change the function of the command
 ([remap describe-function] . counsel-describe-function)
 ([remap describe-command] . helpful-command)
 ([remap describe-variable] . counsel-describe-variable)
 ([remap describe-key] . helpful-key))

(ri/leader-keys
  "h" '(:ignore t :which-key "help")
  "hf" 'describe-function
  "hv" 'describe-variable
  "hk" 'describe-key
  ;"hb" 'describe-bindings
  "hb" 'counsel-descbinds
  "hm" 'describe-mode
  "hP" 'describe-package
  "hp" 'helpful-at-point)

;; org
(defun ri/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . ri/org-mode-setup)
  ;:custom ; do all setq's go in custom?
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers nil) ; hide formatting chars
  (setq doom-modeline-enable-word-count t))

(ri/leader-keys
  "o"  '(:ignore t :which-key "org")
  "ox" '(eval-last-sexp :which-key "eval-last-sexp")
  "oX" '(eval-region :which-key "eval-region"))

;; org-agenda ----
(use-package org
  :config
  (setq org-deadline-warning-days 14)
  (setq org-agenda-start-with-log-mode t) ; enable log-mode by def
  (setq org-log-done 'time)
  (setq org-log-into-drawer t) ; ?

  ;; agenda files ----
  (setq org-agenda-files
        '("~/org/agenda/agenda.org"
          "~/org/agenda/work.org"
          "~/org/agenda/habits.org"))

  ;; todo keywords ----
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; org-refile ----
  ;; (add target locations for org-refile)
  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("work.org" :maxlevel . 1)))
  ;; save org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; org-habit ----
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  ;; commonly known tasks to appear when counsel-org-tag ----
  ;; org-set-tags-command ?
  (setq org-tag-alist
    '((:startgroup)
      ; Put mutually exclusive tags here
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

  ;; Custom Agenda Views! ----
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

  ;; Org Capture Templates! ----
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
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))


;; keybinds! ----

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
  :keymaps 'org-mode-map
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

(defun ri/org-font-setup ()
  (interactive)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
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

(use-package org
  ;; setup org-fonts after loading org
  ;; set up in :after keyword
  :config
  (ri/org-font-setup))

(use-package org-bullets
  :after org
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
  (add-to-list 'org-structure-template-alist '("co" . "src conf-unix")))

;; org-babel (tangle n stuff)
;; Automatically tangle our Emacs.org config file when we save it
(defun ri/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.dotfiles/.emacs.gnu/"))
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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

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
;;   "lgr" 'lsp-find-references)

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

(use-package lsp-ivy)

;; rust-analyzer required. gnu guix package?
(use-package rustic
  :ensure t
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rustic-format-on-save nil))

(use-package lsp-mode
  :custom
  ;; what to use when checking on-save. use clippy instead?
  (lsp-rust-analyzer-cargo-watch-command "check")
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "never") ; skip_trivial
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil) ; def nil
  (lsp-rust-analyzer-display-closure-return-type-hints nil)
  (lsp-rust-analyzer-display-parameter-hints nil) ; def nil
  (lsp-rust-analyzer-display-reborrow-hints "never"))

(use-package lsp-ui
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
   ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
  :config
  ;; fixes evil-normal and cancel company autocomplete when escape
  ;; doesn't work if escape hit very quickly
  (add-hook 'company-mode-hook
   (lambda ()
     (add-hook 'evil-normal-state-entry-hook
               (lambda ()
                 (company-search-abort)))))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; projectile
;; (project management)
;; (bound to C-p)
;; (dir-locals are pretty cool)
;; (learn more about projectile for better project management)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ;; by default auto
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Programming/code")
    (setq projectile-project-search-path '("~/Programming/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
;; (more options in M-o... already installed?)
;; (counsel-projectile-rg + M-o for a massive search in project)
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(ri/leader-keys
  "p"  '(:ignore t :which-key "project")
  "pp" 'projectile-command-map)

;; magit
;; (add several links...)
;; (magit-status is C-x g)
;; (tab to see diff of files)
;; (hunks, "?" for all commands, C-c C-k to quit commit, push to remote, ssh?)
;; (learn more about magit...)
(use-package magit
  ;:custom
  ;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(ri/leader-keys
  "g"  '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "magit")) ; (same as magit)

;; forge
;; (run forge-pull in a repo to pull down)
;; (pull down all issues, pull-reqs, etc)
;; (need to create a token first, then put in .authinfo)
(use-package forge)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package term
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
  "oe" '(eshell :which-key "eshell"))

(ri/leader-keys
  "f"  '(:ignore t :which-key "files")
  "fr" '(counsel-recentf :which-key "recent files"))

;; provides dired-single commands
(use-package dired-single)

;; dired 
(use-package dired
  :ensure nil ; make sure use-package doesn't try to install it.
  :commands (dired dired-jump) ; defer loading of this config until a command is executed.
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t) ; auto select dir to move to if another dired window open.
  (delete-by-moving-to-trash t)
  ;;(dired-compress-files-alist) ; add more file types to compression.
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
  ;;     ^ Might not work if using two dired windows! (dired-up-directory, dired-find-file)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions
    '(("mkv" . "mpv")
      ("png" . "feh"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(ri/leader-keys
  "d"  '(:ignore t :which-key "dired")
  "dd" 'dired
  "dj" 'dired-jump)

;; matrix client
(use-package ement)

;; rss
;; maybe don't need, phone is enough?
;; maybe syncthing and import from database?
(use-package elfeed)

;; erc
(use-package erc)

;; Keep customization settings in a temporary file (does this even work?)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
