(defun ri/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun ri/set-wallpaper ()
  (interactive)
  (start-process-shell-command
    "feh" nil "feh --bg-fill ~/Pictures/wallpapers/oneshot-wallpaper1.jpg"))

(defun ri/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
         (pos-y (cdr pos)))

  (exwm-floating-move (- pos-x) (- pos y))))

;; also preview themes package
;(defun ri/random-wallpaper ()
;  (interactive))

;; depends on nm-applet, pasystray(?), pavucontrol(?), and blueman
(defun ri/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 0)
  ;; Launch programs at startup 
  ;(eshell)
  ;; Run programs in background at startup 
  ;(ri/run-in-background "nm-applet")
  ;(ri/run-in-background "pasystray")
  ;(ri/run-in-background "blueman-applet")
  )

;; switch to last workspace (hack)
(defun ri/exwm-workspace-switch-to-last ()
  "Switch to the workspace that was used before current workspace"
  (interactive)
  (exwm-workspace-switch (cdr ri/exwm-workspace--switch-history-hack)))

;; switch to next workspace
(defun ri/exwm-workspace-switch-to-next ()
  "Switch to the next workspace"
  (interactive)
  (exwm-workspace-switch (+ exwm-workspace-current-index 1)))

;; switch to previous workspace
(defun ri/exwm-workspace-switch-to-previous ()
  "Switch to the previous workspace"
  (interactive)
  (exwm-workspace-switch (- exwm-workspace-current-index 1)))

;; switch to last window (any frame inclusive)
(defun ri/switch-to-last-window-any-frame ()
  "Switch to the last window, including "
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

;; switch to last window
(defvar ri/last-window-direction 1)
(defun ri/switch-to-last-window ()
  (interactive)
  (other-window ri/last-window-direction)
  (setq ri/last-window-direction (- 0 ri/last-window-direction)))

(defun ri/exwm-update-class ()
  (exwm-workspace-rename-buffer (format "EXWM: %s" exwm-class-name)))

(defun ri/exwm-update-title ()
  (pcase exwm-class-name
    ("librewolf" (exwm-workspace-rename-buffer (format "Librewolf: %s" exwm-title)))
    ("vterm" (exwm-workspace-rename-buffer (format "vterm: %s" exwm-title)))))

(defun ri/exwm-configure-window-startup ()
  (message "Window '%s' appeared!" exwm-class-name)
  (pcase exwm-class-name
    ("librewolf" (exwm-workspace-move-window 3))
    ("keepassxc" (exwm-workspace-move-window 4))
    ("discord" (exwm-workspace-move-window 2))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))
    ("gimp" (exwm-workspace-move-window 1))))
    ;; if game has trouble put it in exwm-input-release-keyboard, and then s-r to reset.

;; run when startup floating window
(defun ri/exwm-configure-floating-setup ()
  (exwm-layout-hide-mode-line))

;; shrink and expand windows... 

;; spawn mpv in corner of screen so can watch youtube video

;; make function keys work in fullscreen... always pass to emacs instead of exwm.

;; move focus onto popup window

; -------

(defun ri/run-keepassxc()
  (interactive)
  (ri/run-in-background "keepassxc")
  (exwm-workspace-switch-create 4))
;;
(defun ri/run-librewolf ()
  (interactive)
  (ri/run-in-background "librewolf")
  (exwm-workspace-switch-create 3))

; -------

;; very annoying?
;; makes the cursor visible? 
;; only warp when on X window buffer!
;; warps useful when on EXWM-mode with pop-ups
;; when on EXWM, enable mouse. When on emacs, disable mouse.
(use-package exwm-mff
  :config
  (exwm-mff-mode nil))
  ;; ^ should already be nil...

;; warp cursor to center when switch to a X window buffer.
(add-hook 'exwm-manage-finish-hook (lambda () (exwm-mff-warp-to-selected)))

;; automatically balance windows after splitting
(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; -------

;; for floating minibuffer...
;; ; show minibuffer in separate frame?
;; ; show minibuffer on polybar? make it pop up from under polybar?
;; ; auto-hiding minibuffer at top of screen, and posframe for all else but quit.
;; ; polybar at top, print minibuffer messages, stable, bottom is only modeline

(use-package ivy-posframe
  :config
  (setq ivy-posframe-height-alist '((swiper . 20)
                                    (counsel-M-x . 40)))

  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          ;(counsel-M-x    . ivy-posframe-display-at-window-bottom-left)
          (counsel-M-x     . ivy-display-function-fallback)
          (counsel-switch-buffer . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
  ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-mode 0))

;; -------

(use-package exwm
  :bind 
  ("C-M-h" . 'ri/exwm-workspace-switch-to-previous)
  ("C-M-l" . 'ri/exwm-workspace-switch-to-next)
  ("C-M-k" . 'ri/exwm-workspace-switch-to-last)
  :custom
  ;; Systray addons
  (display-time-day-and-date t)
  (display-time-24hr-format t)
  (display-time-mode t)
  (display-battery-mode t)
  (display-time-default-load-average nil)
  :config
  (setq exwm-workspace-number 5 ; 0-5
        focus-follows-mouse nil
        ;exwm-workspace-warp-cursor t ;?
        exwm-debug nil) ;enable for debug mode

  ;; -------

  ;; When EXWM starts up, run some functions
  (add-hook 'exwm-update-class-hook #'ri/exwm-init-hook)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'ri/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'ri/exwm-update-title)

  ;; Configure windows as they're created (includes spawn in certain wkspaces)
  ;; For automoving to workspace, use a function.
  (add-hook 'exwm-manage-finish-hook #'ri/exwm-configure-window-startup)

  ;; Hide the modeline on all floating windows
  (add-hook 'exwm-floating-setup-hook #'ri/exwm-configure-floating-setup)

  ;; hook and command to go to the previous workspace
  (defvar ri/exwm-workspace--switch-history-hack (cons exwm-workspace-current-index '()))
  ;;
  (add-hook 'exwm-workspace-switch-hook
     (lambda ()
       (setq ri/exwm-workspace--switch-history-hack
              (cons exwm-workspace-current-index
                (car ri/exwm-workspace--switch-history-hack)))))

  ;; --------

  ;; Sets up dvorak keybinds (also does ctrl:nocaps?)
  ;(start-process-shell-command "setxkbmap qwerty,dvorak ctrl:nocaps" nil "setxkbmap -layout 'us,us' -variant 'dvorak,' -option grp:alts_toggle ctrl:nocaps")
  ;;
  ;; Rebind Ctrl to CapsLock 
  ;; (start-process-shell-command "Xmodmap S-C Esc" nil 
     ;; (concat "xmodmap " (concat user-emacs-directory "exwm/Xmodmap")))

  ;; set hold type startup and speed
  ;; (start-process-shell-command "xset typing speed" nil "xset r rate 300 40")

  ;; set cursor type (what if not enabled?)
  ;; (start-process-shell-command "xsetroot cursor" nil "xsetroot -cursor_name left_ptr")

  ;; night light 
  (ri/run-in-background (expand-file-name "exwm/sct-auto-adjust.sh" user-emacs-directory))

  ;; -------

  ;; See all X windows with exwm-switch-to-buffer, so can pull into current workspace
  (setq exwm-layout-show-all-buffers nil)

  ;; Display all EXWM buffers in every workspace buffer list
  ;; Could always have another keybind that shows all active EXWM buffers...
  (setq exwm-workspace-show-all-buffers nil)

  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  (setq exwm-workspace-minibuffer-position 'nil)

  ;; -------

  ;; set screen resolution (arandr to graphically extract xrandr command).
  (require 'exwm-randr)
  (exwm-randr-enable) ; set resolution before init.
  (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1 --off --output DP-1 --off --output DP-2 --off --output DP-3 --off --output DP-4 --off")

  ;; set wallpaper (after xrandr so can get correct dimensions)
  (ri/set-wallpaper)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height nil)
  (exwm-systemtray-enable)

  ;; ---------

  ;; send to emacs instead of X application
  (setq exwm-input-prefix-keys
        '(?\M-x
          ?\M-:
          ?\M-`
          ?\M-o
          ?\M-&
          ?\C-x
          ?\C-h
          ?\C-u
          ?\C-\M-j  ;;  buffer list 
          ?\C-\M-h  ;;  previous workspace
          ?\C-\M-l  ;;  next workspace
          ?\C-\M-k  ;;  last workspace
          ?\C-\ ))  ;;  C-<space>

  ;; C-q to send next key to X-applicaiton
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([?\s-h] . windmove-left)
          ([?\s-t] . windmove-up)
          ([?\s-n] . windmove-down)
          ([?\s-s] . windmove-right)

          ;; Move windows 
          ([?\s-H] . windmove-swap-states-left)
          ([?\s-T] . windmove-swap-states-up)
          ([?\s-N] . windmove-swap-states-down)
          ([?\s-S] . windmove-swap-states-right)

          ;; Shortcuts for windows 
          ([?\s-g] . (lambda () (interactive) (other-window 1)))
          ([?\s-c] . (lambda () (interactive) (other-window -1)))
          ([?\s-d] . delete-window)
          ;;;; alternatively, s-u + s-S-u for windows, and s-i for launcher or sum (comfy?)
          ;;;; or maybe s-o for windows and s-u s-i for buffers?

          ;; buffers and more
          ([?\s-o] . evil-switch-to-windows-last-buffer)
          ([?\s-b] . counsel-switch-buffer)

          ;; Toggles
          ([?\s-F] . exwm-floating-toggle-floating)
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-m] . exwm-layout-toggle-mode-line)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Special
          ([?\s-\C-\S-l] . (lambda () (interactive) (desktop-environment-lock-screen)))

          ;; Programs

          ;; Switch workspace
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-\M-c] . ri/exwm-workspace-switch-to-last)
          ([?\s-\M-l] . (lambda () (interactive) (ri/exwm-workspace-switch-to-next)))
          ([?\s-\M-h] . (lambda () (interactive) (ri/exwm-workspace-switch-to-previous)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;; s-i instead?
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-<return>") 'shell-pop)
  (exwm-input-set-key (kbd "s-/ l") 'ri/run-librewolf)
  (exwm-input-set-key (kbd "s-/ p") 'ri/run-keepassxc)

  (exwm-enable))

;; depends on scrot (screenshot), brightnessctl (brightness), and playerctl (player)
(use-package desktop-environment
  :after exwm
  :config 
  (setq desktop-environment-update-exwm-global-keys :prefix)
  (define-key desktop-environment-mode-map (kbd "s-l") nil) ;; what is this???
  (desktop-environment-mode)
  :custom
  ;; brightness
  (desktop-environment-brightness-normal-increment "10%+")
  (desktop-environment-brightness-normal-decrement "10%-")
  (desktop-environment-brightness-small-increment "5%+")
  (desktop-environment-brightness-small-decrement "5%-")
  ;; volume
  (desktop-environment-volume-normal-decrement "-10%")
  (desktop-environment-volume-normal-increment "+10%")
  (desktop-environment-volume-small-decrement "-5%")
  (desktop-environment-volume-small-increment "+5%")
  (desktop-environment-volume-set-command "pactl set-sink-volume @DEFAULT_SINK@ %s")
  (desktop-environment-volume-get-command "bash ~/.emacs.d/exwm/pactl-print-volume-w-mute.sh")
  (desktop-environment-volume-get-regexp "\\(.*\\)")
  (desktop-environment-volume-toggle-command "pactl set-sink-mute @DEFAULT_SINK@ toggle && bash ~/.emacs.d/exwm/pactl-print-mute.sh")
  (desktop-environment-volume-toggle-regexp "\\(.*\\)" )
  ;; screenshot
  (desktop-environment-screenshot-command "flameshot gui"))

;; ivy-posframe to have a floating minibuffer.
;; maybe have disappearing minibuffer above the modeline?
(use-package ivy-posframe
  :disabled
  :after exwm)

;; copy cuts to system clipboard
;;  security concerns? private emacs better? make a keybind?
;; (use-package xclip
;;   :after exwm
;;   :config
;;   (xclip-mode 1))


