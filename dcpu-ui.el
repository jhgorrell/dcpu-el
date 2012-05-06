;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-ui.el ---
;;
;; $Id: dcpu-ui.el,v 1.23 2012/05/06 05:22:18 harley Exp $
;;

(eval-when-compile
  (require 'cl))

(require 'dcpu-defs)
(require 'dcpu-display)
(require 'dcpu-cpu)

;;;;;

(defvar dcpu:ui-active nil)
(defvar dcpu:ui-enter-hook nil)
(defvar dcpu:ui-exit-hook nil)

(defvar dcpu:ui-reg-win    nil)
(defvar dcpu:ui-mem-win    nil)
(defvar dcpu:ui-screen-win nil)
(defvar dcpu:ui-main-win   nil)
(defvar dcpu:ui-right-win  nil)

(defvar dcpu:ui-window-config nil)
(defvar dcpu:ui-window-orig-config nil)

(defvar dcpu:ui-main-width 80)

;;;;;

(defvar dcpu:ui-menu-key [f11])
(defvar dcpu:ui-run-key  [f12])

(defvar dcpu:ui-menu-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map dcpu:ui-menu-key 'dcpu:ui-toggle)
    ;;
    (define-key map "R" 'dcpu:ui-cpu-reset)
    ;;
    (define-key map "l" 'dcpu:ui-load-mem)
    (define-key map "s" 'dcpu:ui-save-mem)
    ;;
    (define-key map "w1" 'dcpu:ui-goto-reg-win)
    (define-key map "w2" 'dcpu:ui-goto-mem-win)
    (define-key map "w3" 'dcpu:ui-goto-screen-win)
    (define-key map "w4" 'dcpu:ui-goto-main-win)
    (define-key map "w5" 'dcpu:ui-goto-right-win)
    (define-key map "wm" 'dcpu:ui-goto-mem-win)
    (define-key map "wr" 'dcpu:ui-goto-reg-win)
    (define-key map "ws" 'dcpu:ui-goto-screen-win)
    (define-key map "wt" 'dcpu:ui-view-trace)
    (define-key map "ww" 'dcpu:ui-goto-main-win)
    ;;
    (define-key map "b" 'dcpu:ui-set-break)
    (define-key map "c" 'dcpu:ui-clear-break)
    (define-key map "m" 'dcpu:ui-add-areg)
    (define-key map "u" 'dcpu:ui-update)
    ;;
    (define-key map "1" 'dcpu:ui-run-speed-1)
    (define-key map "2" 'dcpu:ui-run-speed-2)
    (define-key map "3" 'dcpu:ui-run-speed-3)
    ;;
    (define-key map "t" 'dcpu:trace-toggle)
    ;;
    (define-key map "C" 'dcpu:ui-toggle-screen-color)
    ;;
    (define-key map "?" 'dcpu:ui-help)
    ;;
    ;;(define-key map "g" 'dasm:)
    map))

(defvar dcpu:ui-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map dcpu:ui-menu-key dcpu:ui-menu-keymap)
    (define-key map dcpu:ui-run-key 'dcpu:ui-run)
    map)
  "")

(define-minor-mode dcpu:ui
  "A global minor mode which controls the DCPU emulator and settings.
\\<dcpu:ui-keymap>
\\[dcpu:ui-run] runs the emulator. (Prefix arg is num of instrs)
\\[dcpu:ui-toggle] toggles the window layout to the emulator and back.

\\{dcpu:ui-keymap}
" ;; doc
  :init-value t
  :global t
  :lighter " dcpu"
  :keymap dcpu:ui-keymap
  (when dcpu:ui
    (message "dcpu:ui powerers activated!")))
;; (dcpu:ui t)

(defun dcpu:ui-help ()
  "Shows the help for dcpu:ui minor mode."
  (interactive)
  (describe-minor-mode 'dcpu:ui))
;; (dcpu:ui-help)

;;;;;

(defun dcpu:ui-set-break ()
  (interactive)
  (let ((addr (read-number "dcpu: set breakpoint addr: " (or dcpu:pc 0))))
    (message "dcpu: breakpoint: %s" addr)
    (dcpu:set-breakpoint-addrs addr)))

(defun dcpu:ui-clear-break ()
  (interactive)
  (let ((addr (read-number "dcpu: clear breakpoint addr: " (or dcpu:pc 0))))
    (dcpu:clear-breakpoint-addrs addr)))

(defvar dcpu:ui-add-mem-list-len 32)

(defun dcpu:ui-add-areg ()
  (interactive)
  (let ((addr (read-number "dcpu: add mem addr: " (or dcpu:tmp-addr dcpu:pc 0)))
        (len  (read-number "dcpu: add mem len: " (or dcpu:ui-add-mem-list-len 32)))
        (data (read-string "dcpu: add mem format: ")))
    (let ((areg (dcpu:make-aregion :s addr :l len :d (intern data))))
      (dcpu:aregionlist-push 'dcpu:display-areg-list areg)))
  nil)

;;;;;

(defun dcpu:ui-select-window (win)
  (if (window-live-p win)
    (select-window win)
    (error "not live")))

(defun dcpu:ui-goto-reg-win ()
  (interactive)
  (dcpu:ui-select-window dcpu:ui-reg-win))
;; (dcpu:ui-goto-reg-win)

(defun dcpu:ui-goto-mem-win ()
  (interactive)
  (dcpu:ui-select-window dcpu:ui-mem-win))
;; (dcpu:ui-goto-mem-win)

(defun dcpu:ui-goto-screen-win ()
  (interactive)
  (dcpu:ui-select-window dcpu:ui-screen-win))
;; (dcpu:ui-goto-screen-win)

(defun dcpu:ui-goto-main-win ()
  (interactive)
  (dcpu:ui-select-window dcpu:ui-main-win))
;; (dcpu:ui-goto-main-win)

(defun dcpu:ui-goto-right-win ()
  (interactive)
  (dcpu:ui-select-window dcpu:ui-right-win))
;; (dcpu:ui-goto-right-win)

(defun dcpu:ui-view-trace ()
  (interactive)
  (let ((tbuf (get-buffer-create dcpu:trace-bufname)))
    (if (eq tbuf (current-buffer))
      (bury-buffer)
      (switch-to-buffer tbuf))))

;;;;;

(defun dcpu:current-window ()
  ;; we do this as
  ;; (get-buffer-window (current-buffer))
  ;; seems to return the lower numbered window
  ;; when that buffer is already in a window.
  (let ((start-buf (current-buffer))
        (win nil))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (setq win (get-buffer-window (current-buffer))))
    (switch-to-buffer start-buf)
    win))
;; (dcpu:current-window)

;; this function ignores "ARG"; but replacements might use it.
(defun dcpu:build-standard-ui (&optional arg)
  (let ((main-buf (current-buffer)))
    ;; one window
    (delete-other-windows)
    ;; wide? split and use the left side.
    (let ((ww (window-width)))
      (setq dcpu:ui-right-win nil)
      (when (< 130 ww)
        ;; 1+ for the scrollbar
        (split-window-horizontally (1+ dcpu:ui-main-width))
        (other-window 1)
        (setq dcpu:ui-right-win (dcpu:current-window))
        (other-window -1)))
    ;; start with the reg window and work down.
    (switch-to-buffer (get-buffer-create dcpu:reg-bufname))
    (setq dcpu:ui-reg-win (dcpu:current-window))
    (set-window-dedicated-p dcpu:ui-reg-win t)
    ;; create the mem window
    (split-window-vertically 5)
    (other-window 1)
    (switch-to-buffer (get-buffer-create dcpu:mem-bufname))
    (setq dcpu:ui-mem-win (dcpu:current-window))
    (set-window-dedicated-p dcpu:ui-mem-win t)
    ;; create the main
    (split-window-vertically dcpu:ui-mem-lines)
    (setq dcpu:ui-main-win (dcpu:current-window))
    ;; now go back to create the screen window.
    (select-window dcpu:ui-mem-win)
    (split-window-horizontally -33)
    (other-window 1)
    (switch-to-buffer (get-buffer-create dcpu:screen-bufname))
    (setq dcpu:ui-screen-win (dcpu:current-window))
    (set-window-dedicated-p dcpu:ui-screen-win t)
    ;;
    (select-window dcpu:ui-main-win)
    (switch-to-buffer main-buf)
    ;;
    (setq dcpu:ui-window-config (current-window-configuration))
    ;;
    (dcpu:ui-select-window dcpu:ui-main-win)
    nil))
;; (progn (eval-buffer) (dcpu:build-standard-ui)
;; (window-width)

(defun dcpu:ui-enter (&optional arg)
  (interactive "P")
  (if (not dcpu:ui-active)
    (setq dcpu:ui-window-orig-config (current-window-configuration)))
  (let ((buf (current-buffer)))
    (if (or (not arg) (not dcpu:ui-window-config))
      (dcpu:build-standard-ui arg))
    (set-window-configuration dcpu:ui-window-config)
    ;; keep the current buffer in the main window.
    (select-window dcpu:ui-main-win)
    (switch-to-buffer buf))
  (setq dcpu:ui-active t)
  (dcpu:ui-update)
  (run-hooks dcpu:ui-enter-hook))

(defun dcpu:ui-exit ()
  (interactive)
  (if dcpu:ui-window-orig-config
    (set-window-configuration dcpu:ui-window-orig-config))
  (setq dcpu:ui-active nil)
  (run-hooks dcpu:ui-exit-hook))

(defun dcpu:ui-toggle (arg)
  (interactive "P")
  (message "ui-toggle: %s" arg)
  (cond
   ((null arg)
    (if dcpu:ui-active
      (dcpu:ui-exit)
      (dcpu:ui-enter)))
   ((equal arg 0)
    (dcpu:ui-exit))
   ((numberp arg)
    (dcpu:ui-enter arg))
   ;; what should we do with C-u?
   (t
    (error ""))))
;; (dcpu:ui-toggle)

(defun dcpu:ui-update ()
  (interactive)
  ;;
  (dcpu:display-regs)
  (dcpu:display-mem)
  (dcpu:display-screen)
  nil)

;;;;;

(defun dcpu:ui-cpu-reset (arg)
  (interactive "p")
  ;; test arg
  (dcpu:init-regs)
  (dcpu:ui-update))

(defun dcpu:ui-run (arg)
  (interactive "p")
  (message "ui-run: %s" arg)
  (dcpu:run-loop (if (= 0 arg) t arg) t))

;;;;;

(defun dcpu:ui-load-mem (filename)
  (interactive "fLoad mem from file: ")
  (dcpu:load-from-file filename))

(defun dpcu:ui-save-mem (filename)
  (interactive "FSave mem to file: ")
  (dcpu:save-to-file filename))

;;;;;

(defun dcpu:ui-run-speed-1 ()
  "Turn off updates and run fast."
  (interactive)
  (setq dcpu:run-sit-for nil)
  (dcpu:run-loop t t)
  nil)

(defun dcpu:ui-run-speed-2 ()
  "Run fast with updates."
  (interactive)
  (setq dcpu:run-sit-for 0.1)
  (dcpu:run-loop t t)
  nil)

(defun dcpu:ui-run-speed-3 ()
  "Run slow with updates."
  (interactive)
  (setq dcpu:run-sit-for 0.5)
  (dcpu:run-loop t t)
  nil)

;;;;;

(defun dcpu:ui-toggle-screen-color (&optional arg)
  (interactive "P")
  (cond
   ((null arg)
    (setq dcpu:screen-color (not dcpu:screen-color)))
   ((equal 0 arg)
    (setq dcpu:screen-color nil))
   ((numberp arg)
    (setq dcpu:screen-color t)))
  (dcpu:display-screen))
;; (dcpu:ui-toggle-screen-color)

;; (dcpu:ui-enter t)
;; (dcpu:ui-enter)
;; (dcpu:ui-exit)
;; (dcpu:ui-update)
;; (progn (eval-buffer) (dcpu:ui nil)  (dcpu:ui t))

(provide 'dcpu-ui)
