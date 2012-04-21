;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-ui.el ---
;;
;; $Id: dcpu-ui.el,v 1.14 2012/04/21 21:54:31 harley Exp $
;;

(require 'dcpu-display)
(require 'dcpu-cpu)

(eval-when-compile
  (require 'cl))

;;;;;

(defvar dcpu:ui-active nil)
(defvar dcpu:ui-enter-hook nil)
(defvar dcpu:ui-exit-hook nil)

(defvar dcpu:ui-reg-win    nil)
(defvar dcpu:ui-mem-win    nil)
(defvar dcpu:ui-screen-win nil)
(defvar dcpu:ui-main-win   nil)

(defvar dcpu:ui-window-config nil)
(defvar dcpu:ui-window-orig-config nil)

;;;;;

(defvar dcpu:ui-menu-key [f11])
(defvar dcpu:ui-run-key  [f12])

(defvar dcpu:ui-menu-keymap 
  (let ((map (make-sparse-keymap)))
    (define-key map dcpu:ui-menu-key 'dcpu:ui-toggle)
    ;;
    (define-key map "R" 'dcpu:cpu-reset)
    ;;
    (define-key map "l" 'dcpu:ui-load-mem)
    (define-key map "s" 'dcpu:ui-save-mem)
    ;;
    (define-key map "b" 'dcpu:ui-set-break)
    (define-key map "c" 'dcpu:ui-clear-break)
    (define-key map "m" 'dcpu:ui-add-mem-list)
    ;;
    (define-key map "1" 'dcpu:ui-run-speed-1)
    (define-key map "2" 'dcpu:ui-run-speed-2)
    (define-key map "3" 'dcpu:ui-run-speed-3)
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

(defun dcpu:ui-add-mem-list ()
  (interactive)
  (let ((addr (read-number "dcpu: add mem addr: " (or dcpu:addr dcpu:pc 0)))
        (len (read-number "dcpu: add mem len: " (or dcpu:ui-add-mem-list-len 32))))
    (setq dcpu:ui-add-mem-list-len len)
    (dcpu:add-to-mem-list addr len)
    ;; update
    ;;(dcpu:display-mem-list)
    nil))
;; (dcpu:ui-add-mem-list)

;;;;;

;; this function ignores "ARG"; but replacements might use it.
(defun dcpu:build-standard-ui (&optional arg)
  (let ((buf (current-buffer)))
    ;; one window
    (delete-other-windows)
    ;; reg
    (split-window-vertically 5)
    (switch-to-buffer (get-buffer-create dcpu:reg-bufname))
    (setq dcpu:ui-reg-win (get-buffer-window (current-buffer)))
    ;; mem
    (other-window 1)
    (switch-to-buffer (get-buffer-create dcpu:mem-bufname))
    (setq dcpu:ui-mem-win (get-buffer-window (current-buffer)))
    ;; the main
    (split-window-vertically dcpu:ui-mem-lines)
    (other-window 1)
    (switch-to-buffer buf)
    (setq dcpu:ui-main-win (get-buffer-window (current-buffer)))
    ;; now go back to split the mem window into
    (select-window dcpu:ui-mem-win)
    (split-window-horizontally -33)
    (other-window 1)
    (switch-to-buffer (get-buffer-create dcpu:screen-bufname))
    (setq dcpu:ui-screen-win (get-buffer-window (current-buffer)))
    ;;
    (setq dcpu:ui-window-config (current-window-configuration))
    nil))
;; (progn (eval-buffer) (dcpu:build-standard-ui))
;; (select-window dcpu:ui-reg-win)
;; (select-window dcpu:ui-main-win)

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
  (save-excursion
    (dcpu:display-regs)
    (dcpu:display-mem)
    (dcpu:display-screen)
    nil))

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
  (setq dcpu:sit-for nil)
  (dcpu:run-loop t t)
  nil)

(defun dcpu:ui-run-speed-2 ()
  "Run fast with updates."
  (interactive)
  (setq dcpu:sit-for 0.1)
  (dcpu:run-loop t t)
  nil)

(defun dcpu:ui-run-speed-3 ()
  "Run slow with updates."
  (interactive)
  (setq dcpu:sit-for 0.5)
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
