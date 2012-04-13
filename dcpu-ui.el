;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-ui.el ---
;;
;; $Id: dcpu-ui.el,v 1.7 2012/04/13 05:50:47 harley Exp $
;;

(require 'dcpu-display)
(eval-when-compile (require 'cl))

;;;;;

(defvar dcpu:ui-reg-win    nil)
(defvar dcpu:ui-mem-win    nil)
(defvar dcpu:ui-screen-win nil)
(defvar dcpu:ui-main-win   nil)

(defvar dcpu:ui-window-config nil)
(defvar dcpu:ui-window-orig-config nil)

(defun dcpu:build-standard-ui ()
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
  (setq dcpu:ui-window-orig-config (current-window-configuration))
  (let ((buf (current-buffer)))
    (if (or arg (not dcpu:ui-window-config))
      (dcpu:build-standard-ui)
      (set-window-configuration dcpu:ui-window-config))
    (select-window dcpu:ui-main-win)
    (set-buffer buf))
  (dcpu:ui-update))

(defun dcpu:ui-exit ()
  (interactive)
  (if dcpu:ui-window-orig-config
    (set-window-configuration dcpu:ui-window-orig-config)))

;; (dcpu:ui-enter t)
;; (dcpu:ui-enter)
;; (dcpu:ui-exit)
     
(defun dcpu:ui-update ()
  (interactive)
  (save-excursion
    (dcpu:display-regs)
    (dcpu:display-mem)
    (dcpu:display-screen)
    nil))
;; (dcpu:ui-update)


(provide 'dcpu-ui)
