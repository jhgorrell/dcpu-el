;;
;; ~/0x10c/dcpu-el/dcpu-cli.el ---
;;
;; $Id: dcpu-cli.el,v 1.3 2012/05/03 16:34:07 harley Exp $
;;

(eval-when-compile
  (require 'cl))

;; (require 'cl)
(defvar dcpu:cli-main-args nil)

;;(message "dcpu-cli: %s" argv)
;;(message "dcpu-cli: %s" command-line-args)

(require 'dcpu)

(defun dcpu:cli-main ()
  ;; take the rest of the for us.
  (message "dcpu:cli-main: %s" command-line-args-left)
  (setq dcpu:cli-main-args command-line-args-left
        command-line-args-left nil)
  (message "dcpu:cli-main-args: %s" dcpu:cli-main-args)
  ;;
  (dcpu:init-cpu)
  (when (car dcpu:cli-main-args)
    (dcpu:load-file (car dcpu:cli-main-args)))
  ;;
  (dcpu:ui-enter)
  nil)

;; dcpu:cli-main-args
;; (dcpu:cli-main)
;; (dcpu:cli-main)
