;;
;; ~/0x10c/dcpu-el/dcpu-cli.el ---
;;
;; $Id: dcpu-cli.el,v 1.1 2012/04/10 20:04:09 harley Exp $
;;

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
  (dcpu:ensure-active-cpu)
  (when (car dcpu:cli-main-args)
    (dcpu:load-file (car dcpu:cli-main-args)))
  ;;
  (dcpu:standard-ui)
  (dcpu:ui-update)
  )

;; dcpu:cli-main-args
;; (dcpu:cli-main)
;; (dcpu:cli-main)
