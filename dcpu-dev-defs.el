;;
;; ~/0x10c/dcpu-el/dev-defs.el ---
;;
;; $Id: dcpu-dev-defs.el,v 1.1 2012/05/08 05:44:50 harley Exp $
;;

(eval-when-compile
  (require 'cl))

;;;;;

(defstruct 
  (dcpu:dev
   (:conc-name dcpu:dev-)
   (:constructor dcpu:make-dev))
  name
  ;;
  hwq-func
  hwi-func
  ;; spot to park per-dev info
  data
  aregion1
  aregion2
  )

;;
(defvar dcpu:cur-dev nil)

(defun dcpu:dev-hwq (dev)
  (let ((func (dcpu:dev-hwq-func dev)))
    (if func
      (funcall func dev))))

(defun dcpu:dev-hwi (dev)
  (let ((func (dcpu:dev-hwi-func dev)))
    (if func
      (funcall func dev))))

(provide 'dcpu-dev-defs)
