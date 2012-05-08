;;
;; ~/0x10c/dcpu-el/dev-kbd.el ---
;;
;; $Id: dev-kbd.el,v 1.1 2012/05/08 05:44:50 harley Exp $
;;

(require 'dcpu-dev-defs)

;;;;;

(defvar dev:kbd-wait-secs 2.0)

;;;;;

(defun dev:make-kbd ()
  (let ((dev (dcpu:make-dev)))
    (setf (dcpu:dev-hwq-func dev) 'dev:kbd-hwq-func
          (dcpu:dev-hwi-func dev) 'dev:kbd-hwi-func)
    dev))
;; (setq dcpu:cur-dev (dev:make-kbd))

(defun dev:kbd-hwq-func (dev)
  ;; 0x30cf7406
  (setf dcpu:b #x30cf
        dcpu:a #x7406
        dcpu:c 1
        dcpu:y #x0
        dcpu:x #x0)
  dev)
;; (dev:kbd-hwq-func dcpu:cur-dev)

(defun dev:kbd-hwi-func (dev)
  (cond
   ;; clear buffer
   ((= dcpu:a 0)
    (setf (dcpu:dev-data dev) nil))
   ;; get next key from buffer
   ((= dcpu:a 1)
    ;;(pop (dcpu:dev-data dev))
    (let ((c (read-char "dcpu:keyboard: " nil dev:kbd-wait-secs)))
      (setq dcpu:c (if c c 0))))
   ;; return 1 if key pressed
   ((= dcpu:a 2)
    ;; we dont test for keys being held down in emacs.
    (setq dcpu:c 0))
   ;; set the
   ((= dcpu:a 3)
    nil)
   (t
    (error "")))
  dev)

;; (progn (setq dcpu:a 1 dcpu:c nil) (dcpu:dev-hwi dcpu:cur-dev) dcpu:c)

;; (eval-buffer)
(provide 'dev-kbd)