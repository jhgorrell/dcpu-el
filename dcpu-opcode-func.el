;;
;; ~/0x10c/dcpu-el/dcpu-opcode-func.el ---
;;
;; $Id: dcpu-opcode-func.el,v 1.12 2012/05/08 00:39:20 harley Exp $
;;

(eval-when-compile
  (require 'cl))

(require 'dcpu-elisp)
(require 'dcpu-util)
(require 'dcpu-defs)

;;;;;

(defun dcpu:opcodes-clear ()
  (fillarray dcpu:opcode-basic-vec   nil)
  (fillarray dcpu:opcode-special-vec nil)
  (clrhash dcpu:opcode-hash-table))
;; (dcpu:opcodes-clear)
;; (eval-buffer)

(defun dcpu:defopcode-func (name body)
  (let ((opc (dcpu:make-opcode)))
    (setf (dcpu:opcode-name opc) name)
    (puthash name opc dcpu:opcode-hash-table)
    (while body
      (cond
       ((eql (car body) :cycles)
        (setf (dcpu:opcode-cycles opc) (cadr body))
        (setq body (cddr body)))
       ((eql (car body) :opcode)
        (setf (elt dcpu:opcode-basic-vec (cadr body)) opc)
        (setf (dcpu:opcode-opcode opc) (cadr body))
        (setq body (cddr body)))
       ((eql (car body) :special)
        (setf (elt dcpu:opcode-special-vec (cadr body)) opc)
        (setf (dcpu:opcode-specialcode opc) (cadr body))
        (setq body (cddr body)))
       (t
        (setf (dcpu:opcode-body opc) body)
        (setq body nil))))
    opc))
;; (dcpu:defopcode-func 'foo nil)

(defmacro dcpu:defopcode (name &rest body)
  (dcpu:defopcode-func name body))

(defun dcpu:opcodes-list ()
  (let ((lst nil))
    (maphash (lambda (k v) (push v lst)) dcpu:opcode-hash-table)
    lst))
;; (dcpu:opcodes-list)

(defun dcpu:opcode-dump (opc &optional dump-body)
  (dcpu:msg
   "%2s | %-6s | %s"
   (dcpu:opcode-cycles opc)
   (if (dcpu:opcode-specialcode opc)
     (format "S:%02x" (dcpu:opcode-specialcode opc))
     (format "%02x" (dcpu:opcode-opcode opc)))
   (dcpu:opcode-name opc))
  (when dump-body
    (dcpu:msg "%s" (pp-to-string (dcpu:opcode-body opc)))))
;; (dcpu:opcode-dump (car (dcpu:opcodes-list)) t)


(defun dcpu:opcode-cmp-name (opc-a opc-b)
  (string<
   (dcpu:opcode-name opc-a)
   (dcpu:opcode-name opc-b)))

(defun dcpu:opcode-cmp-code (opc-a opc-b)
  (cond
   ((and
     (dcpu:opcode-specialcode opc-a)
     (dcpu:opcode-specialcode opc-b))
    (< (dcpu:opcode-specialcode opc-a)
       (dcpu:opcode-specialcode opc-b)))
   ((and (dcpu:opcode-specialcode opc-a)
         (not (dcpu:opcode-specialcode opc-b)))
    t)
   ((and (not (dcpu:opcode-specialcode opc-a))
         (dcpu:opcode-specialcode opc-b))
    nil)
   (t
    (< (or (dcpu:opcode-opcode opc-a) 100)
       (or (dcpu:opcode-opcode opc-b) 100)))))

(defun dcpu:opcodes-dump (&optional sort-func show-body)
  (let ((opc-lst (dcpu:opcodes-list)))
    (setq opc-lst (sort opc-lst (or sort-func 'dcpu:opcode-cmp-code)))
    (mapc '(lambda (opc) (dcpu:opcode-dump opc show-body)) opc-lst)
    nil))
;; (progn (dcpu:opcodes-clear) (eval-buffer) (dcpu:opcodes-dump nil t))
;; (dcpu:opcodes-dump)

(defun dcpu:opcode-get (op)
  (gethash op dcpu:opcode-hash-table nil))

(defun dcpu:opcode-cast (opc)
  (if (dcpu:opcode-p opc)
    opc
    (dcpu:opcode-get opc)))
;; (dcpu:opcode-cast (dcpu:opcode-cast 'set))

(defun dcpu:opcode-basic-p (opc)
  (setq opc (dcpu:opcode-cast opc))
  (if (dcpu:opcode-opcode opc)
    t))
;; (dcpu:opcode-basic-p 'set)

(defun dcpu:gen-instr-if-lst ()
  (dcpu:sort-strings
   (dcpu:to-strings
    (dcpu:map-select
     (lambda (opc)
       (let ((n (dcpu:opcode-opcode opc)))
         (if (and (numberp n) (<= #x10 n) (<= n #x17))
           (dcpu:opcode-name opc))))
     (dcpu:opcodes-list)))))
;; (dcpu:gen-instr-if-lst)

(defun dcpu:gen-instr-lst ()
  (dcpu:sort-strings
   (dcpu:to-strings
    (dcpu:map-select
     (lambda (opc)
       (if (or (dcpu:opcode-opcode opc)
               (dcpu:opcode-specialcode opc))
         (dcpu:opcode-name opc)))
     (dcpu:opcodes-list)))))
;; (dcpu:gen-instr-lst)

;; (progn (eval-buffer) (dcpu:opcodes-clear) (load "dcpu-opcode-list"))
(provide 'dcpu-opcode-func)
