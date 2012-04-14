;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-util.el ---
;;
;; $Id: dcpu-util.el,v 1.4 2012/04/14 04:58:59 harley Exp $
;;

(defmacro dcpu:u16 (w0)
  `(logand ,w0 #xFFFF))

(defmacro dcpu:u16+ (w0 w1)
  `(logand (+ ,w0 ,w1) #xFFFF))

;;;;;;;;;;

(defmacro dcpu:extract-op (instr)
  `(logand ,instr #xF))
;; (dcpu:extract-op #x0008)
(defmacro dcpu:extract-ra (instr)
  `(logand (lsh ,instr -4) #x3F))
;; (dcpu:extract-ra #x0000)
(defmacro dcpu:extract-rb (instr)
  `(logand (lsh ,instr -10) #x3F))
;; (macroexpand '(dcpu:extract-rb zzz))
;; (dcpu:extract-rb #xF000)

;;;;;;;;;;

(defun dcpu:fmt-addr (addr)
  ""
  (cond 
   ((numberp addr)
    (format "%04x" addr))
   (t
     "????")))

(defun dcpu:fmt-word (word)
  (cond 
   ((numberp word)
    (format "%04x" word))
   (t
    "????")))

(defun dcpu:fmt-reg-name (rn)
  (format "%s" rn))

;;;;;

(defun dcpu:insert-string2dat (str)
  (interactive "sString:")
  (insert "; " str "\n")
  (let ((i 0))
    (while (< i (length str))
      (when (= 0 (mod i 8))
        (when (/= 0 i)
          (insert "\n"))
        (insert "dat "))
      (insert (format " 0x%04x," (elt str i)))
      (setq i (1+ i)))
    (insert "\n")))
;; (dcpu:insert-string2dat "hello")

;;;;

(defun dcpu:sort-mem-list (memlst)
  (sort (copy-seq memlst) (lambda (a b) (< (car a) (car b)))))
;; (dcpu:sort-mem-list '((10 16) (0 16) (100 16)))

(provide 'dcpu-util)
