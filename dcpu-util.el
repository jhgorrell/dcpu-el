;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-util.el ---
;;
;; $Id: dcpu-util.el,v 1.9 2012/05/04 08:14:35 harley Exp $
;;

(eval-when-compile
  (require 'cl))

;;;;;

(defvar dcpu:msg-bufname " *dcpu msg*")

(defun dcpu:msg (&rest args)
  (with-current-buffer (get-buffer-create dcpu:msg-bufname)
    (goto-char (point-max))
    (insert (apply 'format args) "\n")
    (switch-to-buffer-other-window (current-buffer))))
;; (dcpu:msg "%s" 1)
;; (dcpu:msg "%s" "string")

(defun dcpu:msg-erase ()
  (with-current-buffer (get-buffer-create dcpu:msg-bufname)
    (erase-buffer)))
;; (dcpu:msg-erase)

(defvar dcpu:pp1-bufname " *dcpu pp1*")

(defun pp1 (obj)
  (with-current-buffer (get-buffer-create dcpu:pp1-bufname)
    (erase-buffer)
    (pp obj (current-buffer))
    (switch-to-buffer-other-window (current-buffer))))
;; (pp1 1)

;;;;; Math

(defmacro dcpu:u16 (w0)
  `(logand ,w0 #xFFFF))

(defmacro dcpu:u16+ (w0 w1)
  `(dcpu:u16 (+ ,w0 ,w1)))

;; (format "%x" -1)
(defmacro dcpu:sext (w0)
  `(let ((w1 (dcpu:u16 ,w0)))
     (if (< w1 #x8000)
       w1
       (- w1 #x10000))))

(defmacro dcpu:s16+ (w0 w1)
  `(+ (dcpu:sext ,w0) (dcpu:sext ,w1)))

;;;;; opcodes

;; aaaa|aabb|bbbo|oooo

;; basic:   aaaaaa bbbbb ooooo
;;           rA/6   rB/5  op/5
;;
;; special: aaaaaa ooooo 00000
;;           rA/6   op/5   Z/5

(defmacro dcpu:extract-op (instr)
  `(logand ,instr #x1F))
;; (dcpu:extract-op #x0001F)

(defmacro dcpu:extract-ra (instr)
  `(logand (lsh ,instr -10) #x3F))
;; (dcpu:extract-ra #x0000)

(defmacro dcpu:extract-rb (instr)
  `(logand (lsh ,instr -5) #x1F))
;; (dcpu:extract-rb #x0010)

(defun dcpu:fmt-addr (addr)
  ""
  (cond
   ((numberp addr)
    (format "%04x" addr))
   (t
    "????")))

(defun dcpu:fmt-wordb (word)
  (cond
   ((numberp word)
    (format "%04x" word))
   (t
    "    ")))

(defun dcpu:fmt-wordq (word)
  (cond
   ((numberp word)
    (format "%04x" word))
   (t
    "????")))

;;;;;

(defun dcpu:fmt-reg-name (rn)
  (format "%s" rn))

;;;;;

(defun dcpu:insert-string2words (str)
  (interactive "sString:")
  (insert ";; " str "\n")
  (let ((i 0))
    (while (< i (length str))
      (when (= 0 (mod i 8))
        (when (/= 0 i)
          (insert "\n"))
        (insert "word "))
      (insert (format " 0x%04x," (elt str i)))
      (setq i (1+ i)))
    (insert "\n")))
;; (dcpu:insert-string2words "hello")

;;
(provide 'dcpu-util)
