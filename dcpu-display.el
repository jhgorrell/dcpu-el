;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-display.el ---
;;
;; $Id: dcpu-display.el,v 1.10 2012/04/14 04:58:59 harley Exp $
;;

(require 'dcpu-defs)
(require 'dcpu-util)
(eval-when-compile (require 'cl))

;;;;;

(defface dcpu:reg
  '((t
     :background "yellow"))
  ""
  :group 'dcpu)

(defface dcpu:addr
  '((t
     :background "green"))
  ""
  :group 'dcpu)

(defface dcpu:pc
  '((t
     :background "red"))
  ""
  :group 'dcpu)

;;;;;

(defun dcpu:iwf (&rest args)
  (let ((cur-face nil)
        arg)
    (while args
      (setq arg (car args)
            args (cdr args))
      (cond
       ((stringp arg)
        (insert (propertize arg 'face cur-face)))
       ((facep arg)
        (setq cur-face arg))
       (t
        (error ""))))))

(defun dcpu:dump-mem-line (addr cnt)
  (when dcpu:mem-vec
    (setf addr (dcpu:u16 addr))
    (dotimes (i cnt)
      (insert " " (dcpu:fmt-word (dcpu:get-mem addr)))
      (setf addr (dcpu:u16 (1+ addr))))))

;;;;;

(defun dcpu:display-r1 (n r)
  (dcpu:iwf 'dcpu:reg
            (format "%7s:" n)
            'default " "
            (cond
             ((null r)
              "?")
             ((symbolp r)
              (dcpu:fmt-word (dcpu:get-reg r)))
             (t
              (format "%4s" r)))
            'default " "))

(defun dcpu:display-regs ()
  (with-current-buffer (get-buffer-create dcpu:reg-bufname)
    (erase-buffer)
    ;;
    (dcpu:display-r1 "A" 'A)
    (dcpu:display-r1 "B" 'B)
    (dcpu:display-r1 "C" 'C)
    (dcpu:display-r1 "I" 'I)
    (dcpu:display-r1 "icnt" (dcpu:get-reg 'icnt))
    (insert "\n")
    ;;
    (dcpu:display-r1 "X" 'X)
    (dcpu:display-r1 "Y" 'Y)
    (dcpu:display-r1 "Z" 'Z)
    (dcpu:display-r1 "J" 'J)
    (dcpu:display-r1 "cycles" (dcpu:get-reg 'cycles))
    (insert "\n")
    ;;
    (let ((addr (dcpu:get-reg 'PC)))
      (dcpu:display-r1 "PC" (dcpu:fmt-word addr))
      (insert " =>")
      (dcpu:dump-mem-line addr 3)
      (if (dcpu:get-reg 'skip)
        (insert " " (propertize "SKIP" 'face 'dcpu:reg) " "))
      ;; dont
      (let ((tmp-break (delete 'icnt-max (copy-list dcpu:break))))
        (if tmp-break
          (insert
           "          "
           (propertize "  break:" 'face 'dcpu:reg)
           (format " %s" tmp-break))))
      (insert "\n"))
    ;;
    (let ((addr (dcpu:get-reg 'SP)))
      (dcpu:display-r1 "SP" (dcpu:fmt-word addr))
      (insert " =>")
      (dcpu:dump-mem-line addr 8)
      (insert "\n"))
    ;;
    nil))

;; (progn (eval-buffer) (dcpu:display-regs))

(defun dcpu:add-to-mem-list (addr len)
  (setq dcpu:display-mem-list 
        (dcpu:sort-mem-list
         (cons (list addr len) dcpu:display-mem-list))))

(defun dcpu:display-mem (&optional addr-list)
  (if addr-list
    (setq dcpu:display-mem-list addr-list))
  ;;
  (with-current-buffer (get-buffer-create dcpu:mem-bufname)
    (erase-buffer)
    (let ((addr-list dcpu:display-mem-list))
      (while addr-list
        (apply 'dcpu:dump-mem (car addr-list))
        (setf addr-list (cdr addr-list))
        (if addr-list
          ;; add ";" so it can be loaded
          (insert ";     ---------------------------------------\n"))))))
;; (dcpu:display-mem '((0 32) (#xffe0 32)))
;; (dcpu:display-mem '((0 32)))
;; (dcpu:display-mem)

;; @todo width
(defun dcpu:dump-mem (addr len)
  (let ((addr-end (+ addr len)))
    (while (< addr addr-end)
      (when (= 0 (logand addr #x07))
        (dcpu:iwf 'dcpu:addr (dcpu:fmt-addr addr) ":"))
      ;;
      (dotimes (i 8)
        (setf word (dcpu:get-mem addr)
              wstr (dcpu:fmt-word word))
        (cond
         ;; color
         ((equal addr dcpu:pc)
          (setf wstr (propertize wstr 'face 'dcpu:pc)))
         ((equal addr dcpu:sp)
          (setf wstr (propertize wstr 'face 'dcpu:pc)))
         ;;
         (t t))
        (insert " " wstr)
        (incf addr))
      (insert "\n"))))

;; (dcpu:dump-mem 0 8)

;;;;;

(defun dcpu:screen-word2char (word)
  (if (null word)
    "_"
    (when (numberp word)
      (setq word (logand word #xFF))
      (cond
       ((= 0 word)
        " ")
       ((and (<= 32 word) (<= word 127))
        (make-string 1 word))
       (t
        ".")))))

(defun dcpu:display-screen ()
  (interactive)
  (with-current-buffer (get-buffer-create dcpu:screen-bufname)
    (erase-buffer)
    (let ((addr dcpu:screen-addr))
      (dotimes (y dcpu:screen-y)
        (dotimes (x dcpu:screen-x)
          (insert (dcpu:screen-word2char (dcpu:get-mem addr)))
          (incf addr))
        (insert "\n")))))
;; (progn (dcpu:set-mem-bulk #x9000 "hello there!\n\n") (dcpu:screen-update))
;; (dcpu:screen-update)

;;;;;

(defun dcpu:trace-append (&rest args)
  (with-current-buffer (get-buffer-create dcpu:trace-bufname)
    (goto-char (point-max))
    (apply 'insert args)))
;; (dcpu:trace-append "1" "2" "3")

(defun dcpu:trace-log (&rest args)
  (dcpu:trace-append
   ;;
   (if (numberp dcpu:pc)
     (propertize (format "%04x |" dcpu:pc) 'face 'dcpu:addr)
     "     |")
   ;;
   " "
   (apply 'format args)
   "\n")
  nil)
;; (dcpu:trace-log "%s" "foo")

;; called with dcpu:pc pointing at the instr.
(defun dcpu:trace-instr ()
  (dcpu:trace-log (dcpu:fmt-word (dcpu:get-mem dcpu:pc)))
  nil)

(defun dcpu:trace-buffer-clear ()
  (interactive)
  (with-current-buffer (get-buffer-create dcpu:trace-bufname)
    (erase-buffer)))
;; (dcpu:trace-buffer-clear)


;; (eval-buffer)
(provide 'dcpu-display)
