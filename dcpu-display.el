;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-display.el ---
;;
;; $Id: dcpu-display.el,v 1.26 2012/05/06 05:22:18 harley Exp $
;;

(eval-when-compile
  (require 'cl))

(require 'dcpu-defs)
(require 'dcpu-aregion)
(require 'dcpu-util)
(require 'dcpu-screen)
(require 'dcpu-disasm)
(require 'dcpu-cpu)
(require 'dcpu-faces)

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

(defun dcpu:display-color-by-addr (txt addr)
  (cond
   ;; color
   ((equal addr dcpu:pc)
    (setf txt (propertize txt 'face 'dcpu:pc)))
   ((equal addr dcpu:sp)
    (setf txt (propertize txt 'face 'dcpu:sp))))
  txt)

;;;;;

(defun dcpu:dump-mem-line (addr cnt)
  (when dcpu:mem-vec
    (setf addr (dcpu:u16 addr))
    (dotimes (i cnt)
      (insert " " (dcpu:fmt-wordq (dcpu:mem-get addr)))
      (setf addr (dcpu:u16 (1+ addr))))))

;;;;;

(defun dcpu:display-r1 (name val)
  (dcpu:iwf
   'dcpu:reg
   (format "%7s:" name)
   'default " "
   (cond
    ((null val)
     "?")
    ((numberp val)
     (format "%04x" val))
    (t
     (format "%4s" val)))
   'default " "))

(defun dcpu:display-regs ()
  (with-current-buffer (get-buffer-create dcpu:reg-bufname)
    (erase-buffer)
    ;;
    (dcpu:display-r1 "A" dcpu:a)
    (dcpu:display-r1 "B" dcpu:b)
    (dcpu:display-r1 "C" dcpu:c)
    (dcpu:display-r1 "I" dcpu:i)
    (dcpu:iwf 'dcpu:reg "   icnt:")
    (insert (format " %5d" dcpu:state-icount))
    (insert "\n")
    ;;
    (dcpu:display-r1 "X" dcpu:x)
    (dcpu:display-r1 "Y" dcpu:y)
    (dcpu:display-r1 "Z" dcpu:z)
    (dcpu:display-r1 "J" dcpu:j)
    (dcpu:iwf 'dcpu:reg " cycles:")
    (insert (format " %5d" dcpu:state-cycles))
    (insert "\n")
    ;;
    (let ((addr dcpu:pc))
      (dcpu:display-r1 "PC" (dcpu:fmt-wordq addr))
      (insert "=>")
      (dcpu:dump-mem-line addr 3)
      (insert " | ")
      (insert (dcpu:disasm-addr dcpu:pc))

      (if dcpu:state-skip
        (insert " " (propertize "SKIP" 'face 'dcpu:reg) " "))
      ;; dont show max-icount, it is boring.
      (let ((tmp-break (delete 'max-icount (copy-tree dcpu:state-breaks))))
        (if tmp-break
          (insert
           "          "
           (propertize "  break:" 'face 'dcpu:reg)
           (format " %s" tmp-break))))
      (insert "\n"))
    ;;
    (let ((addr dcpu:sp))
      (dcpu:display-r1 "SP" (dcpu:fmt-wordq addr))
      (insert "=>")
      (dcpu:dump-mem-line addr 8)
      (insert "\n"))
    ;;
    nil))
;; (dcpu:display-regs)
;; (progn (eval-buffer) (dcpu:display-regs))

;; (setq ack-arguments '("-H" "--sort-files"))
(defun dcpu:display-mem (&optional areg-list)
  (or areg-list (setq areg-list dcpu:display-areg-list))
  ;;
  (with-current-buffer (get-buffer-create dcpu:mem-bufname)
    (erase-buffer)
    (let (areg)
      (while areg-list
        (setq areg (car areg-list)
              areg-list (cdr areg-list))
        (dcpu:display-areg areg)
        (if areg-list
          ;; add ";" so it can be loaded
          (dcpu:iwf 'dcpu:mem-hr ";     ---------------------------------------\n"))))))

;; @todo center mem-buffer on current focus.
;;       check dcpu:display-mem-addr, dcpu:cur-addr, dcpu:mem-addr
;;       update dcpu:display-mem-addr-prior from them.
;;
;; @todo Make a list of interesting markers in the mem buffer
;;       remember which marker is being looked at
;;       and center on it
;;       markers at the top of each areg
;;       keys to flip between markers.
;;       F11-up moves mem window

(defun dcpu:display-areg (areg)
  (cond
   ((eq (dcpu:aregion-data areg) 'disassemble)
    (dcpu:display-areg-disassmble areg))
   (t
    (dcpu:display-areg-words areg))))

(defun dcpu:display-areg-disassmble (areg)
  (let ((addr  (dcpu:aregion-start  areg))
        (addr-end (dcpu:aregion-end areg))
        i-len w0 w1 w2)
    (while (<= addr addr-end)
      (setq w0 (dcpu:mem-get addr)
            w1 (dcpu:mem-get (+ addr 1))
            w2 (dcpu:mem-get (+ addr 2)))
      (setq i-len (dcpu:disasm-instr-len w0))
      (when (/= 0 w0)
        (insert
         (propertize (format "%04x:" addr) 'face 'dcpu:addr)
         " "
         ;;(format "%d " i-len)
         (dcpu:display-color-by-addr (dcpu:fmt-wordb w0) addr)
         (format
          " %s %s ; %s\n"
          (dcpu:fmt-wordb (if (<= 2 i-len) w1))
          (dcpu:fmt-wordb (if (<= 3 i-len) w2))
          (dcpu:disasm-to-str w0 w1 w2))))
      (setq addr (+ addr i-len)))
    areg))
;; (dcpu:display-mem (list (dcpu:make-aregion :s 0 :l 3 :data 'disassemble)))

;; @todo width
(defun dcpu:display-areg-words (areg)
  (dcpu:dump-words (dcpu:aregion-start areg) (dcpu:aregion-end areg)))

(defun dcpu:dump-words (addr len)
  (let ((addr-end (+ addr len))
        word wstr)
    (while (< addr addr-end)
      (dcpu:iwf 'dcpu:addr (dcpu:fmt-addr addr) ":")
      ;;
      (dotimes (i 8)
        (setf word (dcpu:mem-get addr)
              wstr (dcpu:fmt-wordq word))
        (setq wstr (dcpu:display-color-by-addr wstr addr))
        (insert " " wstr)
        (incf addr))
      (insert "\n"))))
;; (dcpu:dump-mem 0 8)

(defun dcpu:display-clear-aregs ()
  (setq dcpu:display-areg-list nil))

(defun dcpu:display-add-areg (areg)
  (dcpu:aregionlist-push 'dcpu:display-areg-list areg))

;;;;;

(defun dcpu:screen-word2char (word)
  (if (null word)
    "_"
    (when (numberp word)
      (setq word (logand word #xFF))
      (cond
       (dcpu:screen-do-map-chars
        (elt dcpu:screen-char-table word))
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
    (let ((addr dcpu:screen-addr)
          word char)
      (dotimes (y dcpu:screen-y-size)
        (dotimes (x dcpu:screen-x-size)
          (setq word (dcpu:mem-get addr))
          (setq char (dcpu:screen-word2char word))
          (if dcpu:screen-color
            (setq char (dcpu:screen-color-char char (logand (ash word -8) #xFF))))
          (insert char)
          (incf addr))
        (insert "\n")))))

;; (progn (dcpu:mem-set-bulk #x8000 "hello there!\n\n") (dcpu:display-screen))
;; (dcpu:screen-update)

;;;;;

(defun dcpu:trace-append (&rest args)
  (with-current-buffer (get-buffer-create dcpu:trace-bufname)
    (goto-char (point-max))
    (apply 'insert args)))
;; (dcpu:trace-append "1" "2" "3")

(defun dcpu:trace-msg (&rest args)
  (if dcpu:trace
    (dcpu:trace-append (apply 'format arg) "\n")))

(defun dcpu:trace-msga (&rest args)
  (when dcpu:trace
    (dcpu:trace-append
     ;;
     (propertize
      (if (numberp dcpu:pc)
        (format "%04x |" dcpu:pc)
        "     |")
      'face 'dcpu:addr)
     " "
     ;;
     (if args
       (apply 'format args)
       "")
     "\n"))
  nil)
;; (dcpu:trace-msga "%s" "foo")

;; called with dcpu:pc pointing at the instr.
(defun dcpu:trace-instr ()
  (dcpu:trace-log (dcpu:fmt-wordq (dcpu:mem-get dcpu:pc)))
  nil)

(defun dcpu:trace-buffer-clear ()
  (interactive)
  (with-current-buffer (get-buffer-create dcpu:trace-bufname)
    (erase-buffer)))
;; (dcpu:trace-buffer-clear)

(defun dcpu:trace-show ()
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create dcpu:trace-bufname)))
;; (dcpu:trace-show)

(defun dcpu:trace-toggle (&optional arg)
  (interactive "P")
  (cond
   ((null arg)
    (setq dcpu:trace (not dcpu:trace)))
   ((eq arg 0)
    (setq dcpu:trace nil))
   (t
    (setq dcpu:trace t)))
  ;;
  (if (called-interactively-p 'interactive)
    (message 
     "dcpu:trace is %s"
     (if dcpu:trace
       "on"
       "off")))
  dcpu:trace)
;; (dcpu:trace-toggle)
;; (dcpu:trace-toggle 0)
;; (dcpu:trace-toggle t)

;; (eval-buffer)
(provide 'dcpu-display)
