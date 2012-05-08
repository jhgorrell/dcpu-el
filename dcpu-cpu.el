;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-cpu.el ---
;;
;; $Id: dcpu-cpu.el,v 1.30 2012/05/08 05:44:50 harley Exp $
;;

(eval-when-compile
  (require 'cl))

;;
(require 'dcpu-defs)
(require 'dcpu-util)
(require 'dcpu-opcode-list)
;;
(require 'dev-kbd)
(require 'dev-lem)

;;;;;

(defun dcpu:init-regs ()
  (interactive)
  (setf dcpu:a  0
        dcpu:b  0
        dcpu:c  0
        dcpu:x  0
        dcpu:y  0
        dcpu:z  0
        dcpu:i  0
        dcpu:j  0
        dcpu:ex 0
        dcpu:pc 0
        dcpu:sp 0
        dcpu:state-cycles 0
        dcpu:state-icount 0
        dcpu:state-onfire nil
        dcpu:state-skip   nil
        ))

(defun dcpu:init-devs ()
  (let ((dvec (make-vector 16 nil)))
    (setf dcpu:dev-vec dvec)
    (setf (elt dvec 0) (dev:make-kbd))
    (setf (elt dvec 1) (dev:make-lem))
    nil))
;; (dcpu:init-devs)

(defun dcpu:dev-get (idx)
  (and 
   (numberp idx)
   (vectorp  dcpu:dev-vec)
   (< idx (length dcpu:dev-vec))
   (elt dcpu:dev-vec idx)))
;; (dcpu:dev-get 0)

(defun dcpu:init-cpu ()
  (interactive)
  (setq dcpu:mem-vec (make-vector dcpu:mem-size 0))
  (dcpu:init-regs)
  (dcpu:breaks-clear)
  nil)
;; (dcpu:init-cpu)

(defun dcpu:mem-get (addr)
  (elt dcpu:mem-vec addr))
;; (dcpu:mem-get 0)

(defun dcpu:mem-set (addr val)
  (setf (elt dcpu:mem-vec addr) val))
;; (dcpu:mem-set 0 #xFFFF)
;; (dcpu:mem-set #xFFFF 1)

(defun dcpu:mem-set-bulk (addr &rest args)
  (apply 'dcpu:mem-set-list addr args))

(defun dcpu:mem-set-list (addr args)
  (let (arg)
    (while args
      (setq arg  (car args)
            args (cdr args))
      (cond
       ((numberp arg)
        (setf (elt dcpu:mem-vec addr) arg)
        (incf addr))
       ((stringp arg)
        (dotimes (i (length arg))
          (setf (elt dcpu:mem-vec addr) (elt arg i))
          (incf addr)))
       (t
        (error "dcpu:mem-set-bulk"))))
    addr))
;; (dcpu:init-cpu)
;; (dcpu:mem-set-list 0 '(0 1 2 3))

;;;;;

(defun dcpu:checkpoint-val-copy (val)
  (cond
   ((listp val)
    (copy-tree val))
   ((vectorp val)
    (copy-sequence val))
   ((hash-table-p val)
    (copy-hash-table val))
   (t
    val)))

(defun dcpu:checkpoint-var-copy (sym)
  (list sym (dcpu:checkpoint-val-copy (symbol-value sym))))
;; (dcpu:checkpoint-var 'dcpu:a)

(defun dcpu:checkpoint-make ()
  (mapcar 'dcpu:checkpoint-var-copy dcpu:checkpoint-vars))
;; (dcpu:checkpoint-make)

(defun dcpu:checkpoint-save (name)
  (puthash name (dcpu:checkpoint-make) dcpu:checkpoints)
  name)
;; (dcpu:checkpoint-save "aaa")

(defun dcpu:checkpoint-get (name)
  (gethash name dcpu:checkpoints))
;; (dcpu:checkpoint-get "aaa")
;; (dcpu:checkpoint-get "30")

(defun dcpu:checkpoint-load (name)
  (let ((varval-lst (dcpu:checkpoint-get name)))
    (if (not varval-lst)
      (error "Null checkpoint"))
    (mapc
     (lambda (varval)
       (set (car varval) (dcpu:checkpoint-val-copy (cadr varval))))
     varval-lst))
  name)
;; (dcpu:checkpoint-load "aaa")
;; (dcpu:checkpoint-load "30")

;;;;;

;;;#autoload
(defun dcpu:load-from-file (path)
  (when (not (file-readable-p path))
    (error "file is not readable"))
  (let ((buf (get-buffer-create dcpu:load-mem-bufname)))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents path nil)
      (dcpu:load-from-buffer buf))))

(defun dcpu:load-from-buffer (buf)
  ;; @todo compile or assemble buffer?
  (dcpu:load-from-buffer-bin buf))

(defun dcpu:load-from-buffer-bin (buf)
  (with-current-buffer buf
    (goto-char (point-min))
    (let ((addr 0) word)
      (catch 'break
        (while t
          (cond
           ;; whitespace
           ((looking-at "[ \t]+")
            nil)
           ((looking-at "\n")
            nil)
           ;; comment to end of line
           ((looking-at "[#;].*")
            nil)
           ;; "addr:"
           ((looking-at "\\([0-9a-fA-F]+\\):")
            (setq addr (string-to-number (match-string-no-properties 0) 16))
            nil)
           ((looking-at "\\([0-9a-fA-F]+\\)")
            (setq word (string-to-number (match-string-no-properties 0) 16))
            (dcpu:mem-set addr word)
            (setf addr (dcpu:u16+ 1 addr)))
           (t
            (throw 'break nil)))
          ;;(message "LB: %s" (match-string-no-properties 0))
          (goto-char (match-end 0)))))))
;; (dcpu:load-from-file "../programs/test-1.bin")

;;;;;

(defun dcpu:sp-push (val)
  (setf dcpu:sp (dcpu:u16 (1- dcpu:sp)))
  (dcpu:mem-set dcpu:sp val))

(defun dcpu:sp-pop ()
  (prog1
      (dcpu:mem-get dcpu:sp)
    (setf dcpu:sp (dcpu:u16 (1+ dcpu:sp)))))

(defun dcpu:sp-peek (&optional off)
  (let ((addr (dcpu:u16 (+ dcpu:sp (or off 0)))))
    (dcpu:mem-get addr)))

;; (progn (dcpu:sp-push #x5555) (dcpu:sp-peek) (dcpu:sp-pop))

(defun dcpu:pc-next-word ()
  (prog1
      (dcpu:mem-get dcpu:pc)
    (setf dcpu:pc (dcpu:u16 (1+ dcpu:pc)))))
;; (dcpu:pc-next-word) dcpu:pc

;;;;;;;;;;

(defun dcpu:breaks-clear ()
  (setf dcpu:state-breaks nil))
;; (dcpu:breaks-clear)

(defun dcpu:break-set (b)
  (when b
    (push b dcpu:state-breaks)))
;; (dcpu:break-set 'on-fire)

(defun dcpu:break-set-if (c b)
  (if c
    (dcpu:break-set b)))
;; (dcpu:break-set-if t 'true)

;;;;;;;;;;

(defun dcpu:breakpoint-set (addr)
  (puthash addr t dcpu:state-breakpoints))

(defun dcpu:breakpoint-clear (addr)
  (remhash addr dcpu:state-breakpoints))

(defun dcpu:breakpoint-clear-all ()
  (interactive)
  (clrhash dcpu:state-breakpoints))

;;;;;;;;;;

(defun dcpu:reg-get (idx)
  (cond
   ((= idx 0)
    dcpu:a)
   ((= idx 1)
    dcpu:b)
   ((= idx 2)
    dcpu:c)
   ((= idx 3)
    dcpu:x)
   ((= idx 4)
    dcpu:y)
   ((= idx 5)
    dcpu:z)
   ((= idx 6)
    dcpu:i)
   ((= idx 7)
    dcpu:j)
   ;;
   ((= idx 8)
    dcpu:ex)
   ((= idx 9)
    dcpu:pc)
   ((= idx 10)
    dcpu:sp)
   (t
    (error "dcpu:reg-get out of range"))))
;; (dotimes (i 8) (dcpu:reg-get i))

(defun dcpu:reg-set (idx val)
  (cond
   ((= idx 0)
    (setf dcpu:a val))
   ((= idx 1)
    (setf dcpu:b val))
   ((= idx 2)
    (setf dcpu:c val))
   ((= idx 3)
    (setf dcpu:x val))
   ((= idx 4)
    (setf dcpu:y val))
   ((= idx 5)
    (setf dcpu:z val))
   ((= idx 6)
    (setf dcpu:i val))
   ((= idx 7)
    (setf dcpu:j val))
   ;;
   ((= idx 8)
    (setf dcpu:ex val))
   ((= idx 9)
    (setf dcpu:pc val))
   ((= idx 10)
    (setf dcpu:sp val))
   (t
    (error "dcpu:reg-set"))))
;; (dcpu:reg-set 0 #xAAAA)
;; (progn (dotimes (i 8) (dcpu:reg-set i #xFFFF)) dcpu:j)

(defun dcpu:dst-set (val)
  (setq dcpu:dst-val val)
  (if dcpu:dst-reg
    (dcpu:reg-set dcpu:dst-reg val))
  (if dcpu:dst-addr
    (dcpu:mem-set dcpu:dst-addr val)))

;;;;;

(defun dcpu:instr-val-ra (rx)
  (setq
   dcpu:valA
   (cond
    ;; a,b,c,x,y,z,i,j
    ((<= rx #x07)
     (dcpu:reg-get rx))
    ;; [A]
    ((<= rx #x0F)
     (dcpu:mem-get (dcpu:reg-get (- rx #x08))))
    ;; [index+A]
    ((<= rx #x17)
     (incf dcpu:state-cycles)
     (setq dcpu:tmp-addr (+ (dcpu:reg-get (- rx #x10)) (dcpu:pc-next-word)))
     (dcpu:mem-get dcpu:tmp-addr))
    ;; POP
    ((= rx #x18)
     (dcpu:sp-pop))
    ;; PEEK / [SP]
    ((= rx  #x19)
     (dcpu:sp-peek))
    ;; PICK / [SP + i]
    ((= rx  #x1a)
     (incf dcpu:state-cycles)
     (setq dcpu:tmp-addr (dcpu:u16+ dcpu:sp (dcpu:pc-next-word)))
     (dcpu:mem-get dcpu:tmp-addr))
    ;; SP
    ((= rx #x1b)
     dcpu:sp)
    ;; PC
    ((= rx #x1c)
     dcpu:pc)
    ;; EX
    ((= rx #x1d)
     dcpu:ex)
    ;; [next word]
    ((= rx #x1e)
     (incf dcpu:state-cycles 1)
     (setq dcpu:tmp-addr (dcpu:pc-next-word))
     (dcpu:mem-get dcpu:tmp-addr))
    ;; LIT ; =>
    ((= rx #x1f)
     (incf dcpu:state-cycles 1)
     (dcpu:pc-next-word))
    ;; Immediate (-1 ... 30)
    ((<= rx #x3f)
     (- rx #x21))
    (t
     (error "dcpu:instr-val-ra")))))

(defun dcpu:instr-val-rb (rx)
  (setq
   dcpu:valB
   (cond
    ;; a,b,c,x,y,z,i,j
    ((<= rx #x07)
     (setq dcpu:dst-reg rx)
     (dcpu:reg-get rx))
    ;; [A]
    ((<= rx #x0F)
     (setq dcpu:dst-addr (dcpu:reg-get (- rx #x08)))
     (dcpu:mem-get dcpu:dst-addr))
    ;; [index+A]
    ((<= rx #x17)
     (incf dcpu:state-cycles)
     (setq dcpu:dst-addr (+ (dcpu:reg-get (- rx #x10)) (dcpu:pc-next-word)))
     (dcpu:mem-get dcpu:dst-addr))
    ;; PUSH
    ;; http://www.reddit.com/r/dcpu16/comments/suq7z/\
    ;; 16_1_5_spec_question_sp_and_push_as_a_bvalue/
    ((= rx #x18)
     (setq dcpu:sp (dcpu:u16+ dcpu:sp -1))
     (setq dcpu:dst-addr dcpu:sp)
     (dcpu:mem-get dcpu:dst-addr))
    ;; PEEK / [SP]
    ((= rx  #x19)
     (setq dcpu:dst-addr dcpu:sp)
     (dcpu:mem-get dcpu:dst-addr))
    ;; PICK / [SP + i]
    ((= rx  #x1a)
     (incf dcpu:state-cycles)
     (setq dcpu:dst-addr (dcpu:u16+ dcpu:sp (dcpu:pc-next-word)))
     (dcpu:mem-get dcpu:dst-addr))
    ;; SP
    ((= rx #x1b)
     (setq dcpu:dst-reg 10)
     dcpu:sp)
    ;; PC
    ((= rx #x1c)
     (setq dcpu:dst-reg 9)
     dcpu:pc)
    ;; EX
    ((= rx #x1d)
     (setq dcpu:dst-reg 8)
     dcpu:ex)
    ;; [next word]
    ((= rx #x1e)
     (incf dcpu:state-cycles 1)
     (setq dcpu:dst-addr (dcpu:pc-next-word))
     (dcpu:mem-get dcpu:dst-addr))
    ;; LIT ; =>
    ((= rx #x1f)
     (incf dcpu:state-cycles 1)
     (dcpu:pc-next-word))
    (t
     (error "dcpu:instr-val-rb")))))

;;;;;;;;;;

(defmacro dcpu:gen-run-cond ()
  (let* ((opc-lst (dcpu:opcodes-list))
         opc-spc-lst
         opc-basic-lst)
    ;; split into two lists.
    (mapc
     (lambda (x)
       (if (dcpu:opcode-specialcode x)
         (push x opc-spc-lst)
         (if (dcpu:opcode-opcode x)
           (push x opc-basic-lst))))
     opc-lst)
    ;;
    `(cond
      ((eql dcpu:cur-op 0)
       (setq dcpu:cur-op (dcpu:extract-rb dcpu:cur-instr))
       (cond
        ,@(mapcar
           (lambda (opc)
             `((eql dcpu:cur-op ,(dcpu:opcode-specialcode opc))
               (incf dcpu:state-cycles ,(dcpu:opcode-cycles opc))
               ,@(dcpu:opcode-body opc)))
           opc-spc-lst)
        (t
         (dcpu:break-set 'bad-special-instr)
         ;;(error "no special match")
         )))
      ,@(mapcar
         (lambda (opc)
           `((eql dcpu:cur-op ,(dcpu:opcode-opcode opc))
             (incf dcpu:state-cycles ,(dcpu:opcode-cycles opc))
             ,@(dcpu:opcode-body opc)))
         opc-basic-lst)
      (t
       (dcpu:break-set 'bad-basic-instr)
       ;;(error "no basic match")
       )
      )))
;; (pp1 (macroexpand '(dcpu:gen-run-cond)))

(defun dcpu:run-loop (&optional num-icount num-cycles)
  ;;
  (setq dcpu:run-until-icount
        (if (numberp num-icount)
          (+ dcpu:state-icount num-icount)
          nil))
  (setq dcpu:run-until-cycles
        (if (numberp num-cycles)
          (+ dcpu:state-cycles num-cycles)
          nil))

  ;; make a checkpoint at the start of a loop
  (when dcpu:auto-checkpoint-name
    (dcpu:checkpoint-save dcpu:auto-checkpoint-name))

  ;; clear our running breaks
  (dcpu:breaks-clear)

  ;;
  (run-hooks dcpu:run-start-hook)

  ;; run until a break is posted
  (while (not dcpu:state-breaks)
    (setq dcpu:dst-addr  nil
          dcpu:dst-reg   nil
          dcpu:dst-val   nil
          dcpu:valA      nil
          dcpu:valB      nil)
    ;;
    (unwind-protect
        (progn
          (setq dcpu:orig-pc   dcpu:pc)
          (setq dcpu:cur-pc    dcpu:pc)
          (setq dcpu:cur-instr (dcpu:pc-next-word))
          (setq dcpu:cur-op    (dcpu:extract-op dcpu:cur-instr))
          ;;
          (dcpu:instr-val-ra (dcpu:extract-ra dcpu:cur-instr))
          (if (/= 0 dcpu:cur-op)
            (dcpu:instr-val-rb (dcpu:extract-rb dcpu:cur-instr)))

          ;; handle skip
          (cond
           (dcpu:state-skip
            ;; keep skipping ifs
            (if (and (<= #x10 dcpu:cur-op) (<= dcpu:cur-op #x16))
              nil
              (setq dcpu:state-skip nil)))
           ;;
           (t
            (dcpu:gen-run-cond)))

          ;; post step update
          (incf dcpu:state-icount)

          ;; ran out?
          (if (and dcpu:run-until-cycles
                   (<= dcpu:run-until-cycles dcpu:state-cycles))
            (dcpu:break-set 'max-cycles))
          (if (and dcpu:run-until-icount
                   (<= dcpu:run-until-icount dcpu:state-icount))
            (dcpu:break-set 'max-icount))

          ;;
          (if dcpu:trace
            (dcpu:trace-msga
             "%-30s %s"
             (dcpu:disasm-addr dcpu:orig-pc)
             (if dcpu:dst-val
               (format "=#x%04x (%s)" dcpu:dst-val dcpu:dst-val)
               "")))

          ;;
          (if (gethash dcpu:pc dcpu:state-breakpoints)
            (dcpu:break-set 'break))

          ;; commit our pc
          ;; @todo better unwind
          (setq dcpu:orig-pc dcpu:pc)
          (dcpu:ui-update)
          nil)
      (progn ;; unwind form
        (setq dcpu:pc dcpu:orig-pc)))

    (when (and dcpu:run-sit-for (not dcpu:state-breaks))
      (sit-for dcpu:run-sit-for))
    ;; end of one step
    (run-hooks dcpu:run-step-hook)
    nil)
  ;; end of run-loop
  (run-hooks dcpu:run-stop-hook)
  nil)

;; (pp1 (macroexpand (symbol-function 'dcpu:run-loop)))
;; (progn (dcpu:init-cpu) (dcpu:load-from-file "test-basic1.dasm.hex"))
;; (dcpu:ui-update)
;; (dcpu:run-loop)

;; (eval-buffer)
(provide 'dcpu-cpu)
