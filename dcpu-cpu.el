;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-cpu.el ---
;;
;; $Id: dcpu-cpu.el,v 1.13 2012/04/21 21:54:31 harley Exp $
;;

(require 'dcpu-util)
(require 'dcpu-defs)

(require 'cl) ;; find & position

(eval-when-compile
  (require 'cl))

;;;;;

;;;#autoload
(defun dcpu:cpu-init (cpu)
  (setf
   (dcpu:cpu-dev-vec cpu) (make-vector dcpu:dev-vec-size nil)
   (dcpu:cpu-mem-vec cpu) (make-vector dcpu:mem-vec-size 0)
   (dcpu:cpu-reg-vec cpu) (make-vector dcpu:reg-vec-size 0))
  (let ((dcpu:reg-vec (dcpu:cpu-reg-vec cpu)))
    (dcpu:set-reg 'skip nil)
    nil)
  cpu)

;;;#autoload
(defun dcpu:new-cpu ()
  (dcpu:cpu-init (dcpu:make-cpu)))
;; (dcpu:new-cpu)

(defun dcpu:activate-cpu (cpu)
  (setf
   dcpu:cur-cpu cpu
   dcpu:dev-vec (dcpu:cpu-dev-vec cpu)
   dcpu:mem-vec (dcpu:cpu-mem-vec cpu)
   dcpu:reg-vec (dcpu:cpu-reg-vec cpu))
  ;;cpu)
  nil)

;;;#autoload
(defun dcpu:ensure-active-cpu ()
  (when (not dcpu:cur-cpu)
    (dcpu:activate-cpu (dcpu:new-cpu))))
;; (dcpu:ensure-active-cpu)

(defun dcpu:reset ()
  "Reset all the registers to 0."
  (interactive)
  (fillarray dcpu:reg-vec 0))

;;;;;;;;;;

;;;#autoload
(defun dcpu:load-from-file (path)
  (when (not (file-readable-p path))
    (error "file is not readable"))
  (let ((buf (get-buffer-create "*dcpu load mem*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents path nil)
      (dcpu:load-from-buffer buf))))

(defun dcpu:load-from-buffer (buf)
  ;; @todo compile or assemble buffer?
  (dcpu:load-from-buffer-bin buf))

(defun dcpu:load-from-buffer-bin (buf)
  (dcpu:ensure-active-cpu)
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
            (dcpu:set-mem addr word)
            (setf addr (dcpu:u16+ 1 addr)))
           (t
            (throw 'break nil)))
          ;;(message "LB: %s" (match-string-no-properties 0))
          (goto-char (match-end 0)))))))
;; (dcpu:load-from-file "../programs/test-1.bin")

;;;;;;;;;;

(defun dcpu:screen-xy2addr (x y)
  (+ dcpu:screen-addr (* y dcpu:screen-x) x))
;; (dcpu:screen-xy2addr 0 0)
;; (dcpu:screen-xy2addr 0 1)

;;;;;;;;;;

(defun dcpu:get-mem (addr)
  (elt dcpu:mem-vec addr))
;; (dcpu:get-mem 0)

(defun dcpu:set-mem (addr val)
  ;;(assert dcpu:mem-vec)
  (setf (elt dcpu:mem-vec addr) val))

(defun dcpu:set-mem-bulk (addr &rest args)
  (let ((arg))
    (while args
      (setq arg (car args)
            args (cdr args))
      (cond
       ((numberp arg)
        (setf (elt dcpu:mem-vec addr) arg)
        (incf addr))
       ((stringp arg)
        (dotimes (i (length arg))
          (setf 
           (elt dcpu:mem-vec addr) (elt arg i)
           addr (1+ addr))))
       (t
        (error ""))))))

;; *sigh* diff order of args than "memset", but better sense.
(defun dcpu:memset (addr len val)
  (dotimes (i len)
    (setf (elt (elt dcpu:mem-vec addr) val))
    (setq addr (1+ addr))))

(defun dcpu:mem-clear ()
  (dcpu:memset 0 #xFFFF 0))

;; @todo macroize
(defun dcpu:get-reg (reg)
  (if (symbolp reg)
    (setf reg (position reg dcpu:reg-names)))
  (if (and (numberp reg) (<= 0 reg) (< reg dcpu:reg-vec-size))
    (elt dcpu:reg-vec reg)
    (error "bad reg")))
;; (dcpu:get-reg 'A)
;; (dcpu:get-reg 'PC)

;; @todo macrofiy for fixed args
(defun dcpu:set-reg (reg val)
  ;;(assert dcpu:reg-vec)
  (if (symbolp reg)
    (setf reg (position reg dcpu:reg-names)))
  (if (and (numberp reg) (<= 0 reg) (< reg dcpu:reg-vec-size))
    (setf (elt dcpu:reg-vec reg) val)
    (error "bad reg")))
;; (dcpu:set-reg 'A 4)

(defun dcpu:reg2idx (reg)
  (cond
   ((and (numberp reg) (< reg dcpu:reg-vec-size))
    reg)
   ((position reg dcpu:reg-names)
    (position reg dcpu:reg-names))
   (t
    (error ""))))
;; (dcpu:reg2idx 'PC)
;; (dcpu:reg2idx 100)

(defun dcpu:instr-len-arg (arg)
  (cond
   ;; reg & [reg]
   ((<= arg #x0f) 0)
   ;; [pc++ + reg]
   ((<= arg #x17) 1)
   ;; pop, peek, push, sp, pc, o
   ((<= arg #x1d) 0)
   ;; [word],word
   ((<= arg #x1f) 1)
   ;; lit
   ((<= arg #x3f) 0)
   (t
    (error ""))))
;; (dcpu:instr-len-arg 0)
;; (dcpu:instr-len-arg #x10)

(defun dcpu:instr-len (instr)
  (let ((op (dcpu:extract-op instr)))
    (+ 1
       (dcpu:instr-len-arg (dcpu:extract-rb instr))
       (if (< 0 op)
         (dcpu:instr-len-arg (dcpu:extract-ra instr))
         0))))
;; (dcpu:instr-len #xC00D)
;; (dcpu:instr-len #x7c01)
;; (dcpu:instr-len #x7de1)

;; expects dcpu:pc and dcpu:sp to be correct.
(defun dcpu:get-rx-val (rx)
  (cond
   ;; A
   ((<= rx #x07)
    (setf dcpu:dst-reg rx)
    (elt dcpu:reg-vec rx))
   ;; [A]
   ((<= rx #x0F)
    (incf dcpu:cycles)
    (setq dcpu:dst-addr (elt dcpu:reg-vec (- rx #x08)))
    (dcpu:get-mem dcpu:dst-addr))
   ;; [index+A]
   ((<= rx #x17)
    (incf dcpu:cycles)
    (setq dcpu:dst-addr (+ (dcpu:get-mem dcpu:pc) (elt dcpu:reg-vec (- rx #x10))))
    (setq dcpu:pc (dcpu:u16+ dcpu:pc 1))
    (dcpu:get-mem dcpu:dst-addr))
   ;; POP / [SP++]
   ((= rx #x18)
    (setq dcpu:dst-addr dcpu:sp)
    (setq dcpu:sp (dcpu:u16+ dcpu:sp 1))
    (incf dcpu:cycles)
    (dcpu:get-mem dcpu:dst-addr))
   ;; PEEK / [SP]
   ((= rx  #x19)
    (incf dcpu:cycles)
    (setq dcpu:dst-addr dcpu:sp)
    (dcpu:get-mem dcpu:sp))
   ;; PUSH / [--SP]
   ((= rx  #x1a)
    (incf dcpu:cycles)
    (setq dcpu:sp (dcpu:u16+ dcpu:sp -1))
    (setq dcpu:dst-addr dcpu:sp)
    (dcpu:get-mem dcpu:dst-addr))
   ;; SP
   ((= rx #x1b)
    (setq dcpu:dst-reg (dcpu:reg2idx 'SP))
    dcpu:sp)
   ;; PC
   ((= rx #x1c)
    (setq dcpu:dst-reg (dcpu:reg2idx 'PC))
    dcpu:pc)
   ;; O
   ((= rx #x1d)
    (setq dcpu:dst-reg (dcpu:reg2idx 'O))
    (dcpu:get-reg 'O))
   ;; [next word]
   ((= rx #x1e)
    (incf dcpu:cycles 2)
    (setq dcpu:dst-addr dcpu:pc)
    (setq dcpu:pc (dcpu:u16+ dcpu:pc 1))
    (setq dcpu:dst-addr (dcpu:get-mem dcpu:dst-addr))
    (dcpu:get-mem dcpu:dst-addr))
   ;; LIT ; =>
   ((= rx #x1f)
    (incf dcpu:cycles)
    (setq dcpu:addr dcpu:pc)
    (setq dcpu:pc (dcpu:u16+ dcpu:pc 1))
    (dcpu:get-mem dcpu:addr))
   ;; Immedate
   ((<= rx #x3f)
    (- rx #x20))
   (t
    (error ""))))

;;;;;

(defun dcpu:set-break (do-break &optional why)
  (when do-break
    (push (or why 'unknown) dcpu:break)))

(defun dcpu:clear-break ()
  (setf dcpu:break nil))

(defun dcpu:check-breakpoint-addr (addr why)
  (when (gethash addr dcpu:breakpoint-addr-table)
    (dcpu:set-break t why)))

(defun dcpu:set-breakpoint-addrs (&rest addrs)
  (dolist (addr addrs)
    (puthash addr t dcpu:breakpoint-addr-table)))

(defun dcpu:clear-breakpoint-addrs (&rest addrs)
  (dolist (addr addrs)
    (remhash addr dcpu:breakpoint-addr-table)))

(defun dcpu:clear-all-breakpoint-addr ()
  (clrhash dcpu:breakpoint-addr-table))

;;;;;

(defun dcpu:run-loop (for-icnt &optional for-cycles)
  ;;(assert dcpu:cur-cpu t "dcpu:cur-cpu needs to be set.")
  (let ((dcpu:cycles (dcpu:get-reg 'cycles))
        (cycles-max  (dcpu:get-reg 'cycles-max))
        (dcpu:icnt   (dcpu:get-reg 'icnt))
        (icnt-max    (dcpu:get-reg 'icnt-max))
        (dcpu:skip   (dcpu:get-reg 'skip))
        (dcpu:pc     (dcpu:get-reg 'PC))
        (dcpu:sp     (dcpu:get-reg 'SP))
        (dcpu:dst-addr nil)
        (dcpu:dst-reg  nil)
        last-dcpu:pc
        addr instr op ra rb
        val val-a val-b val-o
        )
    ;;
    (dcpu:clear-break)
    ;; max cycles?
    (cond
     ((numberp for-cycles)
      (setq cycles-max (+ (or dcpu:cycles 0) for-cycles))
      ;;(dcpu:set-break (< cycles-max dcpu:cycles) 'cycles-max)
      )
     ((null for-cycles)
      nil)
     ;;
     (t
      (setq cycles-max t)))
    (dcpu:set-reg 'cycles-max cycles-max)
    ;; max icnt?
    (cond
     ((numberp for-icnt)
      (setq icnt-max (+ (or dcpu:icnt 0) for-icnt)))
     ((null for-icnt)
       t)
     (t
      (setq icnt-max t)))
    (dcpu:set-reg 'icnt-max icnt-max)

    ;;
    (while (not dcpu:break)
      (setf dcpu:dst-addr nil
            dcpu:dst-reg  nil
            last-dcpu:pc  dcpu:pc
            instr   (dcpu:get-mem dcpu:pc)
            op      (dcpu:extract-op instr)
            ra      (dcpu:extract-ra instr)
            rb      (dcpu:extract-rb instr)
            val     nil
            val-o   nil)
      ;; trace what we are about to do
      (when dcpu:feature-trace
        (dcpu:trace-instr))
      ;;
      (setf dcpu:pc (dcpu:u16+ dcpu:pc 1))
      (incf dcpu:cycles)
      (incf dcpu:icnt)
      ;;
      (cond
       ;; suppressed instr?
       (dcpu:skip
        ;; take off one, as we
        (incf dcpu:pc (1- (dcpu:instr-len instr)))
        (setq dcpu:skip nil))
       ;; non-basic?
       ((= 0 op)
        (setf op ra
              ra (dcpu:extract-rb instr))
        ;;
        (case op
          ;; reserved
          (#x0
           (error ""))
          ;; JSR
          (#x1
           (setf dcpu:sp (dcpu:u16+ dcpu:sp -1))
           (dcpu:set-mem dcpu:sp dcpu:pc)
           (setq val-a (dcpu:get-rx-val ra))
           (setf dcpu:pc val-a))
          ;; GETC ;; 000000 000010 0000 => 0x0020
          (#x2
           ;;(dcpu:display-screen)
           (dcpu:ui-update)
           (let ((c (read-char "DCPU read-char:" nil dcpu:read-char-delay)))
             (if (not c)
               (setq c 0))
             (setq val c
                   dcpu:dst-reg ra)))
          )
        nil)
       ;; basic...
       (t
        ;; get the values
        (setq val-a (dcpu:get-rx-val ra))
        (dcpu:check-breakpoint-addr dcpu:addr 'mem-a)
        ;; discard the dest
        (let ((dcpu:dst-addr nil)
              (dcpu:dst-reg  nil))
          (setf val-b (dcpu:get-rx-val rb))
          (dcpu:check-breakpoint-addr dcpu:addr 'mem-b))
        (case op
          (#x0
           (error "shouldnt get here"))
          ;; SET a, b
          (#x1
           (setq val val-b))
          ;; ADD a, b
          (#x2
           (incf dcpu:cycles 1) ; extra time
           (setq val (+ val-a val-b)
                 val-o (if (< #xFFFF val) 0 #x0001)))
          ;; SUB a, b
          (#x3
           (incf dcpu:cycles 1) ; extra time
           (setq val (- val-a val-b)
                 val-o (if (< val 0) #xFFFF 0)))
          ;; MUL a, b - sets a to a*b, sets O to ((a*b)>>16)&#xffff
          (#x4
           (incf dcpu:cycles 1) ; extra time
           (setq val (* val-a val-b)
                 val-o (dcpu:u16 (lsh val -16))))
          ;; DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&#xffff.
          ;;if b==0, sets a and O to 0 instead.
          (#x5
           (incf dcpu:cycles 2) ; extra time
           (if (= 0 val-b)
             (progn
               (setq val 0)
               val-o 0)
             (progn
               (setq val (/ val-a val-b)
                     val-o (/ (lsh val-a 16) val-b)))))
          ;; MOD a, b - sets a to a%b. if b==0, sets a to 0 instead.
          (#x6
           (incf dcpu:cycles 2) ; extra time
           (setq val (if (= 0 val-b) 0 (mod val-a val-b))))
          ;; SHL a, b - sets a to a<<b, sets O to ((a<<b)>>16)&#xffff
          (#x7
           (incf dcpu:cycles 1) ; extra time
           (setq val (lsh val-a val-b)
                 val-o (lsh val -16)))
          ;; SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&#xffff
          (#x8
           (incf dcpu:cycles 1) ; extra time
           (setq val-b (- val-b)
                 val (lsh val-a val-b)
                 val-o (lsh (lsh val-a 16) val-b)))
          ;; AND a, b
          (#x9
           (setq val (logand val-a val-b)))
          ;; BOR a, b
          (#xa
           (setq val (logior val-a val-b)))
          ;; XOR a, b
          (#xb
           (setq val (logxor val-a val-b)))
          ;; IFE a, b - performs next instruction only if a==b
          (#xc
           (incf dcpu:cycles 1) ; extra time
           (when (/= val-a val-b)
             (incf dcpu:cycles 1) ; extra time
             (setq dcpu:skip t)))
          ;; IFN a, b - performs next instruction only if a!=b
          (#xD
           (incf dcpu:cycles 1) ; extra time
           (when (= val-a val-b)
             (incf dcpu:cycles 1) ; extra time
             (setq dcpu:skip t)))
          ;; IFG a, b - performs next instruction only if a>b
          (#xE
           (incf dcpu:cycles 1) ; extra time
           (when (<= val-a val-b)
             (incf dcpu:cycles 1) ; extra time
             (setq dcpu:skip t)))
          ;; IFB a, b - performs next instruction only if (a&b)!=0
          (#xF
           (incf dcpu:cycles 1) ; extra time
           (when (= 0 (logand val-a val-b))
             (incf dcpu:cycles 1) ; extra time
             (setq dcpu:skip t)))
          )))
      ;; commit reg state
      (dcpu:set-reg 'cycles dcpu:cycles)
      (dcpu:set-reg 'icnt   dcpu:icnt)
      (dcpu:set-reg 'PC     dcpu:pc)
      (dcpu:set-reg 'SP     dcpu:sp)
      (dcpu:set-reg 'skip   dcpu:skip)
      (when val-o
        (dcpu:set-reg 'O (dcpu:u16 val-o)))
      (when val
        (when dcpu:dst-addr
          (dcpu:set-mem dcpu:dst-addr (dcpu:u16 val)))
        (when dcpu:dst-reg
          (dcpu:set-reg dcpu:dst-reg (dcpu:u16 val))
          ;; the above might replace our values
          (setf dcpu:pc (dcpu:get-reg 'PC)
                dcpu:sp (dcpu:get-reg 'SP))))

      ;; keep running?
      (if (numberp cycles-max)
        (dcpu:set-break (<= cycles-max dcpu:cycles) 'cycles-max))
      (if (numberp icnt-max)
        (dcpu:set-break (<= icnt-max dcpu:icnt) 'icnt-max))
      (dcpu:set-break (= dcpu:pc last-dcpu:pc) 'pc-loop)

      ;; hit a break?
      (dcpu:check-breakpoint-addr dcpu:pc 'pc-break)

      ;; post-step updates
      (when (and dcpu:sit-for (not dcpu:break))
        (dcpu:ui-update)
        (sit-for dcpu:sit-for))

      (run-hooks 'dcpu:post-step-hooks))
    ;; post-run update
    (dcpu:ui-update)
    (run-hooks 'dcpu:post-run-hooks)
    nil))

(defun dcpu:run-1 ()
  (interactive)
  (dcpu:run-loop 1 nil))

;; (eval-buffer)
;; (dcpu-test-1)
;; (progn (dcpu:standard-ui) (dcpu:ui-update))
;; (dcpu:run 10)
;; (dcpu:set-reg 'PC #x0a)

(provide 'dcpu-cpu)
