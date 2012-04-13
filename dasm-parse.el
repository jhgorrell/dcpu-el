;;
;; ~/projects/games/0x10c/dcpu-el/dasm-parse.el ---
;;
;; $Id: dasm-parse.el,v 1.10 2012/04/12 22:28:21 harley Exp $
;;
;;

;; @todo:
;; * put into subdir
;; * break into "elex"

;; (progn (add-to-list 'load-path ".") (eval-buffer))
(require 'elex)
(require 'dasm-defs)
(eval-when-compile (require 'cl))

;;;;;

(defun dasm:assemble-buffer-to-mem (&optional buf)
  (or buf (setq buf (current-buffer)))
  (dcpu:ensure-active-cpu)
  ;;
  nil)

;;;;;

(defun dasm:pgm-to-buf (&optional pgm)
  (setf pgm (or pgm dasm:cur-pgm))
  (with-current-buffer (get-buffer-create dasm:asm-bufname)
    (erase-buffer)
    (insert ";;;; PGM\n")
    (insert
     "; path: " (format "%s" (dasm:pgm-pathname pgm)) "\n"
     )
    (mapc 'dasm:to-buf (dasm:pgm-blks pgm))
    nil))
;; (dasm:pgm-to-buf)

;;;;;

(defun dasm:blk-to-buf (blk)
  (insert
   "\n"
   ";;;; BLK\n")
  (mapc
   (lambda (lbl)
     (insert
      (format
       ";; %s = %s\n"
       (dasm:lbl-name lbl)
       (dasm:lbl-addr lbl))))
   (dasm:blk-lbls blk))
  ;;
  (mapc 'dasm:to-buf (dasm:blk-instr-head blk)))

(defun dasm:blk-add-instr (blk ins)
  (let ((lst (cons ins nil)))
    (cond
     ((consp (dasm:blk-instr-tail blk))
      (setcdr (dasm:blk-instr-tail blk) lst)
      (setf  (dasm:blk-instr-tail blk) lst))
     (t
      (setf (dasm:blk-instr-head blk) lst
            (dasm:blk-instr-tail blk) lst)))
    blk))

(defun dasm:blk-str (blk)
  (format "#<dasm:blk >"))

(defun dasm:blk-add-lbl (blk lbl)
  (push lbl (dasm:blk-lbls blk))
  (dasm:blk-add-instr blk lbl))

;; (let ((blk (make-dasm:blk))) (dotimes (i 3) (dasm:blk-push-instr blk i)) blk)

(defun dasm:instr-to-buf (instr)
  (insert
   "  "
   (format "%-10s" (dasm:instr-op instr))
   "  "
   (format "%s" (dasm:instr-ra instr))
   ","
   (format "%s" (dasm:instr-rb instr))
   "\n"))

(defun dasm:lbl-to-buf (lbl)
  (insert
   ":"
   (format "%s" (dasm:lbl-name lbl))
   " ; =>"
   (format "%s" (dasm:lbl-addr lbl))
   "\n"))

(defun dasm:lbl-str (lbl)
  (format
   "#<dasm:label %s : %s>"
   (dasm:lbl-name lbl)
   (dasm:lbl-addr lbl)))
;; (dasm:lbl-str (dasm:make-lbl))

;;;;;;;;;; Generics

(defun dasm:lbls (obj)
  (cond
   ((dasm:blk-p obj)
    (append (dasm:blk-lbls obj) (dasm:lbls (dasm:blk-parent obj))))
   ((dasm:pgm-p obj)
    (dasm:blk-lbls obj))
   (t
    nil)))
;; (dasm:lbls dasm:cur-blk)

(defun dasm:find-lbl (obj name)
  ;; @todo
  (find name (dasm:lbls obj)
        :key 'dasm:lbl-name
        :test 'equalp))

(defun dasm:to-buf (obj)
  (cond
   ((dasm:pgm-p obj)
    (dasm:pgm-to-buf obj))
   ((dasm:blk-p obj)
    (dasm:blk-to-buf obj))
   ((dasm:instr-p obj)
    (dasm:instr-to-buf obj))
   ((dasm:lbl-p obj)
    (dasm:lbl-to-buf obj))
   (t
    (error ""))))

;; (dasm:to-buf dasm:cur-pgm)

;;;;;

(defun dasm:next-token ()
  (elex:next-token
   ('nl            "\n")
   ('comment       "[#;].*")
   ('sp            "\\s-+")
   ('comma         ",")
   ('bracket-open  "\\[")
   ('bracket-close "\\]")
   ('paren-open    "(")
   ('paren-close   ")")
   ('+             "+")
   ('-             "-")
   ('*             "*")
   ('/             "/")
   ;;
   ('addr          "[0-9A-Fa-f]\\{4\\}:\\b")
   ;;
   ('number        dasm:number-regex)
   ('instr         dasm:instrs-regex)
   ('instr-reg     dasm:regs-regex)
   ;; This isnt grabbing the ":"...
   ;; should be (":crash" ":" "crash")
   ;; or ("crash" "" "crash") not ("crash")"
   ;; fixed (dont use "?")
   ;; normal asm (like LLVM generates) is "label:",
   ;; support both ways.
   ('label "\\(:\\|\\)\\([-a-zA-Z0-9_]+\\)\\(:\\|\\)\\b")
   ;;
   ('eof "\\'" (setq rv nil))
   (t t (setq rv nil))
   ))

(defun dasm:parse (obj)
  (if (stringp obj)
    (setq buf (find-file obj)))
  (setq dasm:cur-tokens (elex:buffer-to-tokens 'dasm:next-token buf))
  (setq dasm:cur-tokens (elex:remove-tokens dasm:cur-tokens '(comment sp comma)))
  (elex:print-tokens dasm:cur-tokens dasm:token-bufname)
  (dasm:parse-tokens dasm:cur-tokens)
  nil)

(defun dasm:parse-tokens (toks)
  (let* ((pgm (dasm:make-pgm))
         (blk (dasm:make-blk))
         instr
         tok
         (addr 0))
    (setf dasm:cur-pgm pgm)
    (setf dasm:cur-blk blk)
    (push blk (dasm:pgm-blks pgm))
    ;;
    (while toks
      (setq tok (car toks)
            toks (cdr toks))
      (case (elex:token-id tok)
        ((nl)
         nil)
        ;; :foo ->
        ((label)
         (let* ((name (elex:token-string tok 2))
                (lbl (dasm:find-lbl blk name)))
           (when (not lbl)
             (dasm:blk-add-lbl
              blk
              (dasm:make-lbl
               :name name
               :addr addr)))))
        ;;
        ((instr)
         (let ((instr (dasm:make-instr
                       :op (elex:token-string tok 0)
                       :addr addr)))
           (dasm:blk-add-instr blk instr)
           (incf addr)
           ;;(setf (dasm:instr-op instr)
           (message "instr: %s" (car (elex:token-strings tok)))
           ;;
           (when (equal (elex:token-id (car toks)) 'instr-reg)
             (setq tok (car toks)
                   toks (cdr toks))
             (setf (dasm:instr-ra instr) (elex:token-string tok 0)))
           (when (equal (elex:token-id (car toks)) 'instr-reg)
             (setq tok (car toks)
                   toks (cdr toks))
             (setf (dasm:instr-rb instr) (elex:token-string tok 0)))
           nil))
        (t
         nil)))
    (switch-to-buffer-other-window (get-buffer-create dasm:asm-bufname))
    pgm))

;; (progn (indent-buffer) (eval-buffer))
;; (dasm:parse "../programs/test-1.dasm")
;; (dasm:pgm-to-buf (dasm:parse-tokens dasm:cur-tokens))

(provide 'dasm-parse)
