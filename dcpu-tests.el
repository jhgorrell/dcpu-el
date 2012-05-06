;;
;; ~/0x10c/dcpu-el/dcpu-tests.el ---
;;
;; $Id: dcpu-tests.el,v 1.14 2012/05/04 12:53:28 harley Exp $
;;

;; (progn (setq jhg-cload-enabled nil) (add-to-list 'load-path "."))

(eval-when-compile
  (require 'cl))

(require 'dcpu)

;;(require 'ert)

(defun dcpu:test-u16 ()
  ;;
  (assert (=      1 (dcpu:u16 #x0001)))
  (assert (= #xFFFF (dcpu:u16 #x1FFFF)))
  (assert (= #x0000 (dcpu:u16 #x10000)))
  ;;
  (assert (=      1 (dcpu:u16+      0 1)))
  (assert (=      0 (dcpu:u16+ #xFFFF 1)))
  ;;
  (assert (=      0 (dcpu:sext #x0000)))
  (assert (= #x7FFF (dcpu:sext #x7FFF)))
  (assert (=     -1 (dcpu:sext #xFFFF)))
  (assert (= -32768 (dcpu:sext #x8000)))
  ;;
  (assert (=     -2 (dcpu:s16+ #xFFFF #xFFFF)))
  nil)

(defun dcpu:test-extract ()
  ;;
  (assert (=    0 (dcpu:extract-op 0)))
  (assert (= #x1f (dcpu:extract-op #xFFFF)))
  (assert (=    0 (dcpu:extract-op #xFFE0)))
  ;;
  (assert (=    0 (dcpu:extract-ra 0)))
  (assert (= #x3F (dcpu:extract-ra #xFFFF)))
  (assert (= #x3F (dcpu:extract-ra #xFC00)))
  (assert (=    0 (dcpu:extract-ra #x103FF))) ;; hibit for test
  ;;
  (assert (=    0 (dcpu:extract-rb #x0000)))
  (assert (=    1 (dcpu:extract-rb #x003F)))
  (assert (= #x10 (dcpu:extract-rb #xFE00)))
  (assert (=    0 (dcpu:extract-rb #xFC00)))
  (assert (= #x1F (dcpu:extract-rb #xFFFF)))
  (assert (=    0 (dcpu:extract-rb #xFC1F)))
  nil)

(defun dcpu:test-instr-val-ra ()
  ;;
  (dcpu:init-cpu)
  (setf dcpu:a #x0123)
  (setf dcpu:j #x4567)
  (dcpu:mem-set dcpu:a #x5555)
  ;;
  (assert (= #x0123 (dcpu:instr-val-ra #x00)))
  (assert (= #x4567 (dcpu:instr-val-ra #x07)))
  ;;
  (assert (= #x5555 (dcpu:instr-val-ra #x08)))
  ;; pop
  (dcpu:sp-push #x6666)
  (assert (= #x6666 (dcpu:instr-val-ra #x18)))
  ;;
  (dcpu:sp-push #x7777)
  (assert (= #x7777 (dcpu:instr-val-ra #x19)))
  ;; lit
  (assert (= -1 (dcpu:instr-val-ra #x20)))
  (assert (= 30 (dcpu:instr-val-ra #x3F)))
  ;;
  nil)

(defun dcpu:test-small-1 ()
  (interactive)
  (dcpu:init-cpu)
  (dcpu:asm-to-mem
   0
   '(
     (= :x 10)
     :start
     (set a  1)
     (set b  2)
     (set c  3)
     (set x :x)
     (set y 12)
     (set z 13)
     (set a  5)
     :loop
     (sub a  1)
     (ifg a  0)
     (set pc :loop)
     (add a  5)
     (set pc :loop)
     (set pc :ffff)
     (= :ffff #xFFFF)
     (hcf)
     ))
  ;;
  (dcpu:display-clear-aregs)
  (dcpu:display-add-areg (dcpu:make-aregion :s 0 :l 16 :d 'words))
  (dcpu:display-add-areg (dcpu:make-aregion :s 0 :l 16 :d 'disassemble))
  ;;
  (dcpu:ui-enter)
  nil)
;; (dcpu:test-small-1)
;; (dcpu:asm-labels-dump)

(defun dcpu:test-hw-str (lbl str)
  (let ((lbl_len (intern (concat (symbol-name lbl) "_len"))))
    (list
     (list '= lbl_len (length str))
     lbl
     (append (list 'word) (mapcar 'identity str)))))
;; (dcpu:test-hw-str :text "hello world!")

(defun dcpu:test-hw-1 ()
  (interactive)
  (dcpu:init-cpu)
  (dcpu:asm-to-mem
   0
   `(
     :start
     (= :screen #x8000)
     (set i :screen)
     (set j :text)
     (set a :text_len)
     :loop
     (sti (i) (j))
     (sub a 1)
     (ifg a 0)
     (set pc :loop)
     (hcf)
     (word #x0000 #xFFFF)
     :end_code
     ;;
     ,@(dcpu:test-hw-str :text "hello world")
     :end
     ))
  ;;
  (dcpu:display-clear-aregs)
  (dcpu:display-add-areg
   (dcpu:make-aregion
    :s (dcpu:asm-label-addr :start)
    :l (dcpu:asm-label-addr :end_code)
    :d 'disassemble))
  (dcpu:display-add-areg
   (dcpu:make-aregion
    ;;:s (dcpu:asm-label-addr :end)
    :s (dcpu:asm-label-addr :start)
    :e (dcpu:asm-label-addr :end)
    :d 'words))
  ;;
  (dcpu:ui-enter))
;; (progn (eval-buffer) (dcpu:test-hw-1) (dcpu:ui-update))
;; (dcpu:asm-labels-dump)
