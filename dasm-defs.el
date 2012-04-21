;;
;; ~/projects/games/0x10c/dcpu-el/dasm-defs.el ---
;;
;; $Id: dasm-defs.el,v 1.5 2012/04/21 21:54:31 harley Exp $
;;

(require 'elex)
(eval-when-compile
  (require 'cl))

;; global vars.
(defvar dasm:cur-pgm    nil)
(defvar dasm:cur-blk    nil)
(defvar dasm:cur-lbl    nil)
(defvar dasm:cur-tokens nil)

(defvar dasm:instrs
  '(set add sub mul div mod shl shr and bor xor
        ife ifn ifg ifb
        jsr
        break print ;; extras
        ))

(defvar dasm:instrs-regex
  (elex:keywords-to-regex dasm:instrs))

(defvar dasm:regs
  '(a b c i j x y z
      o pc sp
      pop peek push))
(defvar dasm:regs-regex
  (elex:keywords-to-regex dasm:regs))

(defvar dasm:number-regex
  "\\(0x[0-9A-Fa-f]+\\)\\|\\([0-9]+\\)")

(defvar dasm:token-bufname " *dasm tokens*")
(defvar dasm:asm-bufname   " *dasm asm*")

;;;;;;;;;;

(defstruct
  (dasm:pgm
   (:conc-name dasm:pgm-)
   (:constructor dasm:make-pgm))
  pathname
  blks
  lbls)
;; (setf dasm:cur-pgm (dasm:make-pgm))


(defstruct
  (dasm:blk
   (:conc-name dasm:blk-)
   (:constructor dasm:make-blk))
  parent
  instr-head
  instr-tail
  lbls)

(defstruct
  (dasm:instr
   (:conc-name dasm:instr-)
   (:constructor dasm:make-instr))
  op
  ra
  rb
  addr
  word0 word1 word2)


(defstruct
  (dasm:lbl
   (:conc-name dasm:lbl-)
   (:constructor dasm:make-lbl))
  name
  addr)

(provide 'dasm-defs)
