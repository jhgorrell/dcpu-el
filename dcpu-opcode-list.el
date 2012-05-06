;;
;; ~/0x10c/dcpu-el/dcpu-opcode-list.el ---
;;
;; $Id: dcpu-opcode-list.el,v 1.4 2012/05/06 08:31:18 harley Exp $
;;

;; load this before editing defopcode forms.
;; (require 'dcpu-elisp)

(require 'dcpu-opcode-func)

;; order:
;;   fetch A
;;   fetch B
;;   calc result
;;   update EX,I,J
;;   write result

;; 3 | 0x01 | JSR a | pushes the address of the next instruction to the stack,
;;   |      |       | then sets PC to a
(dcpu:defopcode jsr
  :special #x01
  :cycles  3
  (dcpu:sp-push dcpu:pc)
  (setq dcpu:pc dcpu:valA))

;; 9 | 0x07 | HCF a | use sparingly
(dcpu:defopcode hcf
  :special #x07
  :cycles  9
  (setq dcpu:state-onfire t)
  (dcpu:break-set 'onfire))

;;  4 | 0x08 | INT a | triggers a software interrupt with message a
(dcpu:defopcode int
  :special #x08
  :cycles  4
  (when (/= 0 dcpu:ia)
    (setq dcpu:state-iqueue t)
    (dcpu:sp-push dcpu:pc)
    (dcpu:sp-push dcpu:a)
    (setq dcpu:pc dcpu:ia)))

;; 1 | 0x09 | IAG a | sets a to IA
(dcpu:defopcode iag
  :special #x09
  :cycles  1
  (setq dcpu:a dcpu:ia))

;;  1 | 0x0a | IAS a | sets IA to a
(dcpu:defopcode ias
  :special #x0a
  :cycles  1
  (setq dcpu:ia dcpu:a))

;;  3 | 0x0b | RFI a | disables interrupt queueing, pops A from the stack, then
;;    |      |       | pops PC from the stack
(dcpu:defopcode rfi
  :special #x0b
  :cycles  3
  (setq dcpu:state-iqueue nil)
  (setq dcpu:a (dcpu:sp-pop))
  (setq dcpu:pc (dcpu:sp-pop)))

;;  2 | 0x0c | IAQ a | if a is nonzero, interrupts will be added to the queue
;;    |      |       | instead of triggered. if a is zero, interrupts will be
;;    |      |       | triggered as normal again
(dcpu:defopcode iaq
  :special #x0c
  :cycles  2
  (if (= 0 dcpu:a)
    (setq dcpu:state-iqueue nil)
    (setq dcpu:state-iqueue t)))

;;  2 | 0x10 | HWN a | sets a to number of connected hardware devices
(dcpu:defopcode hwn
  :special #x10
  :cycles  2
  ;; @todo fix this
  (setq dcpu:a 0))

;;  4 | 0x11 | HWQ a | sets A, B, C, X, Y registers to information about hardware a
;;    |      |       | A+(B<<16) is a 32 bit word identifying the hardware id
;;    |      |       | C is the hardware version
;;    |      |       | X+(Y<<16) is a 32 bit word identifying the manufacturer
(dcpu:defopcode hwq
  :special #x11
  :cycles  2
  ;; @todo fix this
  (setq dcpu:a 0
        dcpu:b 0
        dcpu:c 0
        dcpu:x 0
        dcpu:y 0)
  ;; @todo now find the HW device and call it
  nil)

;;  4+| 0x12 | HWI a | sends an interrupt to hardware a
(dcpu:defopcode hwi
  :special #x12
  :cycles  4
  ;; the hw device should call
  ;;(incf dcpu:state-cycles NNN)
  nil)

;;;;;

;;  1 | 0x01 | SET b, a | sets b to a
(dcpu:defopcode set
  :opcode #x01
  :cycles 1
  (dcpu:dst-set dcpu:valA))

;;  2 | 0x02 | ADD b, a | sets b to b+a, sets EX to 0x0001 if there's an overflow,
;;    |      |          | 0x0 otherwise
(dcpu:defopcode add
  :opcode #x02
  :cycles 2
  (setq dcpu:tmp-val (+ dcpu:valB dcpu:valA))
  (setq dcpu:ex (if (< #xFFFF dcpu:tmp-val) 1 0))
  (dcpu:dst-set dcpu:tmp-val))

;;  2 | 0x03 | SUB b, a | sets b to b-a, sets EX to 0xffff if there's an underflow,
;;    |      |          | 0x0 otherwise
(dcpu:defopcode sub
  :opcode #x03
  :cycles 2
  (setq dcpu:tmp-val (- dcpu:valB dcpu:valA))
  (setq dcpu:ex (if (< dcpu:tmp-val 0) #xffff 0))
  (dcpu:dst-set dcpu:tmp-val))

;;  2 | 0x04 | MUL b, a | sets b to b*a, sets EX to ((b*a)>>16)&0xffff (treats b,
;;    |      |          | a as unsigned)
(dcpu:defopcode mul
  :opcode #x04
  :cycles 2
  (setq dcpu:tmp-val (* dcpu:valB dcpu:valA))
  (setq dcpu:ex (dcpu:u16 (ash dcpu:tmp-val 16)))
  (dcpu:dst-set dcpu:tmp-val))

;;  2 | 0x05 | MLI b, a | like MUL, but treat b, a as signed
(dcpu:defopcode mli
  :opcode #x05
  :cycles 2
  (setq dcpu:tmp-val (* (dcpu:sext dcpu:valB) (dcpu:sext dcpu:valA)))
  (setq dcpu:ex (dcpu:u16 (ash dcpu:tmp-val 16)))
  (dcpu:dst-set dcpu:tmp-val))

;;  3 | 0x06 | DIV b, a | sets b to b/a, sets EX to ((b<<16)/a)&0xffff. if a==0,
;;    |      |          | sets b and EX to 0 instead. (treats b, a as unsigned)
(dcpu:defopcode div
  :opcode #x06
  :cycles 3
  (cond
   ((= 0 dcpu:valA)
    (setq dcpu:tmp-val 0
          dcpu:ex 0))
   (t
    (setq dcpu:tmp-val (/ dcpu:valB dcpu:valA))
    (setq dcpu:ex (dcpu:u16 (ash dcpu:tmp-val 16)))))
  (dcpu:dst-set dcpu:tmp-val))

;;  3 | 0x07 | DVI b, a | like DIV, but treat b, a as signed. Rounds towards 0
(dcpu:defopcode dvi
  :opcode #x07
  :cycles 3
  (cond
   ((= 0 dcpu:valA)
    (setq dcpu:tmp-val 0
          dcpu:ex 0))
   (t
    (setq dcpu:tmp-val (/ (dcpu:sext dcpu:valB) (dcpu:sext dcpu:valA)))
    (setq dcpu:ex (dcpu:u16 (ash dcpu:tmp-val 16)))))
  (dcpu:dst-set dcpu:tmp-val))

;;  3 | 0x08 | MOD b, a | sets b to b%a. if a==0, sets b to 0 instead.
(dcpu:defopcode mod
  :opcode #x08
  :cycles 3
  (cond
   ((= 0 dcpu:valA)
    (setq dcpu:tmp-val 0))
   (t
    (setq dcpu:tmp-val (mod dcpu:valB dcpu:valA))))
  (dcpu:dst-set dcpu:tmp-val))

;;  3 | 0x09 | MDI b, a | like MOD, but treat b, a as signed. (MDI -7, 16 == -7)
(dcpu:defopcode mdi
  :opcode #x09
  :cycles 3
  (cond
   ((= 0 dcpu:valA)
    (setq dcpu:tmp-val 0))
   (t
    (setq dcpu:tmp-val (mod (dcpu:sext dcpu:valB) (dcpu:sext dcpu:valA)))))
  (dcpu:dst-set dcpu:tmp-val))

;;  1 | 0x0a | AND b, a | sets b to b&a
(dcpu:defopcode and
  :opcode #x0a
  :cycles 1
  (dcpu:dst-set (logand dcpu:valB dcpu:valA)))

;;  1 | 0x0b | BOR b, a | sets b to b|a
(dcpu:defopcode bor
  :opcode #x0b
  :cycles 1
  (dcpu:dst-set (logior dcpu:valB dcpu:valA)))

;;  1 | 0x0c | XOR b, a | sets b to b^a
(dcpu:defopcode xor
  :opcode #x0c
  :cycles 1
  (dcpu:dst-set (logxor dcpu:valB dcpu:valA)))

;;  1 | 0x0d | SHR b, a | sets b to b>>>a, sets EX to ((b<<16)>>a)&0xffff
;;    |      |          | (logical shift)
(dcpu:defopcode shr
  :opcode #x0d
  :cycles 1
  (setq dcpu:tmp-val (lsh dcpu:valB (- dcpu:valA)))
  (setq dcpu:ex (dcpu:u16 (lsh (lsh dcpu:valB 16) (- dcpu:valA))))
  (dcpu:dst-set dcpu:tmp-val))

;;  1 | 0x0e | ASR b, a | sets b to b>>a, sets EX to ((b<<16)>>>a)&0xffff
;;    |      |          | (arithmetic shift) (treats b as signed)
(dcpu:defopcode asr
  :opcode #x0e
  :cycles 1
  (setq dcpu:valB (dcpu:sext dcpu:valB))
  (setq dcpu:tmp-val (lsh dcpu:valB (- dcpu:valA)))
  (setq dcpu:ex (dcpu:u16 (lsh (lsh dcpu:valB 16) (- dcpu:valA))))
  (dcpu:dst-set dcpu:tmp-val))

;;  1 | 0x0f | SHL b, a | sets b to b<<a, sets EX to ((b<<a)>>16)&0xffff
;;
(dcpu:defopcode shl
  :opcode #x0f
  :cycles 1
  (setq dcpu:tmp-val (lsh dcpu:valB dcpu:valA))
  (setq dcpu:ex (dcpu:u16 (lsh dcpu:tmp-val -16)))
  (dcpu:dst-set dcpu:tmp-val))

;;  2+| 0x10 | IFB b, a | performs next instruction only if (b&a)!=0
(dcpu:defopcode ifb
  :opcode #x10
  :cycles 2
  (if (= 0 (logand dcpu:valB dcpu:valA))
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x11 | IFC b, a | performs next instruction only if (b&a)==0
(dcpu:defopcode ifc
  :opcode #x11
  :cycles 2
  (if (/= 0 (logand dcpu:valB dcpu:valA))
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x12 | IFE b, a | performs next instruction only if b==a
(dcpu:defopcode ife
  :opcode #x12
  :cycles 2
  (if (/= dcpu:valB dcpu:valA)
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x13 | IFN b, a | performs next instruction only if b!=a
(dcpu:defopcode ifn
  :opcode #x13
  :cycles 2
  (if (= 0 (logand dcpu:valB dcpu:valA))
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x14 | IFG b, a | performs next instruction only if b>a
(dcpu:defopcode ifg
  :opcode #x14
  :cycles 2
  (if (<= dcpu:valB dcpu:valA)
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x15 | IFA b, a | performs next instruction only if b>a (signed)
(dcpu:defopcode ifa
  :opcode #x15
  :cycles 2
  (if (<= (dcpu:sext dcpu:valB) (dcpu:sext dcpu:valA))
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x16 | IFL b, a | performs next instruction only if b<a
(dcpu:defopcode ifl
  :opcode #x16
  :cycles 2
  (if (<= dcpu:valA dcpu:valB)
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  2+| 0x17 | IFU b, a | performs next instruction only if b<a (signed)
(dcpu:defopcode ifu
  :opcode #x17
  :cycles 2
  (if (<= (dcpu:sext dcpu:valA) (dcpu:sext dcpu:valB))
    (setq dcpu:state-skip t)
    (incf dcpu:state-cycles)))

;;  3 | 0x1a | ADX b, a | sets b to b+a+EX, sets EX to 0x0001 if there is an over-
;;    |      |          | flow, 0x0 otherwise
(dcpu:defopcode adx
  :opcode #x1a
  :cycles 3
  (setq dcpu:tmp-val (+ dcpu:valB dcpu:valA dcpu:ex))
  (setq dcpu:ex (if (<= #xFFFF dcpu:tmp-val) 0 1))
  (dcpu:dst-set dcpu:tmp-val))

;;  3 | 0x1b | SBX b, a | sets b to b-a+EX, sets EX to 0xFFFF if there is an under-
;;    |      |          | flow, 0x0 otherwise
(dcpu:defopcode sbx
  :opcode #x1b
  :cycles 3
  (setq dcpu:tmp-val (- dcpu:valB (+ dcpu:valA dcpu:ex)))
  (setq dcpu:ex (if (< dcpu:tmp-val 0) 0 1))
  (dcpu:dst-set dcpu:tmp-val))

;;  2 | 0x1e | STI b, a | sets b to a, then increases I and J by 1
(dcpu:defopcode sti
  :opcode #x1e
  :cycles 2
  (setq dcpu:i (dcpu:u16+ dcpu:i 1)
        dcpu:j (dcpu:u16+ dcpu:j 1))
  (dcpu:dst-set dcpu:valA))

;;  2 | 0x1f | STD b, a | sets b to a, then decreases I and J by 1
(dcpu:defopcode std
  :opcode #x1f
  :cycles 2
  (setq dcpu:i (dcpu:u16+ dcpu:i -1)
        dcpu:j (dcpu:u16+ dcpu:j -1))
  (dcpu:dst-set dcpu:valA))

(provide 'dcpu-opcode-list)
