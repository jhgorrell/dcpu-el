;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-defs.el ---
;;
;; $Id: dcpu-defs.el,v 1.27 2012/05/08 23:04:29 harley Exp $
;;

(eval-when-compile
  (require 'cl))

;;;;;

(defvar dcpu:mem-size #x10000)

;; the cpu registers & state
(defvar dcpu:a    0)
(defvar dcpu:b    0)
(defvar dcpu:c    0)
(defvar dcpu:x    0)
(defvar dcpu:y    0)
(defvar dcpu:z    0)
(defvar dcpu:i    0)
(defvar dcpu:j    0)
(defvar dcpu:ex   0)
(defvar dcpu:sp   0)
(defvar dcpu:pc   0)

;;
(defvar dcpu:ia 0)
(defvar dcpu:ia-enabled nil)
(defvar dcpu:ia-queue nil)

;;
(defvar dcpu:mem-vec nil)
(defvar dcpu:dev-vec nil)
;;
(defvar dcpu:state-breakpoints (make-hash-table))
(defvar dcpu:state-breaks   nil)
(defvar dcpu:state-cycles   0)
(defvar dcpu:state-icount   0)
(defvar dcpu:state-onfire   nil)
(defvar dcpu:state-skip     nil)

;;
(defvar dcpu:auto-checkpoint-name nil)
(defvar dcpu:checkpoint-vars
  '(dcpu:a dcpu:b dcpu:c
    dcpu:x dcpu:y dcpu:z
    dcpu:i dcpu:j
    dcpu:ex dcpu:sp dcpu:pc
    ;;
    dcpu:ia
    dcpu:ia-queue    
    dcpu:ia-enabled
    ;; should be prim values, so copy is a copy
    dcpu:mem-vec
    ;; the checkpoint will copy the vec but not the objects
    ;; so side effects might occur.
    ;; @todo add a "dcpu:dev-copy"
    dcpu:dev-vec
    ;;
    dcpu:state-breakpoints
    dcpu:state-breaks
    dcpu:state-cycles
    dcpu:state-icount
    dcpu:state-onfire
    dcpu:state-skip))

(defvar dcpu:checkpoints (make-hash-table :test 'equal))

;;;;;

(defvar dcpu:run-until-icount nil)
(defvar dcpu:run-until-cycles nil)
(defvar dcpu:run-sit-for      nil)

(defvar dcpu:cur-instr   nil)
(defvar dcpu:cur-op      nil)
(defvar dcpu:cur-pc      nil)
(defvar dcpu:dst-addr    nil)
(defvar dcpu:dst-reg     nil)
(defvar dcpu:dst-val     nil)
(defvar dcpu:orig-pc     nil)
(defvar dcpu:tmp-addr    nil)
(defvar dcpu:tmp-val     nil)
(defvar dcpu:valA        nil)
(defvar dcpu:valB        nil)

;;;;;

;; start with space to disable undo.
(defvar dcpu:mem-bufname      " *dcpu mem*")
(defvar dcpu:reg-bufname      " *dcpu reg*")
(defvar dcpu:screen-bufname   " *dcpu screen*")
(defvar dcpu:trace-bufname    " *dcpu trace*")
(defvar dcpu:load-mem-bufname " *dcpu load mem*")

;;;;;

(defvar dcpu:run-start-hook nil)
(defvar dcpu:run-step-hook nil)
(defvar dcpu:run-stop-hook nil)
(defvar dcpu:run-interrupt-hook nil)

;;;;;

(defvar dcpu:trace t)

;;;;;

(defvar dcpu:screen-x-size 32
  "Number of screen columns.")
(defvar dcpu:screen-y-size 12
  "Number of screen lines.")
(defvar dcpu:screen-addr #x8000
  "Starting address of screen memory.")
(defvar dcpu:screen-display-ctl nil
  "When nil, show non-graphic chars as '.'.")
(defvar dcpu:screen-do-map-chars nil
  "")
(defvar dcpu:screen-color nil
  "When t, colorize the screen.")

;; UI
(defvar dcpu:ui-reg-lines  4
  "Number of lines for register display.")
(defvar dcpu:ui-mem-lines 16
  "Number of lines for memory display.
Shared with the screen, so must be greater than 12.")

(defvar dcpu:display-areg-list nil
  "List of memory address regions to display.")

;;;;;

(defvar dcpu:opcode-basic-vec   (make-vector #x20 nil))
(defvar dcpu:opcode-special-vec (make-vector #x40 nil))
(defvar dcpu:opcode-hash-table  (make-hash-table))

(defstruct
  (dcpu:opcode
   (:conc-name dcpu:opcode-)
   (:constructor dcpu:make-opcode))
  name
  opcode
  specialcode
  ;;
  cycles
  body)

;;;;;

;; (eval-buffer)
(provide 'dcpu-defs)
