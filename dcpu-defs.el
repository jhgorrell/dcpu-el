;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-defs.el ---
;;
;; $Id: dcpu-defs.el,v 1.11 2012/04/21 05:27:39 harley Exp $
;;

(eval-when-compile (require 'cl))

;; @todo more features
(defconst dcpu:feature-gofaster nil)
(defconst dcpu:feature-undo     t)
(defconst dcpu:feature-trace    t)

;; running state vars.
(defvar dcpu:addr      nil)
(defvar dcpu:cycles    nil)
(defvar dcpu:dev-vec   nil)
(defvar dcpu:dst-addr  nil)
(defvar dcpu:dst-reg   nil)
(defvar dcpu:icnt      nil)
(defvar dcpu:mem-vec   nil)
(defvar dcpu:pc        nil)
(defvar dcpu:reg-vec   nil)
(defvar dcpu:skip      nil)
(defvar dcpu:sp        nil)

;;
(defvar dcpu:break     nil)
(defvar dcpu:breakpoint-addr-table (make-hash-table))

;;
(defvar dcpu:cur-cpu   nil)

;; @todo split list
(defconst dcpu:reg-names 
  '(A B C X Y Z I J SP PC O ;; defined
      ;; sim state saved as extra regs
      cycles cycles-max 
      icnt   icnt-max
      skip state
      ))

(defconst dcpu:instr-defs
  '(("set" #x1)
    ("add" #x2)
    ("sub" #x3)
    ("mul" #x4)
    ("div" #x5)
    ("mod" #x6)
    ("shl" #x7)
    ("shr" #x8)
    ("and" #x9)
    ("bor" #xa)
    ("xor" #xb)
    ("ife" #xc)
    ("ifn" #xd)
    ("ifg" #xe)
    ("ifb" #xf)
    ;; special
    ("jsr"   #x0 #x1)
    ("break" #x0 #x2)
    ("print" #x0 #x3)))

;;
(defconst dcpu:mem-vec-size #x10000)
(defconst dcpu:reg-vec-size (length dcpu:reg-names))
(defconst dcpu:dev-vec-size 16)

;;(defvar dcpu:debug-break-funcs nil)

(defvar dcpu:post-step-hooks nil)
(defvar dcpu:post-run-hooks nil)
(defvar dcpu:on-break-hooks nil)

;;
(defvar dcpu:reg-bufname     "*dcpu reg*")
(defvar dcpu:mem-bufname     "*dcpu mem*")
(defvar dcpu:trace-bufname   "*dcpu trace*")
(defvar dcpu:screen-bufname  "*dcpu screen*")

(defvar dcpu:display-mem-list nil)

;; UI
(defvar dcpu:ui-reg-lines  4)
(defvar dcpu:ui-mem-lines 16)
(defvar dcpu:ui-mem-list nil)

;; http://0x10cwiki.com/wiki/Video_RAM
(defvar dcpu:screen-x 32) 
(defvar dcpu:screen-y 12)
(defvar dcpu:screen-addr #x8000)
(defvar dcpu:screen-display-ctl nil)
(defvar dcpu:screen-color nil)
;; (setq dcpu:screen-color t)

(defvar dcpu:color-table
  '("#000000"  
    "#0000aa"  
    "#00aa00"  
    "#00aaaa"  
    "#aa0000"  
    "#aa00aa"  
    "#aa5500"  
    "#aaaaaa"
    "#555555"  
    "#5555ff"  
    "#55ff55"  
    "#55ffff"  
    "#ff5555" 
    "#ff55ff"  
    "#ffff55"  
    "#ffffff"))

;;
(defvar dcpu:sit-for nil)
(defvar dcpu:read-char-delay 10.0)

;;;;;;;;;;

(defstruct 
  (dcpu:cpu
   (:conc-name dcpu:cpu-)
   (:constructor dcpu:make-cpu))
  dev-vec
  mem-vec
  reg-vec)

(provide 'dcpu-defs)
;; (eval-buffer)