;;
;; ~/projects/games/0x10c/dcpu-el/dcpu-defs.el ---
;;
;; $Id: dcpu-defs.el,v 1.12 2012/04/21 21:54:31 harley Exp $
;;

(eval-when-compile
  (require 'cl))

;; @todo more features
(defconst dcpu:feature-gofaster nil
  "When set, turn off some features to go faster. (unimplemented)")
(defconst dcpu:feature-undo     t
  "When set, record undo information for reverse debugging")
(defconst dcpu:feature-trace    t
  "When set, trace execution")

;; running state vars.
(defvar dcpu:addr      nil
  "The last address of interest used by the cpu")
(defvar dcpu:cycles    nil
  "The cycle count. (not instructions)")
(defvar dcpu:dev-vec   nil
  "")
(defvar dcpu:dst-addr  nil
  "Address for the output result")
(defvar dcpu:dst-reg   nil
  "Register name for the output result")
(defvar dcpu:icnt      nil
  "The instruction count. (not cycles.)")
(defvar dcpu:mem-vec   nil
  "The active memory")
(defvar dcpu:pc        nil
  "The current PC")
(defvar dcpu:reg-vec   nil
  "The registers of the cpu. (dcpu:pc and dcpu:sp are copied back.)")
(defvar dcpu:skip      nil
  "Skip the next instr when set")
(defvar dcpu:sp        nil
  "The current SP")

;;
(defvar dcpu:break     nil
  "List of reasons a break was signaled")
(defvar dcpu:breakpoint-addr-table (make-hash-table)
  "Hashtable of addresses to break at")

;;
(defvar dcpu:cur-cpu   nil
  "The current cpu to run. (A struct.)")

;; @todo split list
(defconst dcpu:reg-names
  '(A B C X Y Z I J SP PC O ;; defined
      ;; sim state saved as extra regs
      cycles cycles-max
      icnt   icnt-max
      skip state
      ))

;;
(defconst dcpu:mem-vec-size #x10000
  "Size of main memory.")
(defconst dcpu:reg-vec-size (length dcpu:reg-names)
  "Size of register set.")
(defconst dcpu:dev-vec-size 16
  "Number of devices allowed.")

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


;;(defvar dcpu:debug-break-funcs nil)
(defvar dcpu:post-step-hooks nil
  "Functions run after each step.")
(defvar dcpu:post-run-hooks nil
  "Functions run after each exit from a run of the emulator.")
(defvar dcpu:on-break-hooks nil
  "Funtions to run after each break is encountered.")

;;
(defvar dcpu:reg-bufname     "*dcpu reg*"
  "Name of the buffer to display registers in.")
(defvar dcpu:mem-bufname     "*dcpu mem*"
  "Name of the buffer to display memory in.")
(defvar dcpu:trace-bufname   "*dcpu trace*"
  "Name of the buffer to display the trace in.")
(defvar dcpu:screen-bufname  "*dcpu screen*"
  "Name of the buffer to display the screen in.")

(defvar dcpu:display-mem-list nil
  "List of memory addresses to display.")

;; UI
(defvar dcpu:ui-reg-lines  4
  "Number of lines for register display.")
(defvar dcpu:ui-mem-lines 16
  "Number of lines for memory display.
Shared with the screen, so must be greater than 12.")

;; http://0x10cwiki.com/wiki/Video_RAM
(defvar dcpu:screen-x 32
  "Number of screen columns.")
(defvar dcpu:screen-y 12
  "Number of screen lines.")
(defvar dcpu:screen-addr #x8000
  "Starting address of screen memory.")
(defvar dcpu:screen-display-ctl nil
  "When nil, show non-graphic chars as '.'.")
(defvar dcpu:screen-color nil
  "When t, colorize the screen.")
;; (setq dcpu:screen-color t)

(defvar dcpu:color-table
  ["#000000"
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
   "#ffffff"]
  "Vector of colors to use when decoding the bg and fg ")
;; (elt dcpu:color-table 0)

;;
(defvar dcpu:sit-for nil
  "Time to sit for after each emulator instruction.")
(defvar dcpu:read-char-delay 10.0
  "Seconds to wait for a keypress. (emulator is paused.)")

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