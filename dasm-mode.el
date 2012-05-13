;;; dasm-mode.el --- mode for editing dcpu16 assembly
;;
;; ~/0x10c/dcpu-el/dasm-mode.el ---
;;
;; $Id: dasm-mode.el,v 1.19 2012/05/13 01:15:58 harley Exp $
;;

;; in your ~/.emacs:
;;   (add-to-list 'load-path "~/path/to/dasm-el")
;;   (add-to-list 'auto-mode-alist '("\\.dasm\\'" . dasm-mode))

;; Url:        https://raw.github.com/jhgorrell/dcpu-el/master/dasm-mode.el
;; Homepage:   https://github.com/jhgorrell/dcpu-el
;; Author:     James Harley Gorrell <harley@panix.com>
;; Keywords:   dasm-16
;; Version:    0.1
;; License:    GPL v3 or later (same as emacs)

;; (require 'dcpu-autoloads nil t) ???
;; (require 'dcpu)

;;; Commentary:
;;

;;; Code:

(defvar dasm-label-col        0
  "*The target column for labels.")
(defvar dasm-instr-col        8
  "*The target column for instructions.")
(defvar dasm-instr-col-width  8
  "*The width of the instruction column.")
(defvar dasm-instr-if-offset  2
  "*Amount to indent the instr after an if.")

;; I like "nil"; "t" is more correct
(defvar dasm-indent-instr-line-keep-point t
  "*Keep the point when indenting.")

(defvar dasm-mode-hooks nil
  "Hook run when entering `dasm-mode'.")

(defvar dasm-mode-load-hooks nil
  "Hook run when loading `dasm-mode'.")

(defvar dasm-output-bufname " *dasm assemble*"
  "The buffer name to use for dasm assembly output.")

;;;;;

;; these should be taken from "dcpu:opcode-basic-vec"
;; and "dcpu:opcode-special-vec"

;; (dcpu:gen-instr-if-lst)
(defvar dasm-instr-if-lst
  '("ifb" "ifc" "ife" "ifg" "ifl" "ifn" "ifu")
  "List of dasm 'if' instructions.")

;; (dcpu:gen-instr-lst)
(defvar dasm-instr-op-lst
  '(;; base
    "add" "adx" "and" "asr" "bor" "div" "dvi" "hcf"
    "hwi" "hwn" "hwq" "iag" "iaq" "ias" "ifa" "ifb"
    "ifc" "ife" "ifg" "ifl" "ifn" "ifu" "int" "jsr"
    "mdi" "mli" "mod" "mul" "rfi" "sbx" "set" "shl"
    "shr" "std" "sti" "sub" "xor"
    ;; pseudo-instrs
    "dat" "word"
    ;; extensions
    "break"
    "print"
    ;; dcpu keyboard reading. (nonstandard)
    "getc")
  "List of dasm instructions, not including 'if's.")

(defvar dasm-instr-lst
  (append dasm-instr-if-lst dasm-instr-op-lst)
  "List of dasm instructions (normal and ifs).")

(defvar dasm-instr-start-regexp
  (concat "^[ \t]*" (regexp-opt dasm-instr-lst))
  "Regexp which matches the start of an instruction line.")

(defvar dasm-instr-if-start-regexp
  (concat "^[ \t]*" (regexp-opt dasm-instr-if-lst))
  "Regexp which matches the start of an 'if' line.")

(defvar dasm-label-regexp
  ":[a-z0-9_.]+"
  "Regexp which matches a label.")

(defvar dasm-label-start-regexp
  (concat "^[ \t]*" dasm-label-regexp)
  "Regexp which matches the start of a label line.")

;; @todo make our own faces
(defvar dasm-font-lock-keywords
  `((
     ;; comment
     (";.*" . font-lock-comment-face)
     ;;
     ("\\\"[^\\\"\n]*\\\"" . font-lock-string-face)
     ;; "":label"" or "label:"
     (,(concat "\\(" dasm-label-regexp "\\)")
      . font-lock-function-name-face)
     ;; assembler directives
     ("\\.\\sw+" . font-lock-variable-name-face)
     ;; dasm instructions
     (
      ,(concat "\\<" (regexp-opt dasm-instr-lst ) "\\>")
      . font-lock-keyword-face)
     )
    t ;; keywords-only
    t ;; case fold (@todo: case in regexp?
    )
  "Expressions to higlight in `dasm-mode'.")

(defvar dasm-mode-map
  (let ((map (make-sparse-keymap "Dasm Mode")))
    (define-key map "\C-j"     'newline-and-indent)
    (define-key map "\C-c\C-c" 'dasm-assemble-and-run)
    ;;
    (define-key map "\t"       'dasm-electric-tab)
    (define-key map "M-C-m"    'dasm-electric-ret)
    ;;
    (define-key map [27 down]  'dasm-next-label)
    (define-key map [27 up]    'dasm-prev-label)
    ;; (define-key map "C-cB" 'dasm-toggle-breakpoint)
    ;; (define-key map "C-cB" 'dasm-toggle-breakpoint)
    map)
  "Keymap used in `dasm-mode' buffers.")

(defvar dasm-asm-program "./dcpu16.git/a16"
  "*Program which is used for assembly.")

(defvar dasm-mode-syntax-table
  (let ((dasm-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" dasm-mode-syntax-table)
    dasm-mode-syntax-table)
  "Syntax for `dasm-mode'.")

;;;###autoload
(defun dasm-mode ()
  "Major mode for editing DCPU-16 assembly."
  (interactive)
  (kill-all-local-variables)
  ;;
  (setq major-mode 'dasm-mode)
  (setq mode-name "dasm")
  (use-local-map dasm-mode-map)
  ;;
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'dasm-indent-line)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults dasm-font-lock-keywords)
  ;;
  (run-mode-hooks 'dasm-mode-hooks)
  nil)

;;;###autoload
(defun dasm-assemble-file (filename &optional out-file)
  "Assemble FILENAME putting the bytes into OUT-FILE."
(or out-file (setq out-file (concat filename ".hex ")))
  (let ((buf     (get-buffer-create dasm-output-bufname))
        (cmd      (concat
                   dasm-asm-program " "
                   "-O hex "
                   "-o " out-file
                   filename)))
    (when (file-exists-p out-file)
      (delete-file out-file))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert ";; cmd" cmd "\n")
    (call-process-shell-command cmd nil buf t)))
;; (dasm-assemble-file "./test-hello-emacs.dasm")

;;;###autoload
(defun dasm-assemble-and-run ()
  "Assemble the current buffer and run it in the emulator."
  (interactive)
  (save-buffer)
  ;;
  (let ((fn (buffer-file-name))
        (buf (current-buffer)))
    ;;
    (dcpu:init-cpu)
    (dcpu:ui-update)
    ;;
    (dasm-assemble-file fn)
    (dcpu:load-from-file (concat fn ".hex"))
    (when (not dcpu:display-areg-list)
      (dcpu:aregionlist-push
       'dcpu:display-areg-list
       (dcpu:make-aregion :s 0 :l (* 8 16) :d 'words)))
    ;;
    (dcpu:ui-enter)
    (dcpu:ui-update)
    (switch-to-buffer buf)))

;;;;;

(defun dasm-next-regexp (cnt regexp &optional noerror bound)
  "Find the next CNT REGEX.
NOERROR
BOUND.
Argument REGEXP ."
  (let ((case-fold-search t))
    (prog1
        (cond
         ((< cnt 0)
          (search-backward-regexp regexp (or bound (point-min)) noerror (- cnt)))
         ((< 0 cnt)
          (when (save-excursion
                  (beginning-of-line)
                  (looking-at regexp))
            (end-of-line))
          (search-forward-regexp regexp (or bound (point-max)) noerror cnt))
         (t
          (error "")))
      (beginning-of-line))))

;;;;;

(defun dasm-next-label (&optional cnt noerror)
  "Find the next label.
CNT for the nth label.
NOERROR non-nil for no error."
  (interactive "p")
  (dasm-next-regexp cnt dasm-label-start-regexp noerror))

(defun dasm-prev-label (&optional cnt noerror)
  "Find the next label.
CNT for the nth label.
NOERROR non-nil for no error."
  (interactive "p")
  (dasm-next-regexp (- cnt) dasm-label-start-regexp noerror))

(defun dasm-next-instr (&optional cnt noerror)
  "Find the next label.
CNT for the nth label.
NOERROR non-nil for no error."
  (interactive "p")
  (dasm-next-regexp cnt dasm-instr-start-regexp noerror))

(defun dasm-prev-instr (&optional cnt noerror)
  "Find the next label.
CNT for the nth label.
NOERROR non-nil for no error."
  (interactive "p")
  (dasm-next-regexp (- cnt) dasm-instr-start-regexp noerror))

;;;;;

(defun dasm-line-commentp()
  "True if this line is a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "[ \t]*;"))))

(defun dasm-line-labelp ()
  "True if this line is a label."
  (save-excursion
    (beginning-of-line)
    (looking-at dasm-label-start-regexp)))

(defun dasm-line-instrp ()
  "True if this line is an instruction."
  (save-excursion
    (beginning-of-line)
    (looking-at dasm-instr-start-regexp)))

(defun dasm-prev-instr-if-p ()
  "True if the prev instruction is an 'if'."
  (save-excursion
    (beginning-of-line)
    (and
     (dasm-prev-instr 1 t)
     (looking-at dasm-instr-if-start-regexp))))

;;;;;

(defun dasm-adj-whitespace (adj)
  "Adjust the whitespace on the line.
Argument ADJ ."
  (cond
   ((< 0 adj)
    (insert (make-string adj ? )))
   ((< adj 0)
    (delete-char (- (skip-chars-backward " \t" (+ (point) adj)))))))
;; (dasm-adj-whitespace 10)
;; (dasm-adj-whitespace -10)

(defun dasm-space-to-column (targ)
  "Insert spaces until we get to column TARG."
  (skip-chars-forward " \t")
  (let ((more (- targ (current-column))))
    (if (< 0 more)
      (insert (make-string more ? )))))
;; (dasm-space-to-column 10)
;; (dasm-space-to-column 50)

(defvar dasm-compute-instr-indent nil
  "The value of the the last call to dasm-compute-instr-indent.")

(defun dasm-compute-instr-indent ()
  "Compute the indentation level of this instruction."
  (let ((col dasm-instr-col))
    (save-excursion
      (beginning-of-line)
      ;; any instr one line up? use it
      (when (dasm-prev-instr 1 t)
        (setq col (current-indentation))
        ;; an "if"" one line up? use it +2
        (if (looking-at dasm-instr-if-start-regexp)
          (setq col (+ dasm-instr-if-offset col))
          ;; an if two lines up? use it; no +2
          (when (dasm-prev-instr 1 t)
            (if (looking-at dasm-instr-if-start-regexp)
              (setq col (current-indentation)))))))
    (setq dasm-compute-instr-indent col)))

(defun dasm-indent-instr-line ()
  "Intent the line as an instr wo preserving point."
  (let ((col (dasm-compute-instr-indent)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]+$")
        (delete-horizontal-space)
        (skip-chars-forward " \t")
        (dasm-adj-whitespace (- col (current-column)))
        (forward-word 1)
        (if (looking-at "[ \t]+$")`
          (delete-horizontal-space)
          (skip-chars-forward " \t")
          (dasm-adj-whitespace (- (+ col dasm-instr-col-width) (current-column))))))))

;; dasm-indent-line cant be too magic - indent-buffer uses it
(defun dasm-indent-line ()
  "Indent the current line.  (Non magicly.)."
  (interactive)
  (cond
   ;; toggle between bol and the prior line
   ((looking-at " *;;")
    (indent-line-to 0))
   ((looking-at " *;[^;]")
    (indent-line-to (dasm-compute-instr-indent)))
   ((dasm-line-labelp)
    (indent-line-to 0))
   (t
    (dasm-indent-instr-line)))
  nil)

(defun dasm-open-line ()
  "Open a line for editing."
  (interactive)
  (end-of-line)
  (insert "\n")
  (dasm-space-to-column (dasm-compute-instr-indent)))

(defun dasm-electric-tab ()
  "Magicly do-what-i-mean with the tab key."
  (interactive)
  (let ((p-start (point-marker))
        (dasm-compute-instr-indent nil))
    (dasm-indent-line)
    (goto-char p-start))
  ;; do the magic
  (let* ((ccol (current-column))
         (icol (dasm-compute-instr-indent))
         (acol (+ icol dasm-instr-col-width)))
    (cond
     ((or (dasm-line-labelp) (dasm-line-commentp))
      (dasm-open-line))
     ;; instr!
     ((< ccol icol)
      (dasm-space-to-column icol))
     ;;
     ((= ccol icol)
      (when (looking-at "\\sw+")
        (forward-word 1)
        (dasm-space-to-column acol)))
     ((< ccol acol)
      (dasm-space-to-column acol))
     ((<= acol acol)
      (if (looking-at "[ \t]*$")
        (dasm-open-line)
        (forward-word 1)))
     )))

(defun dasm-electric-ret ()
  "Magicly do-what-i-mean with the return key."
  (interactive)
  (let ((dasm-indent-instr-line-keep-point nil))
    (dasm-indent-line)
    (end-of-line)
    (insert "\n")
    (dasm-indent-line)))

;;
(run-hooks dasm-mode-load-hooks)

;; (eval-buffer)
(provide 'dasm-mode)

;;; dasm-mode.el ends here
