;;
;; ~/0x10c/dcpu-el/dasm-mode.el ---
;;
;; $Id: dasm-mode.el,v 1.10 2012/04/20 07:21:10 harley Exp $
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

;;;;;

;; I like "nil"; "t" is more correct
(defvar dasm-indent-instr-line-keep-point t
  "*Keep the point when indenting.")

(defvar dasm-mode-hooks
  nil)

(defvar dasm-label-col        0)
(defvar dasm-instr-col        8)
(defvar dasm-instr-col-width  8)
(defvar dasm-instr-if-offset  2)

;;;;;

(defvar dasm-instr-if-lst
  '("ifb" "ife" "ifg" "ifn"))

(defvar dasm-instr-op-lst
  '(;; base
    "add" "and" "bor" "div" "mod"
    "mul" "set" "shl" "shr" "sub" "xor"
    ;; special
    "jsr"
    ;; pseudo-instrs
    "dat" "word"
    ;; extensions
    "break"
    "print"
    ;; dcpu keyboard reading. (nonstandard)
    "getc"))

(defvar dasm-instr-lst
  (append dasm-instr-if-lst dasm-instr-op-lst))

(defvar dasm-instr-start-regexp
  (concat "^[ \t]*" (regexp-opt dasm-instr-lst)))

(defvar dasm-instr-if-start-regexp
  (concat "^[ \t]*" (regexp-opt dasm-instr-if-lst)))

(defvar dasm-label-regexp
  ":[a-z0-9_]+")

(defvar dasm-label-start-regexp
  (concat "^[ \t]*" dasm-label-regexp))

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
    ))

(defvar dasm-mode-map
  (let ((map (make-sparse-keymap)))
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
    map))

(defvar dasm-asm-program "./dcpu16.git/a16")

(defvar dasm-mode-syntax-table
  (let ((dasm-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" dasm-mode-syntax-table)
    dasm-mode-syntax-table))

(defun dasm-assemble-file (filename &optional out-file)
  (or out-file (setq out-file (concat filename ".hex ")))
  (let ((buf     (get-buffer-create "*dasm assemble*"))
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

(defun dasm-assemble-and-run ()
  (require 'dcpu)
  (interactive)
  (save-buffer)
  ;;
  (let ((fn (buffer-file-name))
        (buf (current-buffer)))
    ;;
    (dcpu:activate-cpu (dcpu:new-cpu))
    (dcpu:ui-update)
    ;;
    (dasm-assemble-file fn)
    (dcpu:load-from-file (concat fn ".hex"))
    (when (not dcpu:display-mem-list)
      (dcpu:add-to-mem-list 0 (* 8 16)))
    ;;
    (dcpu:ui-enter)
    (dcpu:ui-update)
    (switch-to-buffer buf)))

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

;;;;;

(defun dasm-next-regexp (cnt regexp &optional noerror bound)
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
  (interactive "p")
  (dasm-next-regexp cnt dasm-label-start-regexp noerror))

(defun dasm-prev-label (&optional cnt noerror)
  (interactive "p")
  (dasm-next-regexp (- cnt) dasm-label-start-regexp noerror))

(defun dasm-next-instr (&optional cnt noerror)
  (interactive "p")
  (dasm-next-regexp cnt dasm-instr-start-regexp noerror))

(defun dasm-prev-instr (&optional cnt noerror)
  (interactive "p")
  (dasm-next-regexp (- cnt) dasm-instr-start-regexp noerror))

;;;;;

(defun dasm-line-commentp()
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "[ \t]*;"))))

(defun dasm-line-labelp ()
  (save-excursion
    (beginning-of-line)
    (looking-at dasm-label-start-regexp)))

(defun dasm-line-instrp ()
  (save-excursion
    (beginning-of-line)
    (looking-at dasm-instr-start-regexp)))

(defun dasm-prev-instr-if-p ()
  (save-excursion
    (beginning-of-line)
    (and
     (dasm-prev-instr 1 t) 
     (looking-at dasm-instr-if-start-regexp))))

;;;;;

(defun dasm-adj-whitespace (adj)
  (cond
   ((< 0 adj)
    (insert (make-string adj ? )))
   ((< adj 0)
    (delete-char (- (skip-chars-backward " \t" (+ (point) adj)))))))
;; (dasm-adj-whitespace 10)
;; (dasm-adj-whitespace -10)

(defun dasm-indent-instr-line ()
  "Intent the line as an instr wo preserving point"
  (interactive)
  ;; fix up the indent based on where we are in the line
  (let (p-start p-bol p-boi p-boa prev-indent)
    ;; we have to look at the prior two instr to figure the indent.
    (save-excursion
      (setq prev-indent dasm-instr-col)
      (beginning-of-line)
      ;; any instr one line up? use it
      (when (dasm-prev-instr 1 t)
        (setq prev-indent (current-indentation))
        ;; an if one line up? use it +2
        (if (looking-at dasm-instr-if-start-regexp)
          (setq prev-indent (+ dasm-instr-if-offset prev-indent))
          ;; an if two lines up? use it; no +2
          (when (dasm-prev-instr 1 t)
            (if (looking-at dasm-instr-if-start-regexp)
              (setq prev-indent (current-indentation)))))))
    ;; where we started
    (setq p-start (point-marker))
    ;;
    (beginning-of-line)
    (setq p-bol (point))
    ;;
    (skip-chars-forward " \t")
    (dasm-adj-whitespace (- prev-indent (current-column)))
    ;;
    (setq p-boi (point))
    ;;
    (when (looking-at "\\sw+")
      (forward-word 1)
      (skip-chars-forward " \t")
      (dasm-adj-whitespace (- dasm-instr-col-width (- (point) p-boi)))
      ;;
      (when (looking-at "\\sw+")
        (forward-word 1)))
    ;; could do more parsing etc
    (when dasm-indent-instr-line-keep-point
      (goto-char p-start))
    nil))

;; dasm-indent-line cant be too magic - indent-buffer uses it
(defun dasm-indent-line ()
  (interactive)
  (cond
   ;; toggle between bol and the prior line
   ((looking-at " *;;")
    (indent-line-to 0))
   ((looking-at " *;[^;]")
    (indent-line-to dasm-instr-col))
;;    (cond
;;     ((= 0 (current-column))
;;      (indent-line-to
;;       (save-excursion
;;         (forward-line -1)
;;         (current-indentation))))
;;   (t
;;      (indent-line-to 0))))
   ;; :label
   ((dasm-line-labelp)
    (indent-line-to 0))
;;    (let ((prior-indent (or
;;                         (save-excursion
;;                           (and (dasm-prev-label 1 t) (current-indentation)))
;;                         dasm-label-col)))
;;      (cond
;;       ((= prior-indent (current-indentation))
;;        (indent-line-to 0))
;;       (t
;;        (indent-line-to prior-indent)))))
   ;; treat the line as an instr, even if (not (dasm-line-instrp))
   (t
    (dasm-indent-instr-line)))
  nil)

(defun dasm-electric-tab ()
  (interactive)
  (let ((dasm-indent-instr-line-keep-point nil))
    ;; more magic here
    (dasm-indent-line)))

(defun dasm-electric-ret ()
  (interactive)
  (let ((dasm-indent-instr-line-keep-point nil))
    (dasm-indent-line)
    (end-of-line) 
    (insert "\n")
    (dasm-indent-line)))

;; left-margin-width
;; (set-window-margins (get-buffer-window) 2 0)
;; (insert (propertize "a" 'display '((margin left-margin) "X")))a
;;(insert (propertize "abc" 'face '(foreground-color)))
;;(insert (propertize " "
;;                    'display '((margin left-margin) "C")
;;
;;                    'rear-nonsticky t))
;; (eval-buffer)
(provide 'dasm-mode)
