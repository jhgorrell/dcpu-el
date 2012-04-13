;;
;; ~/0x10c/dcpu-el/dasm-mode.el ---
;;
;; $Id: dasm-mode.el,v 1.1 2012/04/12 22:28:21 harley Exp $
;;

;; in your ~/.emacs:
;;   (add-to-list 'load-path "~/path/to/dasm-el")
;;   (add-to-list 'auto-mode-alist '("\\.dasm\\'" . dasm-mode))

(defvar dasm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [f11] 'dasm-assemble-and-run)
    ;;(define-key map "C-cB" 'dasm-toggle-breakpoint)
    map))

(defvar dasm-mode-hooks nil)

(defvar dasm-font-lock-keywords
  `((
    ;; comment
    (";.*" . font-lock-comment-face)
    ;;
    ("\\\"[^\\\"\n]*\\\"" . font-lock-string-face)
    ;; "":label"" or "label:"
    ("\\(:\\sw+\\|\\sw+:\\)" . font-lock-function-name-face)
    ;; assembler directives
    ("\\.\\sw+" . font-lock-variable-name-face)
    ;; dasm instructions
    (
     ,(concat 
       "\\<"
       (regexp-opt
        '(
          ;; base
          "add" "and" "bor" "div" "ifb" "ife" "ifg" "ifn" "mod"
          "mul" "set" "shl" "shr" "sub" "xor"
          ;; special
          "jsr"
          ;; pseudo-instrs
          "dat"
          ;; extensions
          "break"
          "print"
          ))
       "\\>")
     . font-lock-keyword-face)
     )
    t ;; keywords-only
    t ;; case fold
    ))
    
(defun dasm-assemble-and-run ()
  (interactive)
  (save-buffer)
  (dasm:assemble-buffer-to-mem)
  (dcpu:standard-ui)
  nil)

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
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults dasm-font-lock-keywords)
  ;;
  (run-mode-hooks 'dasm-mode-hooks)
  nil)

;; (eval-buffer)
