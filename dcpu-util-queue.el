;;; dcpu-util-queue.el --- queue data structure
;;
;; ~/0x10c/dcpu-el/dcpu-util-queue.el ---
;;
;; $Id: dcpu-util-queue.el,v 1.4 2012/05/13 01:45:31 harley Exp $
;;

;;; Commentary:
;;  A queue data structure.

(eval-when-compile
  (require 'cl))

;;; Code:

(defun dcpu:make-queue ()
  "Create a queue structure."
  (list 'dcpu:queue nil nil))

(defun dcpu:queue-p (queue)
  "Is this a QUEUE?"
  (and (listp queue) (eq (car queue) 'dcpu:queue) queue))

(defun dcpu:queue-head (queue)
  "Return head of the QUEUE."
  (assert (dcpu:queue-p queue))
  (cadr queue))

(defun dcpu:queue-tail (queue)
  "Return the tail of the QUEUE."
  (assert (dcpu:queue-p queue))
  (caddr queue))

(defun dcpu:queue-nth (n queue)
  "Return the Nth item of the QUEUE."
  (assert (dcpu:queue-p queue))
  (nth n (cadr queue)))

(defun dcpu:queue-append (val queue)
  "Append VAL to the end of the QUEUE."
  (assert (dcpu:queue-p queue))
  (let ((val-cons (cons val nil)))
    (if (caddr queue)
      (setcdr (caddr queue) val-cons)
      (setcar (cdr  queue) val-cons))
    (setcar (cddr queue) val-cons)
    queue))

(defun dcpu:queue-pop (queue)
  "Pop the first item from the QUEUE."
  (assert (dcpu:queue-p queue))
  (let ((val-cons (cadr queue)))
    (setcar (cdr queue) (cdr val-cons))
    (if (not (cadr queue))
      (setcar (cddr queue) nil))
    (car val-cons)))

(defun dcpu:queue-copy (queue)
  "Make a copy of the QUEUE."
  (assert (dcpu:queue-p queue))
  (let ((new-queue (dcpu:make-queue)))
    (setcar (cdr new-queue) (copy-sequence (dcpu:queue-head queue)))
    (setcar (cddr new-queue) (last (cadr new-queue)))
    new-queue))

;; (eval-buffer)
;; (setq q (dcpu:make-queue))
;; (dcpu:queue-p q)
;; (dcpu:queue-append 'a q)
;; (dcpu:queue-append 'b q)
;; (dcpu:queue-copy q)
;; (progn (dotimes (i 5) (dcpu:queue-append i q)) q)
;; (dcpu:queue-nth 4 q)
;; (while (dcpu:queue-head q) (message "pop: %s" (dcpu:queue-pop q)))
;; (dcpu:queue-append 3 q)

(provide 'dcpu-util-queue)
;;; dcpu-util-queue.el ends here
