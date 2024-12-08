;;; motion-trails.el --- Pulse regions that were just changed -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created:  4 August 2024
;; Version: 0.1
;; Package-Requires: ((emacs "29"))
;; Homepage: https://github.com/emil-vdw/motion-trails
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defface motion-trails-face
  '((t (:inherit region)))
  "Face that motion trails are pulsed with.")

(defvar motion-trails--init-functions '())
(defvar motion-trails--remove-functions '())

(defun motion-trails--pulse-region (&optional start end &rest _)
  "Momentarily pulse the region between start and end."
  (let ((start (or start
                   (region-beginning)))
        (end (or end
                 (region-end))))
    (pulse-momentary-highlight-region start
                                      end
                                      'motion-trails-face)))

(defmacro motion-trails--pulse-sexp-at-destination (error-message &rest body)
  "Navigate to a target sexp using BODY and pulse it signalling
user-error ERROR-MESSAGE on failure."
  (declare (indent 1))
  `(save-excursion
     (condition-case _
         (progn
           ,@body
           (motion-trails--pulse-sexp))
       (user-error ,error-message))))

(defun motion-trails--builtin-init ()
  (advice-add #'yank :after #'motion-trails--pulse-region)
  (advice-add #'yank-pop :after #'motion-trails--pulse-region)
  ;; We have to pulse before because of the lengthy point-mark exchange animation.
  (advice-add #'kill-ring-save :before #'motion-trails--pulse-region))

(defun motion-trails--builtin-remove ()
  (advice-remove 'yank #'motion-trails--pulse-region)
  (advice-remove 'yank-pop #'motion-trails--pulse-region)
  (advice-remove 'kill-ring-save #'motion-trails--pulse-region))

(add-to-list 'motion-trails--init-functions #'motion-trails--builtin-init)
(add-to-list 'motion-trails--remove-functions #'motion-trails--builtin-remove)


;;; Treesitter integration.
(defun motion-trails-pulse-ts-node (node)
  "Pulse the treesitter NODE's region."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node)))
    (motion-trails--pulse-region start end)
    (cons start end)))


;;; Lisp functions.
(defun motion-trails--pulse-sexp ()
  (save-excursion
    (thing-at-point--beginning-of-sexp)
    (let ((sexp-start (point)))
      (forward-sexp)
      (motion-trails--pulse-region sexp-start
                                  (point)))))

(defun motion-trails--pulse-previous-sexp ()
  (motion-trails--pulse-sexp-at-destination "no previous sexp"
    (thing-at-point--beginning-of-sexp)))

(defun motion-trails--pulse-next-sexp (&rest _)
  (motion-trails--pulse-sexp-at-destination "no next sexp"
    (thing-at-point--beginning-of-sexp)
    (forward-sexp)
    (forward-sexp)
    (forward-sexp)))

(when (featurep 'combobulate)
  (defun motion-trails--combobulate-drag (original-fun direction)
    (let* ((result (apply original-fun (list direction)))
           (node (combobulate--get-nearest-navigable-node))
           (up (eq direction 'up))
           (sibling (combobulate--get-sibling node (if up 'forward 'backward))))
      (motion-trails-pulse-ts-node sibling)
      result))

  (defun motion-trails--combobulate-clone (original-fun node &rest args)
    (apply original-fun (cons node args))
    (motion-trails-pulse-ts-node (combobulate--get-nearest-navigable-node)))

  (defun motion-trails--combobulate-initialize ()
    (advice-add 'combobulate--drag :around #'motion-trails--combobulate-drag)
    (advice-add 'combobulate--clone-node :around #'motion-trails--combobulate-clone))

  (defun motion-trails--combobulate-remove ()
    (advice-remove 'combobulate--drag #'motion-trails--combobulate-drag)
    (advice-remove 'combobulate--clone-node #'motion-trails--combobulate-clone))

  (add-to-list 'motion-trails--init-functions #'motion-trails--combobulate-initialize)
  (add-to-list 'motion-trails--remove-functions #'motion-trails--combobulate-remove))

(when (featurep 'lext)
  (defun motion-trails--lext-initialize ()
    (advice-add 'lext-drag-sexp-back :after #'motion-trails--pulse-next-sexp)
    (advice-add 'lext-drag-sexp-forward :after #'motion-trails--pulse-previous-sexp)
    (advice-add 'lext-clone-sexp :after #'motion-trails--pulse-previous-sexp))

  (defun motion-trails--lext-remove ()
    (advice-remove 'lext-drag-sexp-back #'motion-trails--pulse-next-sexp)
    (advice-remove 'lext-drag-sexp-forward #'motion-trails--pulse-previous-sexp)
    (advice-remove 'lext-clone-sexp #'motion-trails--pulse-previous-sexp))

  (add-to-list 'motion-trails--init-functions #'motion-trails--lext-initialize)
  (add-to-list 'motion-trails--remove-functions #'motion-trails--lext-remove))

;;;###autoload
(define-minor-mode motion-trails-mode
  "Show motion trails after buffer manipulations."
  :init-value nil
  :lighter " mo"
  :global t
  :group 'motion-trails
  (if motion-trails-mode
      (dolist (func motion-trails--init-functions)
        (funcall func))
    (dolist (func motion-trails--remove-functions)
      (funcall func))))

(provide 'motion-trails)
;;; motion-trails.el ends here
