;;; motion-trail.el --- Pulse regions that were just changed -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created:  4 August 2024
;; Version: 0.1
;; Package-Requires: ((emacs "29.3"))
;; Homepage: https://github.com/emil-vdw/motion-trail
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defface motion-trail-face
  '((t (:inherit mouse-drag-and-drop-region)))
  "Face that motion trails are pulsed with.")

(defvar motion-trail--init-functions '())
(defvar motion-trail--remove-functions '())

(defun motion-trail--pulse-region (start end)
  "Momentarily pulse the region between start and end."
  (pulse-momentary-highlight-region start
                                    end
                                    'motion-trail-face))

(defmacro motion-trail--pulse-sexp-at-destination (error-message &rest body)
  "Navigate to a target sexp using BODY and pulse it signalling
user-error ERROR-MESSAGE on failure."
  (declare (indent 1))
  `(save-excursion
     (condition-case err
         (progn
           ,@body
           (motion-trail--pulse-sexp))
       (user-error error-message))))

;;; Treesitter integration.
(defun motion-trail-pulse-ts-node (node)
  "Pulse the treesitter NODE's region."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node)))
    (motion-trail--pulse-region start end)
    (cons start end)))


;;; Lisp functions.
(defun motion-trail--pulse-sexp ()
  (save-excursion
    (thing-at-point--beginning-of-sexp)
    (let ((sexp-start (point)))
      (forward-sexp)
      (motion-trail--pulse-region sexp-start
                                  (point)))))

(defun motion-trail--pulse-previous-sexp ()
  (motion-trail--pulse-sexp-at-destination "no previous sexp"
    (thing-at-point--beginning-of-sexp)))

(defun motion-trail--pulse-next-sexp (&rest args)
  (motion-trail--pulse-sexp-at-destination "no next sexp"
    (thing-at-point--beginning-of-sexp)
    (forward-sexp)
    (forward-sexp)
    (forward-sexp)))

(when (featurep 'combobulate)
  (defun motion-trail--combobulate-drag (original-fun direction)
    (let* ((result (apply original-fun (list direction)))
           (node (combobulate--get-nearest-navigable-node))
           (up (eq direction 'up))
           (sibling (combobulate--get-sibling node (if up 'forward 'backward))))
      (motion-trail-pulse-ts-node sibling)
      result))

  (defun motion-trail--combobulate-clone (original-fun node &rest args)
    (apply original-fun (cons node args))
    (motion-trail-pulse-ts-node (combobulate--get-nearest-navigable-node)))

  (defun motion-trail--combobulate-initialize ()
    (advice-add 'combobulate--drag :around #'motion-trail--combobulate-drag)
    (advice-add 'combobulate--clone-node :around #'motion-trail--combobulate-clone))

  (defun motion-trail--combobulate-remove ()
    (advice-remove 'combobulate--drag #'motion-trail--combobulate-drag)
    (advice-remove 'combobulate--clone-node #'motion-trail--combobulate-clone))

  (add-to-list 'motion-trail--init-functions #'motion-trail--combobulate-initialize)
  (add-to-list 'motion-trail--remove-functions #'motion-trail--combobulate-remove))

(when (featurep 'lext)
  (defun motion-trail--lext-initialize ()
    (advice-add 'lext-drag-sexp-back :after #'motion-trail--pulse-next-sexp)
    (advice-add 'lext-drag-sexp-forward :after #'motion-trail--pulse-previous-sexp)
    (advice-add 'lext-clone-sexp :after #'motion-trail--pulse-previous-sexp))

  (defun motion-trail--lext-remove ()
    (advice-remove 'lext-drag-sexp-back #'motion-trail--pulse-next-sexp)
    (advice-remove 'lext-drag-sexp-forward #'motion-trail--pulse-previous-sexp)
    (advice-remove 'lext-clone-sexp #'motion-trail--pulse-previous-sexp))

  (add-to-list 'motion-trail--init-functions #'motion-trail--lext-initialize)
  (add-to-list 'motion-trail--remove-functions #'motion-trail--lext-remove))


(define-minor-mode motion-trail-mode
  "Show motion trails after buffer manipulations."
  :init-value nil
  :lighter " mo"
  :global t
  :group 'motion-trail
  (if motion-trail-mode
      (dolist (func motion-trail--init-functions)
        (funcall func))
    (dolist (func motion-trail--remove-functions)
      (funcall func))))

(provide 'motion-trail)
;;; motion-trail.el ends here
