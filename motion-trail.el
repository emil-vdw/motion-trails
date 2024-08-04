;;; motion-trail.el --- Pulse regions that were just changed -*- lexical-binding: t; -*-

;; Author: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Maintainer: Emil van der Westhuizen <vdwemil@protonmail.com>
;; Created:  4 August 2024
;; Version: 0.1
;; Package-Requires: ((emacs "29.3"))
;; Homepage: https://github.com/emil-vdw/motion-trial
;; Keywords:

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; Code:

(defface motion-trail-face
  '((t (:inherit mouse-drag-and-drop-region)))
  "Face that motion trails are pulsed with.")

(defun motion-trail--pulse-region (start end)
  "Momentarily pulse the region between start and end."
  (pulse-momentary-highlight-region start
                                    end
                                    'motion-trail-face))

;;; Combobulate integration.
(defun motion-trial-pulse-ts-node (node)
  ""
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node)))
    (motion-trail--pulse-region start end)
    (cons start end)))

(defun motion-trail--start-of-sexp ()
  "Like `thing-at-point--beginning-of-sexp' but favors subsequent sexps."
  ;; We treat the start of an sexp, like a \( as a valid starting
  ;; point for an sexp. `thing-at-point--beginning-of-sexp' will jump
  ;; to the stat of the previous sexp.
  (unless
      (save-excursion
        ;; We are already at the start of an s-expression.
        (equal (point)
               (ignore-errors (forward-sexp)
                              (backward-sexp)
                              (point))))
    (thing-at-point--beginning-of-sexp)))

(defun motion-trail--pulse-previous-sexp ()
  (save-excursion
    (let ((sexp-start (progn (motion-trail--start-of-sexp)
                             (backward-sexp)
                             (point)))
          (sexp-end (progn (forward-sexp)
                           (point))))
      (motion-trail--pulse-region sexp-start
                                  sexp-end))))

(defun motion-trail--pulse-next-sexp (&rest args)
  (save-excursion
    (motion-trail--start-of-sexp)

    (condition-case err
        (progn (forward-sexp)
               (forward-sexp)
               (forward-sexp)
               (motion-trail--pulse-previous-sexp))
      (error
       (user-error "No next sexp")))))

(when (featurep 'combobulate)
  (defun motion-trail--combobulate-drag (original-fun direction)
    (let* ((result (apply original-fun (list direction)))
           (node (combobulate--get-nearest-navigable-node))
           (up (eq direction 'up))
           (sibling (combobulate--get-sibling node (if up 'forward 'backward))))
      (motion-trial-pulse-ts-node sibling)
      result))

  (advice-add 'combobulate--drag :around #'motion-trail--combobulate-drag))

(when (featurep 'lext)
  (advice-add 'lext-drag-sexp-back :after #'motion-trail--pulse-next-sexp)
  (advice-add 'lext-drag-sexp-forward :after #'motion-trail--pulse-previous-sexp)
  (advice-add 'lext-clone-sexp :after #'motion-trail--pulse-previous-sexp))

(provide 'motion-trail)
;;; motion-trail.el ends here
