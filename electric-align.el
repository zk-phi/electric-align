;;; electric-align.el --- Insert a certain amout of spaces at once to easily align columns

;; Copyright (C) 2015- zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Version: 2.0.0beta
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; Put electric-align.el in a "load-path"ed directory, and require
;; this script.
;;
;;   (require 'electric-align)
;;
;; then call `M-x electric-align-mode' to turn on the mode.
;;
;; You can also add the function to `prog-mode-hook', to enable the mode
;; in all buffers editing programs.
;;
;;   (add-hook 'prog-mode-hook 'electric-align-mode)
;;
;; While `electric-align-mode' is turned on, you can insert a certain
;; amount of spaces at once by pressing `SPC' multiple times.

;;; Change Log:

;; 2.0.0 Rewrite almost everything

;;; Code:

;; *TODO* NOT compatible with `multiple-cursors' for now

;; [How this plug-in determines the alignment columns]
;;
;; Let `max-column'    be 0
;; Let `min-column'    be a reasonably big number
;; Let `active-aligns' be an empty list
;;
;; For each lines above then below this line :
;;
;;  0. If the line is empty
;;     -> Stop there
;;
;;  1. Check the number of characters the line has
;;     -> If the line has more than `max-column' characters
;;        -> If the line has some alignments (\s[^\s]) above `max-column'
;;           -> Move the alignments to `active-aligns'
;;        -> Anyway update `max-column'
;;     -> If the line's first alignments are below `min-column'
;;        -> Move the alignments to `active-aligns' and update `min-column'
;;
;;  2. If the line lacks an alignment listed in `active-aligns'
;;     -> If either the next or the last alignment in the line is not listed in `active-aligns'
;;        -> Stop there
;;
;;     [Okay]
;;              v   o           v
;;       -2: foo foobar          foo
;;       -1: foo foo foo foo foo foo
;;              ^   ^   ^   ^   ^
;;              o
;;       -2: foobar
;;       -1: foo foo foo foo
;;              ^   ^   ^
;;     [NG]
;;              v   o  vo   o   v
;;       -2: foo foobar foobar   foo
;;       -1: foo foo foo foo foo foo
;;              ^   ^  x^   ^   ^
;;
;;  3. If the line has an extra alignment NOT listed in `active-aligns'
;;     -> If the line lacks either the next or the last alignment in `active-aligns'
;;       -> Stop there
;;     -> Otherwise add the alignment to `active-aligns'
;;
;;     [Okay]
;;              v   o   o   v
;;       -2: foo foo foo foo foo
;;       -1: foo foobar      foo
;;              ^           ^
;;              v   o   o   o
;;       -2: foo foo foo foo foo
;;       -1: foo foobar
;;              ^
;;     [NG]
;;              v   o  xo     v
;;       -2: foo foo foo foo   foo
;;       -1: foo foobar foobar foo
;;              ^      ^      ^

;; [How this plug-in determines which alignment to use]
;;
;; 1. Find the last active alignment in this line
;;
;;        v    v    v
;;    hoge hogehoge  hoge <- concatenated cell
;;    hoge hoge hoge hoge
;;    foo  foobar |
;;        ^
;;
;; 2. For each alignments over the alignment found in step 1
;;
;;  ⅰ. If the alignment is below the current column
;;   -> Align all lines other than the current line
;;
;;        v      v    v
;;    hoge hogehoge    hoge <- *do not modify a concatenated cell*
;;    hoge hoge   hoge hoge
;;    foo  foobar |
;;               ^
;;
;;  ii. If the alignment is over the current column
;;   -> Align this line
;;
;;        v    v    v
;;    hoge hogehoge  hoge
;;    hoge hoge hoge hoge
;;    foo  foobar    |
;;                  ^

(require 'cl-lib)

(defconst electric-align-version "2.0.0beta")

(defgroup electric-align nil
  "Automatically align columns."
  :group 'emacs)

(defcustom electric-align-shortcut-commands nil
  "Extra list of commands counted as SPC."
  :group 'electric-align)

(defface electric-align-face
  '((t (:background "#554444")))
  "Face used to highlight uncommitted spaces inserted by
`electric-align'."
  :group 'electric-align)

;; + utilities

(defun electric-align--merge (l1 l2)
  "Merge two sorted lists of unique elements destructively."
  (cond ((or (null l1) (null l2))
         (or l1 l2))
        ((= (car l1) (car l2))
         (electric-align--merge (cdr l1) l2))
        ((< (car l1) (car l2))
         (let ((head l1) (last l1))
           (while (and (cdr last) (< (cadr last) (car l2)))
             (setq last (cdr last)))
           (setcdr last (electric-align--merge l2 (cdr last)))
           head))
        (t
         (electric-align--merge l2 l1))))

(defun electric-align--current-line-aligns ()
  "Return a pair of the form (LENGTH . ALIGNS) where LENGTH is
the length of this line and ALIGNS is the list of align
columns (smallest first) of this line above the current
column (including the current column). *THIS FUNCTION MAY MOVE
THE POINT inside the line*"
  (let ((eol (point-at-eol)) (aligns nil))
    (when (and (looking-back "[\s\t]\\|^") (looking-at "[^\s\t\n]"))
      (push (current-column) aligns))
    (while (search-forward-regexp "[\s\t][^\s\t]" eol t)
      (push (1- (current-column)) aligns))
    (cons (- eol (point-at-bol)) (nreverse aligns))))

(defun electric-align--find-align-columns (base-column &optional base-active)
  "Return (LINES-BACKWARD LINES-FORWARD . ALIGNS) above
BASE-COLUMN (including BASE-COLUMN). When BASE-ACTIVE is non-nil,
BASE-COLUMN must be also alignment."
  (let* ((active-aligns (when base-active (list base-column)))
         (active-lines-forward 0)
         (active-lines-backward 0)
         (max-column (if base-active base-column -1))
         (min-column (if base-active base-column 100000000))
         (process-line
          (lambda ()
            (let* ((current-aligns     (electric-align--current-line-aligns))
                   (current-max-column (pop current-aligns))
                   (pending-aligns     nil))
              (catch 'return
                ;; 0. check line emptiness
                (when (eolp) (throw 'return nil))
                ;; 1-1. check `max-column'
                (when current-aligns
                  (if (> (car current-aligns) max-column)
                      ;; move all `current-aligns' to `max-column'
                      (setq pending-aligns current-aligns
                            current-aligns nil)
                    ;; at least one align remains
                    (let ((ca current-aligns))
                      (while (and (cdr ca) (<= (cadr ca) max-column))
                        (setq ca (cdr ca)))
                      (setq pending-aligns (cdr ca))
                      (setcdr ca nil))))
                ;; 1-2. check `min-column'
                (when (and current-aligns (< (car current-aligns) min-column))
                  (setq min-column (car current-aligns))
                  (let ((ca current-aligns))
                    (while (and ca (< (car ca) min-column))
                      (push (pop ca) pending-aligns))
                    (setq current-aligns ca)))
                ;; (update max-column)
                (when (> current-max-column max-column)
                  (setq max-column current-max-column))
                ;; 2-3. check cell concatenation
                (let ((ca current-aligns) (last-ca-is-aa nil)
                      (aa active-aligns)  (last-aa-is-ca nil))
                  (while (or aa ca)
                    (cond ((null ca) ; 2. ca lacks an alignment => concatenation ?
                           (unless last-ca-is-aa (throw 'return nil))
                           (setq aa (cdr aa)))
                          ((null aa) ; 3. ca has an extra alignment => separation ?
                           (unless last-aa-is-ca (throw 'return nil))
                           (push (pop ca) pending-aligns))
                          ((< (car aa) (car ca)) ; 2. concatenation ?
                           (unless (and last-ca-is-aa (memql (car ca) aa))
                             (throw 'return nil))
                           (setq aa            (cdr aa)
                                 last-aa-is-ca nil))
                          ((< (car ca) (car aa)) ; 3. separation ?
                           (unless (and last-aa-is-ca (memql (car aa) ca))
                             (throw 'return nil))
                           (push (pop ca) pending-aligns)
                           (setq last-ca-is-aa nil))
                          (t
                           (setq aa (cdr aa) ca (cdr ca) last-aa-is-ca t last-ca-is-aa t)))))
                ;; add pending-aligns to active-aligns
                (setq active-aligns
                      (electric-align--merge
                       active-aligns (cl-remove-duplicates (sort pending-aligns '<))))
                t)))))
    (save-excursion
      (while (and (zerop (forward-line -1))
                  (move-to-column base-column)
                  (funcall process-line))
        (cl-incf active-lines-backward)))
    (save-excursion
      (while (and (zerop (forward-line 1))
                  (not (eobp)) ; (forward-line 1) returns 0 at the EOB
                  (move-to-column base-column)
                  (funcall process-line))
        (cl-incf active-lines-forward)))
    (cons active-lines-backward (cons active-lines-forward active-aligns))))

;; + the mode

(defvar electric-align--active-lines-backward nil)
(defvar electric-align--active-lines-forward nil)
(defvar electric-align--pending-aligns nil)
(defvar electric-align--overlays nil)

(defvar electric-align-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (kbd "SPC") 'electric-align-SPC)
    kmap)
  "Keymap for `electric-align-mode'.")

(defun electric-align--remove-overlays ()
  (mapc 'delete-overlay electric-align--overlays)
  (setq electric-align--overlays nil))

(defun electric-align--pre-command-hook ()
  (when (and electric-align--overlays (not (eq this-command 'electric-align-SPC)))
    (save-excursion
      (dolist (ov electric-align--overlays)
        (goto-char (overlay-start ov))
        (insert (overlay-get ov 'after-string))
        (delete-overlay ov)))
    (setq electric-align--pending-aligns        nil
          electric-align--active-lines-forward  nil
          electric-align--active-lines-backward nil
          electric-align--overlays              nil)))

(defun electric-align-SPC ()
  (interactive)
  (cond ((or (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
             (not (or (eq last-command this-command)
                      (memq last-command electric-align-shortcut-commands))))
         (insert " ")
         (setq electric-align--pending-aligns        nil
               electric-align--active-lines-forward  nil
               electric-align--active-lines-backward nil))
        ((not electric-align--active-lines-backward)
         (save-excursion
           (skip-chars-backward "\s\t")
           (skip-chars-backward "^\s\t\n")
           (let ((base-column (current-column)))
             (setq electric-align--pending-aligns
                   (electric-align--find-align-columns base-column (looking-at "[^\s\t\n]"))
                   electric-align--active-lines-backward (pop electric-align--pending-aligns)
                   electric-align--active-lines-forward  (pop electric-align--pending-aligns))
             (when (= base-column (car electric-align--pending-aligns))
               (pop electric-align--pending-aligns))))
         (electric-align-SPC))
        ((null electric-align--pending-aligns)
         (electric-align--remove-overlays)
         (insert " "))
        ((< (car electric-align--pending-aligns) (current-column))
         (electric-align--remove-overlays)
         (let* ((base-column (current-column))
                (align-fn
                 (lambda ()
                   (let ((pending-aligns electric-align--pending-aligns))
                     (while (and pending-aligns
                                 (< (car pending-aligns) base-column)
                                 (move-to-column (car pending-aligns))
                                 (not (and (memql (char-before) '(?\s ?\t))
                                           (not (memql (char-after) '(?\s \t))))))
                       (pop pending-aligns))
                     (when (< (car pending-aligns) base-column)
                       (push (make-overlay (point) (1- (point))) electric-align--overlays)
                       (overlay-put
                        (car electric-align--overlays) 'after-string
                        (propertize (make-string
                                     (- base-column (car pending-aligns)) ?\s)
                                    'face 'electric-align-face)))))))
           (save-excursion
             (dotimes (_ electric-align--active-lines-backward)
               (forward-line -1)
               (funcall align-fn)))
           (save-excursion
             (dotimes (_ electric-align--active-lines-forward)
               (forward-line 1)
               (funcall align-fn)))
           (pop electric-align--pending-aligns)))
        ((> (car electric-align--pending-aligns) (current-column))
         (electric-align--remove-overlays)
         (push (make-overlay (point) (1- (point))) electric-align--overlays)
         (overlay-put
          (car electric-align--overlays) 'after-string
          (propertize (make-string
                       (- (pop electric-align--pending-aligns) (current-column)) ?\s)
                      'face 'electric-align-face)))
        (t
         (pop electric-align--pending-aligns)
         (electric-align-SPC))))

(define-minor-mode electric-align-mode
  "Insert a certain amout of spaces at once to easily align
columns."
  :init-value nil
  :global     nil
  :keymap     electric-align-mode-map
  (if electric-align-mode
      (add-hook 'pre-command-hook 'electric-align--pre-command-hook nil t)
    (remove-hook 'pre-command-hook 'electric-align--pre-command-hook t)))

;; + provide

(provide 'electric-align)

;;; electric-align.el ends here
