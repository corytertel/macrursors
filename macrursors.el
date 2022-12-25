;;; macrursors.el --- Macro visualizer -*- lexical-binding: t; -*-

;;; Commentary:
;; visualization as to where the macros will take place
;; and reduce the brainwork needed to create the macro.
;; just does it for you

;; 1. Select all like something (selection, word, sexp, line, etc)
;; 2. Place fake cursor at every point to indicate where the macros
;;    will be executed
;; 3. Automatically starts defining the macro.
;; 4. When done execute the macro at every point.

;; Heavily insired by meow's beacon-mode.
;; There are a few code snippets from meow's beacon-mode.
;; The faces were inspired by multiple-cursors.

;; TODO:
;; - Support for add cursor on click
;; - Cursors on lines directly above/below the point
;; - Documentation

;;; Code:

(require 'cl-lib)

(defvar-local macrursors--overlays nil)
(defvar-local macrursors--insert-enter-key nil)

(defgroup macrursors nil
  "Macrursors, a multi-edit tool for GNU Emacs."
  :group 'editing)

(defface macrursors-cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors."
  :group 'macrursors)

(defface macrursors-cursor-bar-face
  `((t (:height 1 :background ,(face-attribute 'cursor :background))))
  "The face used for fake cursors if the `cursor-type' is bar."
  :group 'macrursors)

(defcustom macrursors-match-cursor-style t
  "If non-nil, attempt to match the cursor style that the user has selected.
Namely, use vertical bars the user has configured Emacs to use that cursor.
If nil, just use standard rectangle cursors for all fake cursors.
In some modes/themes, the bar fake cursors are either not
rendered or shift text."
  :type '(boolean)
  :group 'macrursors)

(defface macrursors-region-face
  '((t :inherit region))
  "The face used for fake regions."
  :group 'macrursors)

(defcustom macrursors-preapply-command (lambda ())
  "The command to run before macros are applied.
Useful for optizationing the speed of the macro application.
A simple solution is to disable all minor modes that are purely
aesthetic in `macrursors-preapply-command'
and re-enable them in `macrursors-postapply-command'."
  :type 'function
  :group 'macrursors)

(defcustom macrursors-postapply-command (lambda ())
  "The command to run after macros are applied.
Useful for optizationing the speed of the macro application.
A simple solution is to disable all minor modes that are purely
aesthetic in `macrursors-preapply-command'
and re-enable them in `macrursors-postapply-command'."
  :type 'function
  :group 'macrursors)

(defcustom macrursors-apply-keys "C-;"
  "The bind to end and apply the macro recorded."
  :type 'key-sequence
  :group 'macrursors)

(define-minor-mode macrursors-mode
  "Minor mode for when macrursors in active."
  :lighter "macrursors"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd macrursors-apply-keys) #'macrursors-end)
    (define-key map (kbd "C-g") #'macrusors-early-quit)
    map))

(defun macrursors--second-sel-set-string (string)
  (cond
   ((macrursors--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (goto-char (overlay-start mouse-secondary-overlay))
      (delete-region (overlay-start mouse-secondary-overlay) (overlay-end mouse-secondary-overlay))
      (insert string)))
   ((markerp mouse-secondary-start)
    (with-current-buffer (marker-buffer mouse-secondary-start)
      (goto-char (marker-position mouse-secondary-start))
      (insert string)))))

(defun macrursors--second-sel-get-string ()
  (when (macrursors--second-sel-buffer)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (buffer-substring-no-properties
       (overlay-start mouse-secondary-overlay)
       (overlay-end mouse-secondary-overlay)))))

(defun macrursors--second-sel-buffer ()
  (and (overlayp mouse-secondary-overlay)
       (overlay-buffer mouse-secondary-overlay)))

;;;###autoload
(defun macrursors-grab ()
  "Create secondary selection or a marker if no region available."
  (interactive)
  (if (region-active-p)
      (secondary-selection-from-region)
    (progn
      (delete-overlay mouse-secondary-overlay)
      (setq mouse-secondary-start (make-marker))
      (move-marker mouse-secondary-start (point))))
  (deactivate-mark t))

;;;###autoload
(defun macrursors-swap-grab ()
  "Swap region and secondary selection."
  (interactive)
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (region-str (when (region-active-p) (buffer-substring-no-properties rbeg rend)))
         (sel-str (macrursors--second-sel-get-string))
         (next-marker (make-marker)))
    (when region-str (delete-region rbeg rend))
    (when sel-str (insert sel-str))
    (move-marker next-marker (point))
    (macrursors--second-sel-set-string (or region-str ""))
    (when (overlayp mouse-secondary-overlay)
      (delete-overlay mouse-secondary-overlay))
    (setq mouse-secondary-start next-marker)
    (deactivate-mark t)))

;;;###autoload
(defun macrursors-sync-grab ()
  "Sync secondary selection with current region."
  (interactive)
  (when (region-active-p)
    (let* ((rbeg (region-beginning))
           (rend (region-end))
           (region-str (buffer-substring-no-properties rbeg rend))
           (next-marker (make-marker)))
      (move-marker next-marker (point))
      (macrursors--second-sel-set-string region-str)
      (when (overlayp mouse-secondary-overlay)
	(delete-overlay mouse-secondary-overlay))
      (setq mouse-secondary-start next-marker)
      (deactivate-mark t))))

(defun macrursors--inside-secondary-selection ()
  (and
   (secondary-selection-exist-p)
   (< (overlay-start mouse-secondary-overlay)
      (overlay-end mouse-secondary-overlay))
   (<= (overlay-start mouse-secondary-overlay)
       (point)
       (overlay-end mouse-secondary-overlay))))

;; TODO maybe add support for multiple types of cursor types
(defun macrursors--add-overlay-at-point (pos)
  "Create an overlay to draw a fake cursor at POS."
  (let ((ov (make-overlay pos (1+ pos))))
    (overlay-put ov 'face
		 (cond
		  ((not macrursors-match-cursor-style) 'macrursors-cursor-face)
		  ((let ((cursor-type
			  (if (eq cursor-type t)
			      (frame-parameter nil 'cursor-type)
			    cursor-type)))
		     (or (eq cursor-type 'bar)
			 (and (listp cursor-type)
			      (eq (car cursor-type) 'bar)))) 'macrursors-cursor-bar-face)
		  (t 'macrursors-cursor-face)))
    (overlay-put ov 'macrursors-type 'cursor)
    (push ov macrursors--overlays)))

(defun macrursors--remove-overlays ()
  "Remove all overlays from current buffer."
  (mapc #'delete-overlay macrursors--overlays)
  (setq macrursors--overlays nil))

(defun macrursors--get-overlay-positions (&optional overlays)
  "Return a list with the position of all the cursors in `macrursors--overlays'.
If OVERLAYS in non-nil, return a list with the positions of OVERLAYS."
  (mapcar
   #'overlay-start
   (if overlays overlays macrursors--overlays)))

(defun macrursors--mark-all-instances-of (string orig-point &optional end)
  (let ((loc (re-search-forward string end t)))
    (when loc
      (unless (= loc orig-point) (macrursors--add-overlay-at-point loc))
      (macrursors--mark-all-instances-of string orig-point end))))

;;;###autoload
(defun macrursors-mark-all-instances-of ()
  (interactive)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
	(if (> (mark) (point))
	    (exchange-point-and-mark))
	(let ((region (buffer-substring-no-properties (mark) (point)))
	      (orig-point (point))
	      (start (if (macrursors--inside-secondary-selection)
			 (overlay-start mouse-secondary-overlay)
		       0))
	      (end (if (macrursors--inside-secondary-selection)
		       (overlay-end mouse-secondary-overlay)
		     nil)))
	  (save-excursion
	    (goto-char start)
	    (macrursors--mark-all-instances-of region orig-point end))
	  (deactivate-mark)
	  (macrursors-start)))
    (error "No region active")))

;;; FIXME
;; (defmacro macrursors--defun-mark-all (name func)
;;   `(defun ,name ()
;;      (interactive)
;;      (when mark-active (deactivate-mark))
;;      (let ((orig-point (point))
;; 	   (start (if (macrursors--inside-secondary-selection)
;; 		      (overlay-start mouse-secondary-overlay)
;; 		    0))
;; 	   (end (if (macrursors--inside-secondary-selection)
;; 		    (overlay-end mouse-secondary-overlay)
;; 		  (point-max))))
;;        (save-excursion
;; 	 (goto-char start)
;; 	 (while (and (let ((curr (point)))
;; 		       (funcall ,func)
;; 		       (not (= (point) curr)))
;; 		     (<= (point) end))
;; 	   (unless (= (point) orig-point)
;; 	     (macrursors--add-overlay-at-point (point)))))
;;        (macrursors-start))))

;; (macrursors--defun-mark-all macrursors-mark-all-words #'forward-word)
;; (macrursors--defun-mark-all macrursors-mark-all-symbols (lambda () (call-interactively #'forward-symbol)))
;; (macrursors--defun-mark-all macrursors-mark-all-lists #'forward-list)
;; (macrursors--defun-mark-all macrursors-mark-all-lines #'forward-line)

;;;###autoload
(defun macrursors-mark-all-words ()
  (interactive)
  (when mark-active (deactivate-mark))
  (let ((orig-point (point))
	(start (if (macrursors--inside-secondary-selection)
		   (overlay-start mouse-secondary-overlay)
		 0))
	(end (if (macrursors--inside-secondary-selection)
		 (overlay-end mouse-secondary-overlay)
	       (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (let ((curr (point)))
		    (funcall #'forward-word)
		    (not (= (point) curr)))
		  (<= (point) end))
	(unless (= (point) orig-point)
	  (macrursors--add-overlay-at-point (point)))))
    (macrursors-start)))

;;;###autoload
(defun macrursors-mark-all-symbols ()
  (interactive)
  (when mark-active (deactivate-mark))
  (let ((orig-point (point))
	(start (if (macrursors--inside-secondary-selection)
		   (overlay-start mouse-secondary-overlay)
		 0))
	(end (if (macrursors--inside-secondary-selection)
		 (overlay-end mouse-secondary-overlay)
	       (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (let ((curr (point)))
		    (funcall (lambda () (call-interactively #'forward-symbol)))
		    (not (= (point) curr)))
		  (<= (point) end))
	(unless (= (point) orig-point)
	  (macrursors--add-overlay-at-point (point)))))
    (macrursors-start)))

;;;###autoload
(defun macrursors-mark-all-lists ()
  (interactive)
  (when mark-active (deactivate-mark))
  (let ((orig-point (point))
	(start (if (macrursors--inside-secondary-selection)
		   (overlay-start mouse-secondary-overlay)
		 0))
	(end (if (macrursors--inside-secondary-selection)
		 (overlay-end mouse-secondary-overlay)
	       (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (let ((curr (point)))
		    (funcall #'forward-list)
		    (not (= (point) curr)))
		  (<= (point) end))
	(unless (= (point) orig-point)
	  (macrursors--add-overlay-at-point (point)))))
    (macrursors-start)))

;;;###autoload
(defun macrursors-mark-all-line ()
  (interactive)
  (when mark-active (deactivate-mark))
  (let ((orig-point (point))
	(start (if (macrursors--inside-secondary-selection)
		   (overlay-start mouse-secondary-overlay)
		 0))
	(end (if (macrursors--inside-secondary-selection)
		 (overlay-end mouse-secondary-overlay)
	       (point-max))))
    (save-excursion
      (goto-char start)
      (while (and (let ((curr (point)))
		    (funcall #'forward-line)
		    (not (= (point) curr)))
		  (<= (point) end))
	(unless (= (point) orig-point)
	  (macrursors--add-overlay-at-point (point)))))
    (macrursors-start)))

(defun macrursors-start ()
  "Start kmacro recording, apply to all cursors when terminate."
  (interactive)
  (macrursors-mode 1)
  (call-interactively #'kmacro-start-macro))

(defmacro macrursors--wrap-collapse-undo (&rest body)
  "Like `progn' but perform BODY with undo collapsed."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
        (success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
           ;; Don't truncate any undo data in the middle of this.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum)
           (,success nil))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             (prog1 ,(macroexp-progn body)
               (setq ,success t)))
         (if ,success
             (progn
               (accept-change-group ,handle)
               (undo-amalgamate-change-group ,handle))
           (cancel-change-group ,handle))))))

(defun macrursors--apply-command (overlays cmd)
  (when overlays
    (save-excursion
      (goto-char (overlay-start (car overlays)))
      (call-interactively cmd)
      (macrursors--apply-command (cdr overlays) cmd))))

(defun macrursors-apply-command (cmd)
  (macrursors--wrap-collapse-undo
    (macrursors--apply-command macrursors--overlays cmd)))

(defun macrursors--apply-kmacros ()
  "Apply kmacros."
  (interactive)
  (macrursors-apply-command #'kmacro-call-macro))

;; NOTE DOES NOT WORK WHEN CALLED FROM M-x!!!
;; FIXME applying time
;;;###autoload
(defun macrursors-end ()
  (interactive)
  (if (not defining-kbd-macro)
      (error "Not defining a macro")
    (end-kbd-macro)
    (funcall macrursors-preapply-command)
    (macrursors--apply-kmacros)
    (funcall macrursors-postapply-command)
    (macrursors--remove-overlays)
    (macrursors-mode -1)))

;;;###autoload
(defun macrusors-early-quit ()
  "Used to quit out of recording the macro for the cursors."
  (interactive)
  (when defining-kbd-macro (end-kbd-macro))
  (macrursors--remove-overlays)
  (macrursors-mode -1))

(provide 'macrursors)
;;; macrusors.el ends here
