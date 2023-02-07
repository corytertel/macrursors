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
(require 'mouse)
(require 'thingatpt)

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
    (define-key map (kbd "C-g") #'macrursors-early-quit)
    map))

(defun macrursors--inside-secondary-selection ()
  (and-let*
      ((buf (overlay-buffer mouse-secondary-overlay))
       ((eq buf (current-buffer))))
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
   (or overlays macrursors--overlays)))

(defun macrursors--mark-all-instances-of (string orig-point &optional end)
  (while (search-forward string end t)
    (unless (= (point) orig-point)
      (macrursors--add-overlay-at-point (point)))))

;;;###autoload
(defun macrursors-mark-all-instances-of ()
  (interactive)
  (if (use-region-p)
      (progn
	(let* ((region (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
               (orig-point (region-end))
               (selection-p (macrursors--inside-secondary-selection))
               (start (if selection-p
			 (overlay-start mouse-secondary-overlay)
                        0))
               (end (and selection-p
                         (overlay-end mouse-secondary-overlay))))
	  (goto-char orig-point)
          (save-excursion
	    (goto-char start)
	    (macrursors--mark-all-instances-of region orig-point end))
	  (deactivate-mark)
	  (macrursors-start)))
    (error "No region active")))

(defun macrursors--mark-next-instance-of (string &optional end)
  (let ((cursor-positions (macrursors--get-overlay-positions))
        (matched-p))
    (while (and (setq matched-p
                      (re-search-forward string end t 1))
                (member (point) cursor-positions)))
    (if (or (not matched-p)
            (> (point) (or end (point-max)))
            (member (point) cursor-positions))
        (message "No more matches.")
      (macrursors--add-overlay-at-point (point)))))

;;;###autoload
(defun macrursors-mark-next-instance-of ()
  (interactive)
  (when defining-kbd-macro (end-kbd-macro))
  (if (use-region-p)
      (progn
	(let ((region (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
	      (end (and (macrursors--inside-secondary-selection)
                        (overlay-end mouse-secondary-overlay))))
	  (goto-char (region-end))
          (save-excursion
	    (macrursors--mark-next-instance-of region end))
	  (macrursors-start)))
    (error "No region active")))

(defun macrursors--mark-previous-instance-of (string &optional end)
  (let ((cursor-positions (macrursors--get-overlay-positions))
        (matched))
    (while (and (setq matched
                      (re-search-forward string end t -1))
                (member (match-end 0) cursor-positions)))
    (if (or (not matched)
            (<= (point) (or end (point-min)))
            (member (match-end 0) cursor-positions))
        (message "No more matches.")
      (macrursors--add-overlay-at-point (match-end 0)))))

;;;###autoload
(defun macrursors-mark-previous-instance-of ()
  (interactive)
  (when defining-kbd-macro (end-kbd-macro))
  (if (use-region-p)
      (progn
	(let ((region (buffer-substring-no-properties
                       (region-beginning) (region-end)))
	      (end (if (macrursors--inside-secondary-selection)
		       (overlay-start mouse-secondary-overlay)
		     0)))
          (goto-char (region-end))
          (save-excursion
	    (goto-char (region-beginning))
	    (macrursors--mark-previous-instance-of region end))
	  (macrursors-start)))
    (error "No region active")))

(defun macrursors--forward-number ()
  (interactive)
  (let ((closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (when closest-ahead
      (push-mark)
      (goto-char closest-ahead))))

;;;###autoload
(defmacro macrursors--defun-mark-all (name thing func)
  `(defun ,name ()
     (interactive)
     (when mark-active (deactivate-mark))
     (let ((end-of-thing (cdr (bounds-of-thing-at-point ,thing))))
       (if end-of-thing
	   (goto-char end-of-thing)
	 (funcall ,func)))
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
		     (funcall ,func)
		     (not (= (point) curr)))
		   (<= (point) end))
	   (unless (= (point) orig-point)
	     (macrursors--add-overlay-at-point (point)))))
       (macrursors-start))))

;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-words
			    'word
			    #'forward-word)
;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-symbols
			    'symbol
			    (lambda ()
			      (call-interactively #'forward-symbol)))
;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-lists
			    'list
			    #'forward-list)
;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-sexps
			    'sexp
			    #'forward-sexp)
;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-defuns
			    'defun
			    #'end-of-defun)
;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-numbers
			    'number
			    #'macrursors--forward-number)
;;;###autoload
(macrursors--defun-mark-all macrursors-mark-all-sentences
			    'sentence
			    #'forward-sentence)
;; FIXME there is no forward-url function
;; (macrursors--defun-mark-all macrursors-mark-all-urls
;; 			    'url
;; 			    #'forward-url)
;; FIXME there is no forward-email function
;; (macrursors--defun-mark-all macrursors-mark-all-sexp
;; 			    'word
;; 			    #'forward-sexp)

;;;###autoload
(defun macrursors-mark-all-lines ()
  (interactive)
  (when mark-active (deactivate-mark))
  (let ((start (if (macrursors--inside-secondary-selection)
		   (overlay-start mouse-secondary-overlay)
		 0))
	(end (if (macrursors--inside-secondary-selection)
		 (overlay-end mouse-secondary-overlay)
	       (point-max)))
	(col (current-column)))
    (save-excursion
      (while (and (let ((curr (point)))
                    (forward-line -1)
		    (move-to-column col)
		    (not (= (point) curr)))
                  (>= (point) start))
	(macrursors--add-overlay-at-point (point))))
    (save-excursion
      (while (and (let ((curr (point)))
		    (forward-line 1)
		    (move-to-column col)
		    (not (= (point) curr)))
		  (<= (point) end))
	(macrursors--add-overlay-at-point (point))))
    (macrursors-start)))

;;;###autoload
(defun macrursors-mark-all-lines-or-instances ()
  "If a selection exists, mark all instances of the selection.
Else, mark all lines."
  (interactive)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (macrursors-mark-all-instances-of)
    (macrursors-mark-all-lines)))

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

(defun macrursors--apply-command (overlays cmd &optional args)
  (when overlays
    (save-excursion
      (dolist (ov overlays)
        (goto-char (overlay-start ov))
        (if (commandp cmd)
            (call-interactively cmd)
          (apply cmd args))))))

(defun macrursors-apply-command (cmd &rest args)
  (macrursors--wrap-collapse-undo
    (macrursors--apply-command
     macrursors--overlays
     cmd args)))

(defun macrursors--apply-kmacros ()
  "Apply kmacros."
  (interactive)
  (macrursors-apply-command #'execute-kbd-macro
                            last-kbd-macro))

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
(defun macrursors-early-quit ()
  "Used to quit out of recording the macro for the cursors."
  (interactive)
  (if (region-active-p)
      (progn
	(deactivate-mark)
	(when defining-kbd-macro
	  (end-kbd-macro)
	  (macrursors-start)))
    (when defining-kbd-macro (end-kbd-macro))
    (macrursors--remove-overlays)
    (macrursors-mode -1)))

(provide 'macrursors)
;;; macrursors.el ends here
