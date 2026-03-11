;;; ksm-window --- provides handy window management functions
;;;
;;; Commentary:
;;;
;;; I frequently find myself in a situation where I have half a dozen Emacs
;;; windows in a frame, and I want to focus on just one of those windows for a
;;; few moments, and then later return my window layout to its present state.
;;; Emacs already has facilities for accomplishing such tasks, but I wanted a
;;; quick toggle function like as found in tmux(1) with "C-b z" pane toggling
;;; feature.
;;;
;;; Use:
;;;
;;; I put this file somewhere Emacs will find it, then added the below to my
;;; ~/.emacs/init.el file:
;;;
;;;    (require 'ksm-window)
;;;
;;;    ;; Deletes the current window. With universal prefix it kills a buffer
;;;    ;; and deletes the window.
;;;    (global-set-key (kbd "C-x 0") #'ksm-window-delete-window)
;;;
;;;    ;; Deletes other windows vertically. With universal prefix it deletes
;;;    ;; all other windows.
;;;    (global-set-key (kbd "C-x 1") #'ksm-window-delete-other-windows)
;;;
;;;    ;; Pops and restores window layout from stack.
;;;    (global-set-key (kbd "C-x -") #'ksm-window-zoom-out)
;;;
;;;    ;; Pushes window layout to a stack then deletes other windows.
;;;    (global-set-key (kbd "C-x +") #'ksm-window-zoom-in)
;;;
;;;    ;; Balances all windows.
;;;    (global-set-key (kbd "C-x =") #'balance-windows)
;;;
;;; Sometimes I want to name a particular window layout to be able to layer
;;; quickly return to it.  Perhaps I'm working on Project A and a colleague
;;; asks me to clarify how something in Project B works.  I save my window
;;; layout with a string name, such as "a", open up other files to answer the
;;; question, then can restore my window layout as it was before I loaded the
;;; other files. Note that this itself does not close the buffers that were
;;; opened, but merely restores the saved layout.
;;;
;;;    (global-set-key (kbd "C-x w l") #'ksm-window-layout-load)
;;;    (global-set-key (kbd "C-x w s") #'ksm-window-layout-save)
;;;    (global-set-key (kbd "C-x w x") #'ksm-window-layout-exchange)
;;;
;;; Code:

(require 'empty-string)

(defvar ksm-window--layout-hash (make-hash-table :test #'equal) "Hash table of saved window layouts.")

(defvar ksm-window--layout-name-current nil "Name of current window layout or nil if none.")

(defvar ksm-window--layout-name-history nil "Minibuffer history for window layout names.")

(defvar ksm-window--layout-name-previous nil "Name of previous window layout or nil if none.")

(defvar ksm-window--layout-zoom-stack nil "Stack of saved window layouts.")

(defun ksm-window-layout-drop (name)
  "Prompt user and drop window layout identified by NAME."
  (interactive
   (let ((layouts (ksm-window--layouts)))
	 (unless layouts
	   (user-error "No saved window layouts"))
	 (list
	  (ksm-window--read-layout-name "Drop layout name" nil t))))
  (let ((layout (gethash name ksm-window--layout-hash)))
	(unless layout
	  (user-error "Cannot drop unknown window layout: %s" name))
	(remhash name ksm-window--layout-hash)
	(when (equal ksm-window--layout-name-current name)
	  (setq ksm-window--layout-name-current nil))
	(when (equal ksm-window--layout-name-previous name)
	  (setq ksm-window--layout-name-previous nil))
	(message "Dropped window layout: %s" name)))

(defun ksm-window-layout-exchange (save load)
  "Prompt user to save and load window layout identified by SAVE and LOAD."
  (interactive
   (list
	(ksm-window--read-layout-name "Save layout name" ksm-window--layout-name-current)
	(ksm-window--read-layout-name "Load layout name" ksm-window--layout-name-previous)))
  (ksm-window-layout-save save)
  (ksm-window-layout-load load))

(defun ksm-window-layout-list ()
  "List saved window layouts."
  (interactive)
  (message "Saved window layouts: %s" (or (ksm-window--layouts) "No saved window layouts")))

(defun ksm-window-layout-load (name)
  "Prompt user and load window layout identified by NAME."
  (interactive
   (let ((layouts (ksm-window--layouts)))
	 (unless layouts
	   (user-error "No saved window layouts"))
	 (list
	  (ksm-window--read-layout-name "Load layout name" nil t))))
  (let ((layout (gethash name ksm-window--layout-hash)))
	(unless layout
	  (user-error "Cannot load unknown window layout: %s" name))
	(set-window-configuration (car layout))
	(goto-char (cdr layout))
	(unless (equal ksm-window--layout-name-current name)
	  (setq ksm-window--layout-name-previous ksm-window--layout-name-current)
	  (setq ksm-window--layout-name-current name))
	(message "Loaded window layout: %s" name)))

(defun ksm-window-layout-save (name)
  "Prompt user and save window layout identified by NAME."
  (interactive
   (list
	(ksm-window--read-layout-name "Save layout name" ksm-window--layout-name-current)))
  (when (empty-string-p name)
	(user-error "Cannot save unnamed window layout"))
  (puthash name
		   (cons (current-window-configuration) (point-marker))
		   ksm-window--layout-hash)
  (unless (equal ksm-window--layout-name-current name)
	(setq ksm-window--layout-name-previous ksm-window--layout-name-current)
	(setq ksm-window--layout-name-current name))
  (delete-other-frames)
  (message "Saved window layout: %s" name))

(defun ksm-window-delete-other-windows (&optional vertical)
  "Delete all other windows.  With VERTICAL, delete other windows vertically."
  (interactive "P")
  (if vertical
	  (delete-other-windows-vertically)
	(delete-other-windows)))

(defun ksm-window-delete-window (&optional kill)
  "Delete the current window.  With KILL, kill the current buffer."
  (interactive "P")
  (if kill
	  (kill-buffer))
  (delete-window))

(defun ksm-window-delete-window-above ()
  "Delete the window above the current window."
  (interactive)
  (delete-window (window-in-direction 'above)))

(defun ksm-window-delete-window-below ()
  "Delete the window below the current window."
  (interactive)
  (delete-window (window-in-direction 'below)))

(defun ksm-window-delete-window-left ()
  "Delete the window to the left of the current window."
  (interactive)
  (delete-window (window-in-direction 'left)))

(defun ksm-window-delete-window-right ()
  "Delete the window to the right of the current window."
  (interactive)
  (delete-window (window-in-direction 'right)))

(defun ksm-window-lock ()
  "Mark the current window as dedicated for its current buffer."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) t))

(defun ksm-window-other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun ksm-window-unlock ()
  "Unmark the current window as dedicated for its current buffer."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) nil))

(defun ksm-window-zoom-in ()
  "Push window layout on stack then expand current window to entire frame."
  (interactive)
  (push (cons (current-window-configuration) (point-marker)) ksm-window--layout-zoom-stack)
  (delete-other-windows)
  (message "Zoomed in to %s" (current-buffer)))

(defun ksm-window-zoom-out ()
  "Pop a window layout off the stack and load it."
  (interactive)
  (let ((layout (pop ksm-window--layout-zoom-stack)))
	(if (not layout)
		(message "No window layouts on the stack")
	  (set-window-configuration (car layout))
	  (goto-char (cdr layout))
	  (message "Zoomed out"))))

(defun ksm-window--layouts ()
  "Return sorted list of saved window layouts."
  (let ((keys '()))
	(maphash (lambda (k _v) (push k keys))
			 ksm-window--layout-hash)
	(sort keys #'string-lessp)))

(defun ksm-window--read-layout-name (prompt &optional default require-match)
  "Read a window layout name from the minibuffer using completion.

Display PROMPT in the minibuffer.  Support a DEFAULT value.  When
REQUIRE-MATCH is non-nil, require that the response match one of the
values from `ksm-window--layouts'."
  (let* ((layouts (ksm-window--layouts))
		 (default (if (and require-match
						   default
						   (not (member default layouts)))
					  nil
					default))
		 (full-prompt
		  (if default
			  (format "%s (default: %s): " prompt default)
			(format "%s: " prompt)))
		 (history
		  (if require-match
			  (seq-filter (lambda (name)
							(member name layouts))
						  ksm-window--layout-name-history)
			ksm-window--layout-name-history))
		 (history-var (make-symbol "ksm-window--layout-name-history"))
		 (name nil))
	(set history-var history)
	(setq name
		  (completing-read
		   full-prompt
		   layouts
		   nil
		   require-match
		   nil
		   history-var
		   default))
	(if (empty-string-p name) default name)))

(provide 'ksm-window)
;;; ksm-window.el ends here
