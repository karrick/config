;;; ksm-window --- provides handy window management functions
;;;
;;; Commentary:
;;;
;;; I frequently find myself in a situation where I have half a dozen
;;; Emacs windows in a frame, and I want to focus on just one of those
;;; windows for a few moments, and then later return my window state
;;; to its present configuration.  Emacs already has facilities for
;;; accomplishing such tasks, but I wanted a quick toggle function
;;; like tmux' "C-b z" pane toggling feature.
;;;
;;; Use:
;;;
;;; I put this file somewhere Emacs will find it, then added the below
;;; to my ~/.emacs/init.el file.
;;;
;;;    (require 'ksm-window)
;;;
;;;    ;; Deletes the current window. With universal prefix it kills a
;;;    ;; buffer and deletes the window.
;;;    (global-set-key (kbd "C-x 0") #'ksm/delete-window)
;;;
;;;    ;; Deletes other windows vertically. With universal prefix it
;;;    ;; deletes all other windows.
;;;    (global-set-key (kbd "C-x 1") #'ksm/delete-other-windows)
;;;
;;;    ;; Pops and restores window configuration from stack.
;;;    (global-set-key (kbd "C-x -") #'ksm-window-zoom-out)
;;;
;;;    ;; Pushes window configuration to stack and deletes other
;;;    ;; windows.
;;;    (global-set-key (kbd "C-x +") #'ksm-window-zoom-in)
;;;
;;;    ;; Balances all windows.
;;;    (global-set-key (kbd "C-x =") #'balance-windows)
;;;
;;; Sometimes I want to name a particular window configuration and
;;; quickly return to it. Perhaps I'm working on Project A and a
;;; colleague asks me to clarify how something in Project B works.  I
;;; save my window configuration with a string name, such as "a", open
;;; up other files to answer the question, then can restore my window
;;; configuration as it was before I loaded other files.
;;;
;;;    (global-set-key (kbd "C-x p") #'ksm-window-config-save)
;;;    (global-set-key (kbd "C-x j") #'ksm-window-config-restore)
;;;
;;; Code:

(require 'empty-string)

(defun ksm/delete-other-windows (&optional vertical)
  "Delete all other windows; with VERTICAL delete other windows vertically."
  (interactive "P")
  (if vertical
	  (delete-other-windows-vertically)
	(delete-other-windows)))

(defun ksm/delete-window (&optional kill)
  "Delete the current window.  With optional KILL, kill the current buffer."
  (interactive "P")
  (if kill
	  (kill-buffer))
  (delete-window))

(defun ksm/delete-window-above ()
  "Delete the window above the current window."
  (interactive)
  (delete-window (window-in-direction 'above)))

(defun ksm/delete-window-below ()
  "Delete the window below the current window."
  (interactive)
  (delete-window (window-in-direction 'below)))

(defun ksm/delete-window-left ()
  "Delete the window to the left of the current window."
  (interactive)
  (delete-window (window-in-direction 'left)))

(defun ksm/delete-window-right ()
  "Delete the window to the right of the current window."
  (interactive)
  (delete-window (window-in-direction 'right)))

(defvar ksm-window-configuration-name nil "Name of current window configuration or nil if none.")

(defvar ksm-window-configuration-name-previous nil "Name of previous window configuration or nil if none.")

(defvar ksm-window-configurations-hash (make-hash-table :test #'equal) "Hash table of saved window configurations.")

(defun ksm-window--configs ()
  "Return list of saved window configurations."
  (interactive)
  (let ((keys '()))
	(maphash (lambda (k _v) (push k keys)) ksm-window-configurations-hash)
	keys))

(defun ksm-window-config-drop (name)
  "Prompt user and drop window configuration identified by NAME."
  (interactive
   (list
	(read-string (format "Drop which layout %s: " (ksm-window--configs)))))
  (cond
   ((gethash name ksm-window-configurations-hash)
	(remhash name ksm-window-configurations-hash)
	(message "dropped window configuration: %s" name))
   (t (message "cannot drop unknown window configuration: %s" name))))

(defun ksm-window-config-list ()
  "List saved window configurations."
  (interactive)
  (message "saved window configurations: %s" (ksm-window--configs)))

(defun ksm-window-config-load (name)
  "Prompt user and load window configuration identified by NAME."
  (interactive
   (list
	(read-string (format "Load which layout %s: " (ksm-window--configs)) (or ksm-window-configuration-name ksm-window-configuration-name-previous))))
  (if (empty-string-p name)
	  (message "cannot load without layout name")
	(let ((config (gethash name ksm-window-configurations-hash)))
	  (cond
	   (config (set-window-configuration (car config))
			   (goto-char (cdr config))
			   (unless (equal ksm-window-configuration-name name)
				 (setq ksm-window-configuration-name-previous ksm-window-configuration-name)
				 (setq ksm-window-configuration-name name))
			   (message "loaded window configuration: %s" name))
	   (t (message "cannot load unknown window configuration: %s" name))))))

(defun ksm-window-config-save (name)
  "Prompt user and save window configuration identified by NAME."
  (interactive
   (list
	(read-string (format "Save which layout %s: " (ksm-window--configs)) ksm-window-configuration-name)))
  (if (empty-string-p name)
	  (message "cannot save without layout name")
	(puthash name
			 (cons (current-window-configuration) (point-marker))
			 ksm-window-configurations-hash)
	(unless (equal ksm-window-configuration-name name)
	  (setq ksm-window-configuration-name-previous ksm-window-configuration-name)
	  (setq ksm-window-configuration-name name))
	(delete-other-frames)
	(message "saved window configuration: %s" name)))

(defun ksm-window-config-swap (save load)
  "Prompt user to save and load window configuration identified by SAVE and LOAD."
  (interactive
   (list
	(read-string (format "Save which layout %s: " (ksm-window--configs)) ksm-window-configuration-name)
	(read-string (format "Load which layout %s: " (ksm-window--configs)) ksm-window-configuration-name-previous)))
  (ksm-window-config-save save)
  (ksm-window-config-load load))

(defun ksm-window-lock ()
  "Mark the current window as dedicated for its current buffer."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) t))

(defun ksm-window-unlock ()
  "Unmark the current window as dedicated for its current buffer."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) nil))

(defvar ksm-window-configurations-list nil "Stack of saved window configurations.")

(defun ksm-window-zoom-in ()
  "Push window config on stack then expand current window to entire frame."
  (interactive)
  (push (cons (current-window-configuration) (point-marker)) ksm-window-configurations-list)
  (delete-other-windows)
  (message "zoomed in to %s" (current-buffer)))

(defun ksm-window-zoom-out ()
  "Pop a window configuration off the stack and load it."
  (interactive)
  (let ((config (pop ksm-window-configurations-list)))
	(cond
	 (config (set-window-configuration (car config))
			 (goto-char (cdr config))
			 (message "zoomed out"))
	 (t (message "no more window configurations on the stack")))))

(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(provide 'ksm-window)

;;; ksm-window.el ends here
