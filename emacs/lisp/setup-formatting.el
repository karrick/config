;;; setup-formatting.el -- Buffer formatting configuratin  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar-local my-format-on-save--fallback nil)
(defvar-local my-format-on-save--context nil)

(define-minor-mode my-format-on-save-mode
  "Toggle format-on-save for the current buffer.

When enabled, the buffer is formatted before saving using Eglot when
available, or a fallback formatter if defined."
  :lighter " Fmt"
  :group 'formatter
  (if my-format-on-save-mode
	  (add-hook 'before-save-hook #'my-format-on-save--run nil t)
	(remove-hook 'before-save-hook #'my-format-on-save--run t)))

(defun my-format-on-save--run ()
  "Internal before-save hook for `my-format-on-save-mode."
  (my-format-on-save
   my-format-on-save--fallback
   my-format-on-save--context))

(defun my-enable-format-on-save (&optional fallback context)
  "Enable format-on-save in the current buffer.

FALLBACK is a formatting function to invoke when Eglot is not managing
the buffer.

CONTEXT is a short string for warning messages."
  (setq my-format-on-save--fallback fallback
		my-format-on-save--context context)
  (my-format-on-save-mode 1))

(defun my-eglot-format-and-organize-imports (&optional context)
  "Organize imports and format the current buffer using Eglot.

This uses LSP code actions for import
organization (\"source.organizeImports\"), followed by standard document
formatting.

Errors are reported to the *Warnings* buffer, but never prevent saving.

CONTEXT is an optional short string used in warning messages."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
	;; Organize imports
	(condition-case err
		(progn
		  (eglot-code-actions nil nil "source.organizeImports")
		  (when t
			(message "%s organized via eglot: %s"
					 (if context (format "%s imports" context) "Imports")
					 (buffer-file-name))))
	  (error
	   (display-warning
		'eglot
		(format "Organize imports failed: %s"
				(error-message-string err))
		:warning)))

	;; Format buffer
	(condition-case err
		(progn
		  (eglot-format-buffer)
		  (when t
			(message "%s via eglot: %s"
					 (if context (format "%s formatted" context) "Formatted")
					 (buffer-file-name))))
	  (error
	   (display-warning
		'eglot
		(format "Formatting failed: %s"
				(error-message-string err))
		:warning)))))

(defun my-format-on-save (fallback &optional context)
  "Format current buffer on save.

If the buffer is managed by Eglot, use `my-eglot-format-and-organize-imports`.

Otherwise, call FALLBACK (a function of no arguments).

Errors are reported to the *Warnings* buffer and never prevent saving.

CONTEXT is an optional short string used in warning messages."
  (cond
   ;; Prefer LSP if one has been configured via Eglot.
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
	(my-eglot-format-and-organize-imports context))

   ;; When LSP not managing buffer and a fallback is defined, invoke it.
   ((functionp fallback)
	(condition-case err
		(progn
		  (funcall fallback)
		  (when t
			(message "%s using fallback: %s"
					 (if context (format "%s formatted" context) "Formatted")
					 (buffer-file-name))))
	  (error
	   (display-warning
		'formatter
		(format "%s formatting failed: %s"
				(or context "Fallback")
				(error-message-string err))
		:warning))))

   ;; Nothing available
   (t
	(display-warning
	 'formatter
	 (format "No formatter available%s"
			 (if context (format " (%s)" context) ""))
	 :warning))))

(require 'cl-lib)

(cl-defmacro my-format-on-save! (fallback &optional context)
  "Enable format-on-save in the current buffer.
FALLBACK is a function (or nil) used when Eglot is not managing the buffer.

CONTEXT is a short string used in warning messages."
  (declare (indent 1))
  `(add-hook 'before-save-hook
			 (my-enable-format-on-save ,fallback ,context)
			 nil t))

(provide 'setup-formatting)
;;; setup-formatting.el ends here
