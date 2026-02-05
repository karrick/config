;;; setup-formatting.el -- Buffer formatting configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides a triple-mode format-on-save system:
;;
;;   eglot    → format only if buffer is eglot-managed
;;   fallback → format only using explicit fallback formatter
;;   disabled → no formatting
;;
;; A single toggle command cycles between these states.
;; Unavailable formatter warnings are emitted once per reason per buffer
;; and reset when the formatting mode changes.

;;; Code:

(require 'cl-lib)

;; -------------------------------------------------------------------
;; Buffer-local state
;; -------------------------------------------------------------------

(defvar-local my-format-on-save--fallback nil
  "Fallback formatter function for the current buffer.")

(defvar-local my-format-on-save--context nil
  "Short context string used in messages and warnings.")

(defvar-local my-format-on-save--state 'eglot
  "Current formatting state.
One of: `eglot`, `fallback`, `disabled`.")

(defvar-local my-format-on-save--last-unavailable nil
  "Last reason string for which a `Cannot format' message was emitted.")

;; -------------------------------------------------------------------
;; Modeline
;; -------------------------------------------------------------------

(defun my-format-on-save--lighter ()
  (pcase my-format-on-save--state
	('eglot    " Fmt[E]")
	('fallback " Fmt[F]")
	('disabled " Fmt[-]")))

;; -------------------------------------------------------------------
;; Minor mode
;; -------------------------------------------------------------------

(define-minor-mode my-format-on-save-mode
  "Toggle format-on-save for the current buffer."
  :lighter (:eval (my-format-on-save--lighter))
  :group 'formatter
  (if my-format-on-save-mode
	  (add-hook 'before-save-hook #'my-format-on-save--run nil t)
	(remove-hook 'before-save-hook #'my-format-on-save--run t)))

;; -------------------------------------------------------------------
;; Messaging helpers
;; -------------------------------------------------------------------

(defun my-format-on-save--unavailable (reason)
  "Emit a `Cannot format' message once per REASON per buffer."
  (unless (equal reason my-format-on-save--last-unavailable)
	(setq my-format-on-save--last-unavailable reason)
	(message "Cannot format (%s): %s"
			 reason
			 (buffer-name))))

;; -------------------------------------------------------------------
;; Dispatcher (triple-mode aware, strict semantics)
;; -------------------------------------------------------------------

(defun my-format-on-save--run ()
  "Internal before-save hook for `my-format-on-save-mode`."
  (pcase my-format-on-save--state

	;; Disabled: do nothing
	('disabled
	 nil)

	;; Fallback-only mode
	('fallback
	 (if (functionp my-format-on-save--fallback)
		 (my-format-on-save
		  my-format-on-save--fallback
		  my-format-on-save--context)
	   (my-format-on-save--unavailable "no fallback formatter")))

	;; Eglot-only mode
	('eglot
	 (if (and (fboundp 'eglot-managed-p)
			  (eglot-managed-p))
		 (my-eglot-format-and-organize-imports
		  my-format-on-save--context)
	   (my-format-on-save--unavailable "eglot not managing buffer")))))

;; -------------------------------------------------------------------
;; Public enable helper
;; -------------------------------------------------------------------

(defun my-enable-format-on-save (&optional fallback context)
  "Enable format-on-save in the current buffer.

Defaults to eglot mode when available."
  (setq my-format-on-save--fallback fallback
		my-format-on-save--context context
		my-format-on-save--state 'eglot
		my-format-on-save--last-unavailable nil)
  (my-format-on-save-mode 1))

;; -------------------------------------------------------------------
;; Toggle command
;; -------------------------------------------------------------------

(defun my-toggle-format-on-save ()
  "Cycle formatting mode for the current buffer.

eglot → fallback → disabled → eglot"
  (interactive)

  ;; Reset suppression when user explicitly changes mode
  (setq my-format-on-save--last-unavailable nil)

  (setq my-format-on-save--state
		(pcase my-format-on-save--state
		  ('eglot    'fallback)
		  ('fallback 'disabled)
		  (_         'eglot)))

  (unless my-format-on-save-mode
	(my-format-on-save-mode 1))

  (message "Format on save: %s"
		   (pcase my-format-on-save--state
			 ('eglot    "eglot (preferred)")
			 ('fallback "fallback only")
			 ('disabled "disabled"))))

(global-set-key (kbd "C-c f") #'my-toggle-format-on-save)

;; -------------------------------------------------------------------
;; Eglot formatter
;; -------------------------------------------------------------------

(defun my-eglot-format-and-organize-imports (&optional context)
  "Organize imports and format the current buffer using Eglot.

Uses LSP code actions for import organization followed by document
formatting. Errors are reported to the *Warnings* buffer and never
prevent saving."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))

	;; Organize imports
	(condition-case err
		(eglot-code-actions nil nil "source.organizeImports")
	  (error
	   (display-warning
		'eglot
		(format "Organize imports failed: %s"
				(error-message-string err))
		:warning)))

	;; Format buffer
	(condition-case err
		(eglot-format-buffer)
	  (error
	   (display-warning
		'eglot
		(format "Formatting failed: %s"
				(error-message-string err))
		:warning)))))

;; -------------------------------------------------------------------
;; Generic formatter helper (used for fallback)
;; -------------------------------------------------------------------

(defun my-format-on-save (fallback &optional context)
  "Format current buffer using FALLBACK.

Errors are reported to the *Warnings* buffer and never prevent saving."
  (when (functionp fallback)
	(condition-case err
		(funcall fallback)
	  (error
	   (display-warning
		'formatter
		(format "%s formatting failed: %s"
				(or context "Fallback")
				(error-message-string err))
		:warning)))))

;; -------------------------------------------------------------------
;; Convenience macro
;; -------------------------------------------------------------------

(cl-defmacro my-format-on-save! (fallback &optional context)
  "Enable format-on-save in the current buffer.
FALLBACK is used in fallback mode.
CONTEXT is a short string used in messages."
  (declare (indent 1))
  `(my-enable-format-on-save ,fallback ,context))

(provide 'setup-formatting)
;;; setup-formatting.el ends here
