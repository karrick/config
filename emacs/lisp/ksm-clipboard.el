;;; ksm-clipboard -- terminal Emacs system-clipboard bridge  -*- lexical-binding: t; -*-

;;; Commentary:

;; GUI Emacs already syncs the kill-ring with the window-system clipboard.  A
;; terminal frame (emacs -nw / emacsclient -t) does not: kills stay in the
;; kill-ring and never reach the system clipboard.  This installs
;; `interprogram-cut-function' and `interprogram-paste-function' so that, on a
;; terminal frame, kills and yanks bridge to the system clipboard.  Graphic
;; frames are left to Emacs's native handling.
;;
;; Two transports are supported, selected by `ksm-clipboard-method':
;;
;;   command  Pipe through an external program -- `ksm-clipboard-copy-command'
;;            (clipboard-copy) to copy, `ksm-clipboard-paste-command'
;;            (clipboard-paste) to paste.  Works only where the clipboard is
;;            local to Emacs (macOS, or X/Wayland with a reachable display).
;;
;;   osc52    Emit an OSC 52 terminal escape.  Reaches the clipboard of the
;;            outermost terminal even across SSH and tmux, so it works when
;;            Emacs runs on a remote host.  It is copy-only: terminals refuse
;;            the read direction for security, so paste defers to the kill-ring
;;            (use the terminal's own paste instead).  Through tmux it needs
;;            `set -g set-clipboard on'.
;;
;; The default, `auto', uses `command' when the clipboard is local and `osc52'
;; otherwise -- each kill is sent exactly once, never both ways.

;;; Code:

(defgroup ksm-clipboard nil
  "Bridge terminal Emacs kills and yanks to the system clipboard."
  :group 'killing)

(defcustom ksm-clipboard-method 'auto
  "Transport used to reach the system clipboard from a terminal frame.

`auto'     Use `command' when the clipboard is local to Emacs, else `osc52'.
`command'  Always pipe through the external copy and paste programs.
`osc52'    Always emit an OSC 52 escape (copy only; paste defers to kill-ring)."
  :type '(choice (const :tag "Automatic (local -> command, remote -> OSC 52)" auto)
				 (const :tag "External command (local clipboard only)" command)
				 (const :tag "OSC 52 escape (works over SSH and tmux; copy only)" osc52))
  :group 'ksm-clipboard)

(defcustom ksm-clipboard-copy-command "clipboard-copy"
  "Program that reads standard input and sets the system clipboard.
Used when the effective method is `command'."
  :type 'string
  :group 'ksm-clipboard)

(defcustom ksm-clipboard-paste-command "clipboard-paste"
  "Program that writes the system clipboard to standard output.
Used when the effective method is `command'."
  :type 'string
  :group 'ksm-clipboard)

(defcustom ksm-clipboard-osc52-max-length 74994
  "Maximum OSC 52 base64 payload length, in characters.
Selections whose encoding exceeds this are not sent, because terminals silently
drop over-long OSC 52 sequences.  The default, 74994, is the largest base64
payload that fits within the 100000-byte OSC 52 message limit common to
xterm-derived terminals (the value clipetty uses).  Raise it if your terminal
accepts larger sequences."
  :type 'integer
  :group 'ksm-clipboard)

(defvar ksm-clipboard--last-copy nil
  "Text most recently sent to the clipboard, to suppress echo on paste.")

(defvar ksm-clipboard--warned nil
  "Clipboard programs already reported missing this session.")

(defun ksm-clipboard--find (program)
  "Return the path to PROGRAM, warning once per session when it is absent."
  (or (executable-find program)
	  (progn
		(unless (member program ksm-clipboard--warned)
		  (push program ksm-clipboard--warned)
		  (display-warning 'ksm-clipboard
						   (format "Clipboard program `%s' not found." program)
						   :warning))
		nil)))

(defun ksm-clipboard--local-command (direction)
  "Return the clipboard program for DIRECTION, or nil when non-local.
DIRECTION is `copy' or `paste'.  The clipboard is considered local on
macOS, or on a system with a reachable X or Wayland display."
  (when (or (eq system-type 'darwin)
			(getenv "DISPLAY")
			(getenv "WAYLAND_DISPLAY"))
	(executable-find (if (eq direction 'copy)
						 ksm-clipboard-copy-command
					   ksm-clipboard-paste-command))))

(defun ksm-clipboard--method (direction)
  "Resolve the effective transport for DIRECTION: `command', `osc52', or nil.
DIRECTION is `copy' or `paste'."
  (pcase ksm-clipboard-method
	('command 'command)
	('osc52   (and (eq direction 'copy) 'osc52)) ; OSC 52 cannot read the clipboard
	('auto    (cond ((ksm-clipboard--local-command direction) 'command)
					((eq direction 'copy) 'osc52)
					(t nil)))
	(_ nil)))

(defun ksm-clipboard--osc52-set (text)
  "Set the system clipboard to TEXT with an OSC 52 escape sequence."
  (let ((b64 (base64-encode-string (encode-coding-string text 'utf-8) t)))
	(if (> (length b64) ksm-clipboard-osc52-max-length)
		(display-warning
		 'ksm-clipboard
		 (format "Selection too large for OSC 52 (%d > %d); not copied to clipboard."
				 (length b64) ksm-clipboard-osc52-max-length)
		 :warning)
	  ;; Through tmux this requires `set -g set-clipboard on'.
	  (send-string-to-terminal (concat "\e]52;c;" b64 "\a")))))

(defun ksm-clipboard--command-set (text)
  "Set the system clipboard to TEXT by piping it to `ksm-clipboard-copy-command'."
  (let ((cmd (ksm-clipboard--find ksm-clipboard-copy-command)))
	(when cmd
	  (let ((process-connection-type nil))	; use a pipe, not a pty
		(let ((proc (start-process "ksm-clipboard-copy" nil cmd)))
		  (process-send-string proc text)
		  (process-send-eof proc))))))

(defun ksm-clipboard--command-get ()
  "Return the system clipboard via `ksm-clipboard-paste-command', or nil."
  (let ((cmd (ksm-clipboard--find ksm-clipboard-paste-command)))
	(when cmd
	  (with-temp-buffer
		(let ((coding-system-for-read 'utf-8))
		  (when (zerop (call-process cmd nil t nil))
			(let ((text (buffer-string)))
			  (and (> (length text) 0) text))))))))

(defun ksm-clipboard-copy (text)
  "Send TEXT to the system clipboard.
On a graphic frame defer to Emacs's native selection handling; on a terminal
frame use the transport chosen by `ksm-clipboard-method'.  Intended as
`interprogram-cut-function'."
  (if (display-graphic-p)
	  (gui-select-text text)
	(pcase (ksm-clipboard--method 'copy)
	  ('command (ksm-clipboard--command-set text))
	  ('osc52   (ksm-clipboard--osc52-set text)))
	(setq ksm-clipboard--last-copy text)))

(defun ksm-clipboard-paste ()
  "Return the system clipboard contents for yank, or nil to use the kill-ring.
On a graphic frame defer to Emacs's native selection handling; on a terminal
frame use the transport chosen by `ksm-clipboard-method'.  Return nil when the
clipboard still holds the text we last copied, so our own kills are not echoed
back.  Intended as `interprogram-paste-function'."
  (if (display-graphic-p)
	  (gui-selection-value)
	(let ((text (and (eq (ksm-clipboard--method 'paste) 'command)
					 (ksm-clipboard--command-get))))
	  (unless (equal text ksm-clipboard--last-copy)
		text))))

(setq interprogram-cut-function   #'ksm-clipboard-copy)
(setq interprogram-paste-function #'ksm-clipboard-paste)

(provide 'ksm-clipboard)
;;; ksm-clipboard.el ends here
