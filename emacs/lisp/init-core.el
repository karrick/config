;;; init-core.el --- Core initialization  -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(defmacro init-time (label &rest body)
  "With label LABEL, evaluate BODY and report the elapsed time during init.
LABEL is evaluated once.  BODY is evaluated normally.
When `init-file-debug' is non-nil, display the elapsed time in the echo area."
  `(let ((start (current-time)))
	 ,@body
	 (when init-file-debug
	   (message "Init [%s]: %.3fs"
				,label
				(float-time (time-subtract (current-time) start))))))

(defmacro when-debug-init (&rest body)
  "Evaluate BODY only when `init-file-debug' is non-nil."
  `(when init-file-debug
	 ,@body))

(defmacro when-debug-message (format-string &rest args)
  "Display a message formatted with FORMAT-STRING and ARGS when debugging.
FORMAT-STRING is a format control string for `message'.
ARGS are the format arguments.
When `init-file-debug' is non-nil, display the message in the echo area."
  `(when init-file-debug
	 (let ((inhibit-message nil))
	   (message ,format-string ,@args))))

(init-time "CORE"
		   (if (and (functionp 'native-comp-available-p)
					(native-comp-available-p))
			   (message "native-compilation is available")
			 (message "native-compilation is *not* available"))

		   (when-debug-init
			(setq use-package-verbose t
				  use-package-expand-minimally nil
				  use-package-compute-statistics t))

		   ;; When running in daemon mode, change process directory to user
		   ;; home directory.
		   (when (or (daemonp)
					 (eq system-type 'windows-nt))
			 (cd (file-name-as-directory "~")))

		   (setq package-archives
				 '(("gnu" . "https://elpa.gnu.org/packages/")
				   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
				   ("melpa-stable" . "https://stable.melpa.org/packages/")
				   ("melpa" . "https://melpa.org/packages/")))

		   ;; When "cert" file in user-emacs-directory, presumably placed
		   ;; there as a symbolic link to a host-specific yet non-standard
		   ;; system cert file, then configure gnutls to trust it, before we
		   ;; attempt to contact package-archives from which packages would be
		   ;; downloaded.
		   (if (not (gnutls-available-p))
			   (message "GNU TLS is *not* available.")
			 (with-eval-after-load 'gnutls
			   (let ((cert (expand-file-name "cert" user-emacs-directory)))
				 (when (file-readable-p cert)
				   (message "Adding GNU TLS trustfiles %s" cert)
				   (add-to-list 'gnutls-trustfiles cert)))))

		   ;; On machines that do not have GNU version of `ls(1)` command, use
		   ;; a substitute written in Elisp.
		   (unless (memq system-type '(gnu gnu/linux gnu/kfreebsd))
			 (setq ls-lisp-use-insert-directory-program nil)
			 (require 'ls-lisp))

		   ;; On Windows prefer using `plink.exe` program for TRAMP
		   ;; connections.
		   (when (and (eq system-type 'windows-nt) (executable-find "plink"))
			 (setq tramp-default-method "plink")))

(init-time "PROCESS ENVIRONMENT"
		   ;; (require 'ksm-system)

		   ;; To prioritize access latency over availability, ensure that
		   ;; highly ephemeral cache data is stored on local machine rather
		   ;; than a home directory that is potentially mounted over a
		   ;; network. However, do place all cache files in a directory that
		   ;; makes it trivial to identify the owner and optionally remove all
		   ;; cache data.
		   ;;
		   ;; Platform-agnostic TMPDIR handling:
		   ;; - Respect explicit TMPDIR from the environment
		   ;; - Fall back to `temporary-file-directory`
		   ;; - Avoid appending username when already user-specific
		   (setenv
			"TMPDIR"
			(let* ((username (user-login-name (user-uid)))
				   (base (file-name-as-directory
						  (or (getenv "TMPDIR")
							  temporary-file-directory))))
			  (cond
			   ;; TMPDIR already appears user-specific (macOS, Windows, systemd)
			   ((string-match-p
				 (concat "/" (regexp-quote username) "/?$")
				 (directory-file-name base))
				(directory-file-name base))
			   ;; Otherwise create a per-user subdirectory
			   (t
				(expand-file-name username base)))))

		   (require 'env-set-when-null)

		   ;; XDG base dirs — meaningful on Unix; map to roughly corresponding
		   ;; locations on Windows.
		   ;;
		   ;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
		   (cond
			((eq system-type 'windows-nt)
			 (env-set-when-null "XDG_CACHE_HOME"
								(expand-file-name "AppData/Local/Temp"
												  (expand-file-name "~"))
								init-file-debug)
			 (env-set-when-null "XDG_CONFIG_HOME"
								(expand-file-name "AppData/Roaming"
												  (expand-file-name "~"))
								init-file-debug)
			 (env-set-when-null "XDG_DATA_HOME"
								(expand-file-name "AppData/Local"
												  (expand-file-name "~"))
								init-file-debug)
			 (env-set-when-null "XDG_STATE_HOME"
								(expand-file-name "AppData/Local/State"
												  (expand-file-name "~"))
								init-file-debug))
			(t
			 (env-set-when-null "XDG_CACHE_HOME" (getenv "TMPDIR") init-file-debug)
			 (env-set-when-null "XDG_CONFIG_HOME" (expand-file-name "~/.config") init-file-debug)
			 (env-set-when-null "XDG_DATA_HOME" (expand-file-name "~/.local/share") init-file-debug)
			 (env-set-when-null "XDG_STATE_HOME" (expand-file-name "~/.local/state") init-file-debug)))

		   (let* ((state (getenv "XDG_STATE_HOME"))
				  (history (file-name-concat state "history"))
				  (emacs (file-name-concat history "emacs")))
			 (when (and state (file-directory-p history))
			   (setenv "HISTFILE" emacs)))

		   ;; After XDG_DATA_HOME is set, can set PATH environment variable to
		   ;; any of the directories I typically use, provided that they
		   ;; exist.
		   (use-package paths)

		   ;; Elide `git(1)` paging capability for sub-processes:
		   (setenv "GIT_PAGER" "")

		   ;; In lieu of paging files, dump them to a buffer using `cat(1)`:
		   (let ((pager (or (executable-find "cat")
							(executable-find "type"))))
			 (when pager (setenv "PAGER" pager)))

		   ;; Make certain any child process knows to use `emacsclient(1)` as
		   ;; editor and can route file editing requests to this process.
		   (let ((cmd (executable-find "emacsclient")))
			 (when cmd
			   (setenv "EDITOR" cmd)
			   (setenv "VISUAL" cmd))))

(init-time "WRAP UP"
		   (fset 'yes-or-no-p 'y-or-n-p)
		   (prefer-coding-system 'utf-8)
		   (put 'narrow-to-region 'disabled nil)
		   ;; (desktop-save-mode 0)
		   ;; (fido-mode 1)
		   ;; (ido-mode 1)
		   ;; (vertico-mode)
		   )

(provide 'init-core)
;;; init-core.el ends here
