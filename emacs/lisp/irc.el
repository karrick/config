(eval-after-load "erc"
  (progn
    (defmacro erc-bouncer-connect (command server port nick ssl pass)
      "Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
      (fset command
	    `(lambda (arg)
	       (interactive "p")
	       (if (not (= 1 arg))
		   (call-interactively 'erc)
		 (let ((erc-connect-function ',(if ssl
						   'erc-open-ssl-stream
						 'open-network-stream)))
		   (erc :server ,server :port ,port :nick ,nick :password ,pass))))))
    (defun irc-reset-erc-track-mode ()
      "Clears out annoying erc-track-mode stuff for when we don't care.
Useful for when ChanServ restarts :P"
      (interactive)
      (setq erc-modified-channels-alist nil)
      (erc-modified-channels-update))
    (local-set-key (kbd "C-c r") 'irc-reset-erc-track-mode)
    ;; (setq erc-kill-buffer-on-part t
    ;;       erc-kill-queries-on-quit t
    ;;       erc-kill-server-buffer-on-quit t
    ;;       erc-server-auto-reconnect nil)
    (when t
      (require 'erc-join)
      (erc-autojoin-enable))
    (when t
      (require 'erc-services)
      (let ((path (expand-file-name "~/.ercpass.el")))
	(when (file-readable-p path)
	  (load path)))
      (setq erc-prompt-for-nickserv-password nil)
      (setq erc-autojoin-timing 'ident)
      (erc-services-mode 1))
    (when t
      (require 'erc-log)
      (setq erc-enable-logging 'erc-log-all-but-server-buffers
	    erc-log-channels-directory "~/logs"
	    erc-log-write-after-insert t
	    erc-log-write-after-send t
	    erc-save-buffer-on-part nil
	    erc-save-queries-on-quit nil)
      (erc-log-enable))
    (when t
      (require 'erc-match)
      (setq erc-current-nick-highlight-type 'nick)
      ;; (defface erc-keyword-erc-face '((t (:foreground "Orchid")))
      ;;   "ERC face to highlight occurances of the word erc"
      ;;   :group 'erc-faces)
      (add-to-list 'erc-log-matches-types-alist
		   '(current-nick . "ERC Keywords"))
      (erc-match-enable))
    (setq erc-hide-list (append erc-hide-list '("JOIN" "PART" "QUIT")))
    (when t
      (setq erc-track-exclude-server-buffer t
	    erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				      "324" "329" "332" "333" "353" "477")
	    erc-track-faces-normal-list '(erc-current-nick-face erc-keyword-face)
	    erc-track-priority-faces-only 'all
	    erc-track-use-faces t)
      (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
	(if (erc-query-buffer-p)
	    (setq ad-return-value (intern "erc-current-nick-face"))
	  ad-do-it))
      (defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
	(if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
	ad-do-it
	(if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))
      (when t            ; minimal distraction mode
	(require 'erc-track)
	(setq erc-format-query-as-channel-p t
	      erc-track-priority-faces-only 'all
	      erc-track-faces-priority-list '(erc-error-face
					      erc-current-nick-face
					      erc-keyword-face
					      erc-nick-msg-face
					      erc-direct-msg-face
					      erc-dangerous-host-face
					      erc-notice-face
					      erc-prompt-face)))
      (erc-track-enable))
    (let ((path (expand-file-name "~/.ercconfig.el")))
      (when (file-readable-p path)
	(load path)))))

(provide 'irc)
