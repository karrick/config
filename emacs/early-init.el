;;; early-init.el --- Early initialization  -*- mode: emacs-lisp -*-

;; Enable full init debugging when requested via --debug-init

;;; Code:

(setq debug-on-error t)

(when init-file-debug
  (setq debug-on-error t
		;; debug-on-quit t
		))

;; Reduce UI churn during startup
(setq inhibit-startup-screen t
	  inhibit-startup-message t
	  inhibit-splash-screen t
	  inhibit-redisplay t
	  inhibit-message t)

;; Restore normal behavior after startup
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq inhibit-redisplay nil
				  inhibit-message nil)
			(redisplay)))

;; Package.el should not initialize itself
;; (setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
