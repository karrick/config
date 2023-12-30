;;; setup-org-mode -- customizations for org-mode

;;; Commentary:

;; TODO: Investigate various ob-* packages to allow org-babel integration,
;; such as ob-async and ob-compile.

;;; Code:

(require 'empty-string)

(defun org-mode-begin-src (language)
  "Insert an 'org-mode' source block using LANGUAGE."
  (interactive "sLanguage: ")
  (if (empty-string-p language)
	  (insert (concat "#+BEGIN_SRC\n\n#+END_SRC\n"))
	(insert (concat "#+BEGIN_SRC " language "\n\n#+END_SRC\n")))
  (previous-line 2))

(use-package org
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c s" . org-mode-begin-src)
		 ("C-c l" . org-store-link))

  :custom

  (org-agenda-files '("~/gtd/inbox.org"
					  "~/gtd/projects.org"
					  "~/gtd/tickler.org")
					"TODO DOCUMENT")

  (org-capture-templates '(("t" "Todo [inbox]" entry
							(file+headline "~/gtd/inbox.org" "Inbox")
							"* TODO %i%?")
						   ;; ("p" "Project [projects]" entry
						   ;;  (file+headline "~/gtd/projects.org" "Projects")
						   ;;  "* TODO %i%?")
						   ("T" "Tickler" entry
							(file+headline "~/gtd/tickler.org" "Tickler")
							"* %i%? \n %U"))
						 "TODO DOCUMENT")

  (org-clock-mode-line-today 'today)
  (org-indent-mode t)

  (org-refile-targets '(("~/gtd/projects.org" :maxlevel . 3)
						("~/gtd/agendas.org" :level . 1)
						("~/gtd/inbox.org" :maxlevel . 2)
						("~/gtd/references.org" :level . 1)
						("~/gtd/someday.org" :level . 1)
						("~/gtd/tickler.org" :maxlevel . 2))
					  "TODO DOCUMENT")

  (org-todo-keywords '((sequence
						"MAYBE (m)"
						"TODO(t)"
						"STARTED(s)"
						"WAITING(w)"
						"PR(p)"
						"|"
						"MERGED(g)"
						"DONE(d)"
						"CANCELLED(c)"
						"DELEGATED(g)"
						))
					 "TODO DOCUMENT")

  :ensure t

  :hook
  (org-mode . visual-line-mode))

(provide 'setup-org-mode)

;;; setup-org-mode.el ends here
