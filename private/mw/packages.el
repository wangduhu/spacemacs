(defconst mw-packages '(org))


(defun mw/post-init-org ()
  (use-package org
    :requires f
    :init
    (setq wally-journal-dir "~/Wally/Journal/"
          wally-note-dir (concat wally-journal-dir "core/")
          wally-gtd-dir (concat wally-journal-dir "gtd/")
          wally-data-dir (concat wally-journal-dir "data/")
          wally-note-file (concat wally-gtd-dir "journal.org"))
    (defconst wally-snap-dir (expand-file-name "~/.snap"))
    :hook
    (org-mode . (lambda () (smartparens-mode t)))))
