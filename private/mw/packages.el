(defconst mw-packages '(org
                        anki-editor
                        epc))


(defun mw/post-init-org ()
  (use-package org
    :requires f
    :init
    (defconst wally-snap-dir (expand-file-name "~/.snap"))
    :hook
    (org-mode . (lambda () (smartparens-mode t)))
    :config
    (setq org-download-method 'directory
          org-download-image-dir (concat wally-journal-dir "assets/img/download")
          org-download-heading-lvl nil
          org-download-timestamp "%Y-%m-%d-%H-%M-%S"
          org-download-backend t
          )
    ;; org-protocol
    (org-link-set-parameters
     "org-protocol"
     :export (lambda (path desc backend)
               (cond
                ((eq 'html backend)
                 (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
                ((eq 'hugo backend)
                 (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
                ((eq 'md backend)
                 (format "<a href=\"org-protocol:%s\">%s</a>" path desc))
                )))
    ))


(defun mw/init-anki-editor ()
  (use-package anki-editor
    ))


(defun mw/init-epc ()
  (use-package epc
    :init
    (setq wally-epc nil)
    ))
