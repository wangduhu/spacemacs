(defconst mw-packages '(org
                        anki-editor
                        annotate
                        cnfonts
                        dirvish
                        easy-kill
                        epc
                        highlight-function-calls
                        ;; nlinum-hl
                        ;; org-media-note
                        org-noter
                        projectile
                        sr-speedbar
                        ssh-deploy
                        yasnippet
                        ))


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
    (require 'org-tempo)
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


(defun mw/init-annotate ()
  (use-package annotate
    :after org
    :config
    (setq annotate-file (concat wally-journal-dir "data/annotations"))
    ))


(defun mw/init-cnfonts ()
  (use-package cnfonts
    :config
    (cnfonts-enable)))

(defun mw/init-dirvish ()
  (use-package dirvish
    ))



(defun mw/init-easy-kill ()
  (use-package easy-kill
    :bind
    ([remap kill-ring-save] . easy-kill)))


(defun mw/init-epc ()
  (use-package epc
    :init
    (setq wally-epc nil)
    ))


(defun mw/init-highlight-function-calls ()
  (use-package highlight-function-calls))


(defun mw/init-nlinum-hl ()
  (use-package nlinum-hl
    :hook
    (prog-mode . (lambda ()
                   (nlinum-mode nil)))
    ))


(defun mw/init-org-media-note ()
  (use-package org-media-note
    ;; :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
    :hook (org-mode .  org-media-note-mode)
    :bind (("M-v" . org-media-note-hydra/body))  ;; Main entrance
    :config
    (setq org-media-note-screenshot-image-dir "~/Wally/Journal/assets/mpv"))
  )


(defun mw/init-org-noter ()
  (use-package org-noter
    :config
    (setq org-noter-always-create-frame nil)
    ))


(defun mw/post-init-projectile ()
  (use-package projectile
    :config
    (setq projectile-enable-caching t
          projectile-project-root-files-bottom-up '(".projectile")
          projectile-indexing-method 'native)
    ))


(defun mw/init-sr-speedbar ()
  (use-package sr-speedbar
    :init
    (setq sr-speedbar-right-side  nil
          speedbar-use-images nil
          speedbar-tag-hierarchy-method nil
          speedbar-update-flag nil
          sr-speedbar-auto-refresh nil
          )
    :config
    (speedbar-disable-update)
    ))


(defun mw/init-ssh-deploy ()
  (use-package ssh-deploy
    :bind
    (:map ssh-deploy-prefix-map
          ("f" . ssh-deploy-upload-handler-forced))))


(defun mw/post-init-yasnippet ()
  (use-package sr-speedbar
    :init
    (setq yas-snippet-dirs (list "~/.emacs.d/private/snippets"))
    :config
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
    ))
