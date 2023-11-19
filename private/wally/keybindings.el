(defconst __global__ nil)
;; (global-set-key [f6] 'spaceline-toggle-org-pomodoro)
;; (global-set-key [f5] '(lambda()
;;                         (interactive)
;;                         (delete-other-frames)
;;                         (jump-to-register 119)))
(global-set-key [f7] 'wally/dice)
(global-set-key [f9] 'wally/macro-join-next-line)
(global-set-key "\C-\M-y" 'yas-expand)
(global-set-key "\C-\M-k" 'kill-sexp)

;; elfeed-mark-all-as-read

(spacemacs/set-leader-keys
  "'" '(lambda ()
         (interactive)
         (let ((buffer "*eshell*"))
           (if (get-buffer buffer)
               (switch-to-buffer buffer)
             (eshell))))
  "Kb" 'kmacro-bind-to-key
  "Ki" 'insert-kbd-macro
  "Kn" 'kmacro-name-last-macro
  "aj" 'helm-bookmarks
  "al" 'delete-blank-lines
  "ab" 'helm-bookmarks
  "am" 'bookmark-set
  "an" '(lambda()
          (interactive)
          (annotate-annotate)
          (annotate-save-annotations))
  "aN" 'wally/org-export-note
  "bB" 'mark-whole-buffer
  "cs" 'sp-copy-sexp
  "cz" 'ssh-deploy-prefix-map
  "ei" 'edit-indirect-region
  "ec" 'wally/copyq-read-items
  "ek" 'wally/kill-word-at-point
  "fa" 'find-file-other-window
  "fd" 'dired-at-point
  "fq" 'read-only-mode
  "fw" 'ido-write-file
  "gg" 'goto-line
  "gI" 'wally/helm-gitignore
  "hv" 'describe-variable
  "hR" 'helm-mark-ring
  "hr" 'helm-global-mark-ring
  "ja" 'beginning-of-defun
  "jA" 'sp-beginning-of-sexp
  "je" 'end-of-defun
  "jE" 'sp-end-of-sexp
  "jf" 'xref-find-definitions
  "jp" 'xref-pop-marker-stack
  "jr" 'xref-find-references
  "KR" 'wally/macro-repeat-to-end
  "pG" 'projectile-find-tag
  "pn" '(lambda()
    (interactive)
    (find-file (concat (projectile-acquire-root) "README.org"))
    )
  "pu" 'sp-backward-unwrap-sexp
  "p <tab>" 'wally/projectile-recent-file
  "rj" 'jump-to-register
  "rk" 'kill-rectangle
  "rt" 'string-rectangle
  "rw" 'window-configuration-to-register
  "sd" 'youdao-dictionary-search-at-point+
  "sD" 'youdao-dictionary-search-from-input
  "smo" 'org-mode
  "sme" 'emacs-lisp-mode
  "smc" 'c-mode
  "smC" 'c++-mode
  "smf" 'fundamental-mode
  "sr" 'sr-speedbar-toggle
  "wj" 'wally@julie
  ";Pc" 'wally/org-pomodoro-contine-current-task
  ";Vu" 'wally/video-update-info
  ";Vv" 'wally/evil-add-new-video
  ";aE" 'wally/ann-export-source-list
  ";ac" 'wally/org-scramy-item
  ";ad" 'wally/org-load-data-from-db
  ";cg" 'org-capture-goto-last-stored
  ";ed" 'wally/edit-diff-temp-buffers
  ";ee" 'wally/eval-last-sexp-and-join-result
  ";et" 'wally/edit-copy-region-to-tmp-buffer
  ";hn" 'wally/helm-quick-note
  ";hr" 'wally/helm-regexp
  ";ia" 'wally/org-add-new-subheading
  ";ij" 'wally/org-item-jump
  ";iD" 'wally/org-image-remove-item-at-point
  ";lR" 'leetcode-refresh
  ";lS" 'wally/leetcode-save-current-problem
  ";lc" 'wally/leetcode-start-c-or-cpp
  ";le" 'wally/leetcode-export-to-anki-card
  ";lg" nil
  ";lgC" 'wally/leetcode-switch-to-cpp
  ";lgt" 'wally/leetcode-switch-to-test
  ";ll" 'leetcode
  ";lq" 'leetcode-quit
  ";lr" 'wally/leetcode-run
  ";lt" 'leetcode-try
  ";lu" 'leetcode-submit
  ";ma" 'wally/mindmap-add
  ";mc" 'wally/mindmap-convert
  ";mo" 'wally/media-open-at-point-by-external-app
  ";md" 'wally/org-media-visit-parent-dir
  ";oe" 'wally/org-import-evernote
  ";oh" 'wally/org-yank-html-as-orgmode
  ";ol" 'wally/copyq-orglink
  ";or" 'wally//org-random-subheading
  ";oR" 'wally/org-search-reference
  ";os" 'wally/org-sort-entries
  ";ot" 'org-tags-view
  ";pa" 'wally/pros-init
  ";pd" 'wally/pros-subtask-done
  ";pp" 'wally/pros-snapshot
  ";pr" 'wally/pros-goto-root
  ";pu" 'wally/pros-update-datetime
  ";rs" 'wally/edit-add-blank-line-for-region
  ";vb" 'wally/video-import-bilibi
  ";vo" 'wally/video-open-at-piont
  ;; ";vu" 'wally/video-update-info-at-piont
  ";vu" 'wally/video-update-meta
  ";va" 'wally/video-fetch-new-item
  ";vd" 'wally/video-download-at-point
  )


(defconst __orgmode__ nil)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  ";" 'org-timer-set-timer
  "p" 'org-pomodoro
  "yb" 'wally/macro-copy-org-src-block
  "vv" 'org-columns
  "iv" 'org-toggle-inline-images
  "gb" 'org-mark-ring-goto
  "gr" 'org-random-todo-goto-new
  "hpi" '(lambda()
           (interactive)
           (let ((val (point))
                 (anchor))
             (org-set-property "CUSTOM_ID" (format "%08x" val))
             (setq anchor (format "[[#%08x][archor]]" val))
             (kill-new anchor)
             (message anchor)
             )
           )

  "Dc" 'wally/org-scramy-item
  "Dd" 'wally/org-load-data-from-db
  "ii" 'wally/image-download "iS" 'org-attach-screenshot
  "lc" 'wally/finance-convert-orgheading-to-ledger-item
  "sg" 'org-refile-goto-last-stored
  "st" 'wally/org-add-arhive-tag
  "sR" 'wally/org-refile-local
  "sv" 'wally/org-archive-preprocess
  "tr" 'wally/org-task-refresh-subitems
  "tf" 'wally/org-table-eval-formulas
  "wL" 'wally/org-clear-logbook
  "wd" 'wally/task-finish
  "wl" 'wally/org-delink-at-point
  "ww" 'wally/org-wrap-region
  "xl" 'org-toggle-latex-fragment
  "y." 'wally/org-copy-link-at-point
  ">" 'org-time-stamp-inactive
  "<" '(lambda()
         (interactive)
         (insert (format-time-string "%Y-%m-%d" (current-time))))
  "<tab>" 'wally/org-hiden-subtree
  )

(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
  "p" 'org-pomodoro
  "v" 'wally/org-agenda-set-value
  "," 'wally/org-agenda-done-with-note
  "." 'wally/macro-agenda-done-with-clock
  )

(evil-define-key 'normal org-mode-map (kbd "M-n") '(lambda()
                                                     (interactive)
                                                     (wally/buf-scroll-first-buffer nil)))
(evil-define-key 'normal org-mode-map (kbd "M-p") '(lambda()
                                                     (interactive)
                                                     (wally/buf-scroll-first-buffer t)))


(defconst __programming__ nil)

(spacemacs/set-leader-keys-for-major-mode 'web-mode
  "f" 'web-mode-fold-or-unfold
  "n" 'web-mode-navigate
  "m" 'web-mode-mark-and-expand
  "w" 'web-mode-whitespaces-show
  "," 'browse-url-of-buffer
  "/" 'web-mode-element-close
  )

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  "fc" 'hs-toggle-hiding
  "fa" 'hs-hide-all
  "fs" 'hs-show-all
  )


(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "b" 'wally/func-quick-bind-key-at-point
  "f" '(lambda () (interactive) (elisp-format-buffer) (delete-trailing-whitespace))
  "if" 'wally/func-insert-entry-info-at-point
  )

(evil-define-key 'normal c++-mode-map (kbd "M-n") '(lambda()
                                                     (interactive)
                                                     (wally/buf-scroll-first-buffer nil)))
(evil-define-key 'normal c++-mode-map (kbd "M-p") '(lambda()
                                                     (interactive)
                                                     (wally/buf-scroll-first-buffer t)))

(evil-define-key 'normal c-mode-map (kbd "M-n") '(lambda()
                                                   (interactive)
                                                   (wally/buf-scroll-first-buffer nil)))
(evil-define-key 'normal c-mode-map (kbd "M-p") '(lambda()
                                                   (interactive)
                                                   (wally/buf-scroll-first-buffer t)))
