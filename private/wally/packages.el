(defconst wally-packages
  '(
    anki-editor
    annotate
    buttercup
    calfw
    calfw-org
    cnfonts
    dirvish
    easy-kill
    elisp-format
    emacsql-sqlite
    epc
    highlight-function-calls
    nlinum-hl
    nov
    org
    org-dashboard
    org-media-note
    org-noter
    org-randomnote
    org-random-todo
    org-roam
    org-roam-ui
    org-super-agenda
    ;; ox-latex
    projectile
    simple-httpd
    sr-speedbar
    ssh-deploy
    vue-mode
    yasnippet
    ;; local
    (wally-utils :location local)
    (wally-dice :location local)
    (wally-anki :location local)
    (wally-org :location local)
    (wally-logb :location local)
    (wally-pros :location local)
    (wally-ann :location local)
    (wally-leetcode :location local)
    )
  "The list of Lisp packages required by the wally layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun wally/init-anki-editor ()
  (use-package anki-editor))

(defun wally/post-init-annotate ()
  (use-package annotate
    :after org
    :config
    (setq annotate-file (concat wally-journal-dir "data/annotations"))
    ))

(defun wally/init-buttercup ()
  (use-package buttercup))

(defun wally/init-calfw ()
  (use-package calfw))

(defun wally/init-calfw-org ()
  (use-package calfw-org))

(defun wally/init-cnfonts ()
  (use-package cnfonts
    :config
    (cnfonts-enable)))

(defun wally/init-dirvish ()
  (use-package dirvish
    ))

(defun wally/init-easy-kill ()
  (use-package easy-kill
    :bind
    ([remap kill-ring-save] . easy-kill)))

(defun wally/init-elisp-format ()
  (use-package elisp-format))

(defun wally/init-emacsql-sqlite ()
  (use-package emacsql-sqlite
    :config
    (setq mydb (emacsql-sqlite (format "%s/data/my.db" wally-journal-dir)))))

(defun wally/init-epc ()
  (use-package epc
    ))

(defun wally/init-highlight-function-calls ()
  (use-package highlight-function-calls))

(defun wally/init-nlinum-hl ()
  (use-package nlinum-hl
    :hook
    (prog-mode . (lambda ()
                   (nlinum-mode nil)))
    ))

(defun wally/init-nov ()
  (use-package nov
    ;; :bind
    ;; (:map nov-mode-map
    ;;       ((kbd "n") . scroll-up-command)
    ;;       ((kbd "p") . scroll-down-command)
    ;;       )
    :mode "\\.epub\\'"))

(defun wally/init-org-dashboard ()
  (use-package org-dashboard
    :after org
    :config
    ))

(defun wally/post-init-org-dashboard ()
  (use-package org-dashboard
    :after org
    :config
    (setq org-dashboard-files (list (f-join wally-gtd-dir "pros.org")))
    ))

(defun wally/init-org-media-note ()
  (use-package org-media-note
    ;; :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
    :hook (org-mode .  org-media-note-mode)
    :bind (("M-v" . org-media-note-hydra/body))  ;; Main entrance
    :config
    (setq org-media-note-screenshot-image-dir "~/Wally/Journal/assets/mpv"))
  )

(defun wally/init-org-noter ()
  (use-package org-noter
    :config
    (setq org-noter-always-create-frame nil)
    ))

(defun wally/init-org-random-todo ()
  (use-package org-random-todo
    :config
    (org-random-todo-mode)))

(defun wally/init-org-randomnote ()
  (use-package org-randomnote
    :after org
    :config
    (setq org-randomnote-candidates 'current-buffer)
    ))

(defun wally/init-org-super-agenda ()
  (use-package org-super-agenda
    :config
    (setq org-super-agenda-groups
          '((:name "IMPORTANT" :priority "A" :order 0)
            (:name "JULIE" :file-path "julie.org" :order 1)
            (:name "REFLECTION" :tag "REFLECTION" :order 1)
            (:name "PROGRESS" :file-path "pros.org" :order 1)
            (:name "HEALTH" :tag "HEALTH" :order 2)
            (:name "DIET" :tag "DIET" :order 2)
            (:name "FITNESS" :tag "FITNESS" :order 2)
            (:name "RECREATION" :tag "RECREATION" :order 3)
            (:name "FINANCE" :tag "FINANCE" :order 5)
            (:name "LEARNING" :tag "LEARNING" :order 6)
            (:name "INFO" :tag "INFO" :order 7)
            )
          )
    ))

(defun wally/post-init-projectile ()
  (use-package projectile
    :config
    (setq projectile-enable-caching t
          projectile-project-root-files-bottom-up '(".projectile")
          projectile-indexing-method 'native)
    ))

(defun wally/init-sr-speedbar ()
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

(defun wally/init-simple-httpd ()
  (use-package simple-httpd
    :init
    (setq httpd-port "7070"
          httpd-port-proxy "7071")
    ))

(defun wally/init-ssh-deploy ()
  (use-package ssh-deploy
    :bind
    (:map ssh-deploy-prefix-map
          ("f" . ssh-deploy-upload-handler-forced))))

(defun wally/init-vue-mode ()
  (use-package vue-mode
    :mode "\\.vue\\'"))

(defun wally/post-init-yasnippet ()
  (use-package sr-speedbar
    :init
    (setq yas-snippet-dirs (list "~/.emacs.d/private/snippets"))
    :config
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
    ))

(defun wally/post-init-org-roam ()
  (use-package org-roam
    :requires f
    :after org
    :config
    (setq org-roam-directory (f-join (f-parent wally-journal-dir) "logseq" "roam")
          org-roam-db-location (f-join wally-data-dir "org-roam.db"))
    ))

(defun wally/init-org-roam-ui()
  (use-package org-roam-ui
    :after org-roam
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
    ))

(defun wally/post-init-org ()
  (use-package org
    :requires f
    :commands wally/org-summary-todo
    :init
    (add-hook 'org-after-todo-statistics-hook #'wally/org-summary-todo)
    (add-to-list 'org-src-lang-modes '(("html" . web)
                                       ("browser" . nxhtml)
                                       ("php" . php)
                                       ("cmake" . cmake)
                                       ("gdb" . GDB-Script)
                                       ("makefile" . makefile)
                                       ("markdown" . markdown)))
    :hook
    (org-mode . (lambda()
                  (auto-fill-mode t)
                  (smartparens-mode t)
                  (autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
                  (autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
                  (iimage-mode t)))
    :config
    (setq org-file-apps '((auto-mode . emacs)
                          ("\\.mm\\'" . default)
                          (directory . emacs)
                          ;; ("\\html\\'" . "firefox %s")
                          ("\\.html\\'" . system)
                          ("\\.pdf\\'" . system)
                          ("\\.xmind\\'" . system)
                          ("\\.\\(mp4\\|mov\\|avi\\|wmv\\)\\'" . "mpv %s"))
          org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                                 (vm-imap . vm-visit-imap-folder-other-frame)
                                 (gnus . org-gnus-no-new-news)
                                 (file . find-file)
                                 (wl . wl-other-frame))
          ;; agenda
          org-agenda-time-grid nil
          ;; org-agenda-time-grid '((daily today require-timed)
          ;;                        (500 700 2200 2330)
          ;;                        "......"
          ;;                        "----------------")
          org-agenda-files (append (list (f-join wally-refs-dir "index.org")
                                         (f-join wally-journal-dir "data" "reference.org")
                                         "~/julie/works/README.org")
                                   (directory-files wally-gtd-dir t ".+\.org")
                                   (directory-files wally-note-dir t ".+\.org"))
          org-agenda-custom-commands (list '(";" "key tasks" agenda "" ((org-agenda-span 'day)
                                                                        (org-agenda-skip-function
                                                                         '(org-agenda-skip-entry-if
                                                                           'regexp
                                                                           ":TRIVAL:"))))
                                           '("n" "Agenda and all TODOs" ((agenda "")
                                                                         (alltodo "")))
                                           '("w" "Weekly review" agenda "" ((org-agenda-span 'week)
                                                                            (org-agenda-start-on-weekday 0)
                                                                            (org-agenda-start-with-log-mode t)
                                                                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done)))))

          ;; capture
          org-capture-templates '(("b" "Code Snippet" plain (file "~/Wally/Journal/dice/inbox/babel.org")
                                   "* %F @ %T\n\n#+BEGIN_SRC C :results output :tangle /tmp/a.c\n%i\n#include <stdio.h>\nint main(){\n\nreturn 0;\n}\n#+END_SRC"
                                   :empty-lines 1 :immediate-finish t :jump-to-captured t)
                                  ("c" "Code Snippet" entry
                                   (file+headline "~/Wally/Journal/dice/inbox/snippets.org" "TODO INBOX")
                                   "** TODO %^{PROMPT}\n:PROPERTIES:\n:TIMESTAMP: %T\n:SOURCE: %F\n:ANKI_DECK: Tech::Code\n:ANKI_NOTE_TYPE: 基础\n:ANKI_FAILURE_REASON: Missing fields\n:END:\n\n*** pros\n\n#+BEGIN_SRC C\n%i#+END_SRC\n\n%F\n\n*** cons\n\n"
                                   :empty-lines 1 :immediate-finish t)
                                  ("t" "Task from input" plain (file+headline wally-note-file "INBOX")
                                   "** TODO [#C] %^{PROMPT}\n%u"
                                   :empty-lines 1 :immediate-finish t :prepend t)
                                  ("p" "Protocol" entry (file+headline wally-note-file "INBOX")
                                   "** TODO %i\n%U\n[[%:link][%:description]]"
                                   :empty-lines 1 :immediate-finish t :prepend t)
                                  ("L" "Protocol Link" entry (file+headline wally-note-file "INBOX")
                                   "** TODO [[%:link][%:description]]\n%U"
                                   :empty-lines 1 :immediate-finish t :prepend t)
                                  ("l" "booklet" plain (file+headline "~/Wally/Journal/data/reference.org" "Guide")
                                   "** \n%u"
                                   :empty-lines 1 :immediate-finish t :jump-to-captured t)
                                  )
          ;; refile
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          ;; org-refile-allow-creating-parent-nodes 'confirm org-refile-targets '(("~/Wally/Journal/note/journal.org"
          ;;                                                                       :regexp . "JOURNAL"))
          org-refile-use-outline-path 'file
          wally-dice-files (directory-files wally-dice-dir t ".+\.org")
          wally-note-files (directory-files wally-note-dir t ".+\.org")
          org-refile-targets '(
                               ;; (nil :maxlevel . 1) ;; current
                               (wally-dice-files :maxlevel . 1)
                               (wally-note-files :maxlevel . 1)
                               ;; ((list wally-note-file) :maxlevel . 1)
                               )

          ;; archive
          org-archive-location "~/Wally/Journal/data/anki-inbox.org::** content" ; org-archive
          ;; refile
          ;; org-random-todo
          org-random-todo-files (list (f-join wally-dice-dir "inbox" "tasks.org"))
          org-random-todo-how-often (* 3600 3)
          ;; org-download
          org-download-method 'directory
          org-download-image-dir (concat wally-journal-dir "assets/download")
          org-download-heading-lvl nil
          org-download-timestamp "%Y-%m-%d-%H-%M-%S"
          org-download-backend t)
    (setq wally-pros-note (f-join wally-gtd-dir "pros.org"))
    (setq wally-mindmap-dir (f-join wally-journal-dir "mindmap")
          wally-mindmap-title nil
          wally-mindmap-filenameally-mindmap-filename nil)

    (setq wally-video-note (f-join wally-dice-dir "inbox" "video.org") wally-video-title nil
          wally-video-url nil wally-video-filename nil)

    (setq wally--org-eval-files (list (f-join wally-gtd-dir "routine.org")
                                      (f-join wally-gtd-dir "pros.org")
                                      (f-join wally-gtd-dir "julie.org")))

    (org-babel-do-load-languages  'org-babel-load-languages
                                  '((C . t)
                                    (python . t)
                                    (java . t)
                                    (emacs-lisp . t)
                                    (shell . t)
                                    (latex . t)
                                    (plantuml . t)
                                    (octave . t)
                                    (dot . t)
                                    ;; (ditaa . t)
                                    ;; (gnuplot . t)
                                    ;; (matlab . t)
                                    ;; (org . t)
                                    ;; (ruby . t)
                                    ;; (awk . t)
                                    ))

    ;; latex
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.3))

    (add-to-list 'org-latex-classes
                 '("cn-article"
                   "\\documentclass[10pt,a4paper]{article}
\\usepackage{ctex}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{minted}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,
linkcolor=blue,
urlcolor=blue,
menucolor=blue]{hyperref}
\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
marginparsep=7pt, marginparwidth=.6in}
\\definecolor{foreground}{RGB}{220,220,204}%浅灰
\\definecolor{background}{RGB}{62,62,62}%浅黑
\\definecolor{preprocess}{RGB}{250,187,249}%浅紫
\\definecolor{var}{RGB}{239,224,174}%浅肉色
\\definecolor{string}{RGB}{154,150,230}%浅紫色
\\definecolor{type}{RGB}{225,225,116}%浅黄
\\definecolor{function}{RGB}{140,206,211}%浅天蓝
\\definecolor{keyword}{RGB}{239,224,174}%浅肉色
\\definecolor{comment}{RGB}{180,98,4}%深褐色
\\definecolor{doc}{RGB}{175,215,175}%浅铅绿
\\definecolor{comdil}{RGB}{111,128,111}%深灰
\\definecolor{constant}{RGB}{220,162,170}%粉红
\\definecolor{buildin}{RGB}{127,159,127}%深铅绿
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    )
  )


(defun wally/init-wally-utils ()
  (use-package wally-utils))


(defun wally/init-wally-dice ()
  (use-package wally-dice))


(defun wally/init-wally-anki ()
  (use-package wally-anki))


(defun wally/init-wally-org ()
  (use-package wally-org))


(defun wally/init-wally-logb ()
  (use-package wally-logb))


(defun wally/init-wally-pros ()
  (use-package wally-pros))


(defun wally/init-wally-ann ()
  (use-package wally-ann))


(defun wally/init-wally-leetcode ()
  (use-package wally-leetcode))
