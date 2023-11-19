(defconst __global__ nil)

(global-set-key [f8] 'deft)

(spacemacs/set-leader-keys
  "aa" 'org-agenda
  "al" 'delete-blank-lines
  "a/" 'spacemacs/search-engine-select
  "ba" '(lambda ()
          (interactive)
          (let ((buffer "*Org Agenda*"))
            (if (get-buffer buffer)
                (switch-to-buffer buffer)
              (message "no agenda buffer"))))
  "bo" '(lambda () (interactive) (interactive) (switch-to-buffer "*note*") (org-mode))
  "bt" '(lambda () (interactive) (switch-to-buffer "*tmp*"))
  "b <SPC>" '(lambda () (interactive) (find-file "~/.emacs.d/private/dotspacemacs.el"))
  "gd" 'magit-diff-buffer-file
  "hs" 'highlight-symbol-at-point
  "hu" '(lambda () (interactive) (unhighlight-regexp (concat "\\_<" (thing-at-point 'symbol) "\\_>")))
  "pg" 'project-find-regexp
  "pA" 'helm-projectile-ack
  "qc" 'quick-calc
  "<ESC>" '(lambda() (interactive) (find-file (f-join wally-gtd-dir "journal.org")))
  "<RET>" 'org-clock-goto
  ";" nil                               ; 覆盖掉默认绑定 `evilnc-comment-operator'
  "@" 'pop-global-mark
  "#" 'org-mark-ring-push
  "$" 'org-mark-ring-goto
  "`" 'wally/tmp
  )


(defconst __orgmode__ nil)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "p" 'org-pomodoro
  "fl" 'wally/finance-convert-orgheading-to-ledger-item
  "id" 'wally/org-download-image-and-limit-size
  "iD" 'wally/copyq-download-clipboard-image
  "sc" 'org-capture-goto-last-stored
  "tk" 'wally/org-table-kill-cell
  "t+" 'org-table-sum
  "t." '(lambda()
          (interactive)
          (let (row col coor)
            (setq row (org-table-current-line))
            (setq col (org-table-current-column))
            (setq coor (format "@%d$%d" row col))
            (kill-new coor)
            (message coor)))
  )


(defconst __elisp__ nil)

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "." 'eval-defun
  "b" 'wally/func-quick-bind-key-at-point
  )


(defconst __dired__ nil)

(spacemacs/set-leader-keys-for-major-mode 'dired-mode
  "," nil
  "C" 'wally/dired-fetch-latest-file
  )
