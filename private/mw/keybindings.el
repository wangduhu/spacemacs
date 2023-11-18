(defconst __global__ nil)

(spacemacs/set-leader-keys
  "al" 'delete-blank-lines
  "<ESC>" '(lambda() (interactive) (find-file (f-join wally-gtd-dir "journal.org")))
  "<RET>" 'org-clock-goto
  )


(defconst __orgmode__ nil)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "p" 'org-pomodoro
  "fl" 'wally/finance-convert-orgheading-to-ledger-item
  )
