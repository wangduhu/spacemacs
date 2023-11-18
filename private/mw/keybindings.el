(defconst __global__ nil)

(spacemacs/set-leader-keys
  "al" 'delete-blank-lines
  )


(defconst __orgmode__ nil)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "p" 'org-pomodoro
  "fl" 'wally/finance-convert-orgheading-to-ledger-item
  )
