(setq wally--emacs-on-start t)

(if (not (string-equal system-type "gnu/linux"))
  (progn
    (setq midnight-hook nil)
    (midnight-delay-set 'midnight-delay "11:30pm")
    (add-hook 'midnight-hook (lambda()
                               (when (not wally--emacs-on-start)
                                 (wally/auto-routine)
                                 )))))

(setq wally--emacs-on-start nil)
