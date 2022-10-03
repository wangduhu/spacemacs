
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface hi-read
  '((((class color) (min-colors 66) (background light))
     :background "#E9E9E9"
     )
    (t :inverse-video t))
  "Basic face for highlighting"
  :group 'basic-faces)

(defun wally/highlight-phrase ()
  (interactive)
  (let* (phrase
         len
         (upper-limit 60)
         (lower-limit 4))
    (if (region-active-p)
        (setq phrase (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq phrase (word-at-point)))
    (setq len (length  phrase))
    (if (or (> len upper-limit) (< len lower-limit))
        (message "phrase too long or too short")
      (highlight-phrase phrase 'hi-read))))

(defun wally/unhighlight-symbol-at-point ()
  (interactive)
  (unhighlight-regexp (concat "\\_<" (thing-at-point 'symbol) "\\_>")))

(defun wally/edit-copy-to-line-end ()
  (interactive)
  (let* ((str (buffer-substring-no-properties (point) (line-end-position))))
    (kill-new str)
    (message "%s" str)))

(defun wally/text-center ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                      (/ (window-width) 6)
                      (/ (window-width) 3)))

(defun wally/text-center-reset ()
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) nil nil))

(defun wally/text-replace-word-at-point(replace-str)
  (interactive "sReplace String:")
  (let ((word (word-at-point)) end)
    (save-excursion (forward-word)
                    (setq end (point))
                    (backward-word)
                    (kill-ring-save (point) end)
                    (replace-string (current-kill 0) replace-str))))

(defun wally/text-highlight-phrase()
  (interactive)
  (let* ((phrase (buffer-substring-no-properties (region-beginning) (region-end)))
         (len (length phrase))
         (upper-limit 30)
         (lower-limit 6))
    (if (or (> len upper-limit) (< len lower-limit))
        (message "phrase too long or too short")
      (highlight-phrase phrase))))

(defun wally/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun wally/kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (wally/kill-thing-at-point 'word))

(defun wally/edit-kill-lineno-with-filename()
  (interactive)
  (let ((fl (format "%s:%s" (f-filename (buffer-file-name)) (line-number-at-pos))))
    (message "%s" fl)
    (kill-new fl)))

(defun wally/edit-flush-line-at-point ()
  (interactive)
  (let (pattern)
    (when (not (region-active-p))
      (goto-char (line-beginning-position))
      (set-mark  (line-end-position)))
    (setq pattern (buffer-substring-no-properties (region-beginning) (region-end)))
    (deactivate-mark)
    (save-excursion
      (goto-char (point-min))
      (flush-lines pattern))))

(defun wally/edit-add-blank-line-for-region ()
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end))
        content)
    (setq content (buffer-substring-no-properties beg end))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (save-excursion (replace-regexp "\n+" "\n"))
      (save-excursion (replace-regexp "\n" "\n\n"))
      (setq content (buffer-substring-no-properties (point-min) (point-max)))
      )
    (delete-region beg end)
    (insert content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/buf-scroll-buffer(winnum direction)
  (let ((curwin (winum-get-number)))
  (save-excursion (winum-select-window-by-number winnum)
                  (if direction (if (equal major-mode 'pdf-view-mode)
                                    (pdf-view-previous-page 1)
                                  (scroll-down-line 3))
                    (if (equal major-mode 'pdf-view-mode)
                        (pdf-view-next-page 1)
                      (scroll-up-line 3)))
                  (winum-select-window-by-number curwin))))

(defun wally/buf-scroll-first-buffer(direction)
  "scroll other buffer, typically last one
   if DIRECTION is default, then scroll up, else scroll down
   "
  (interactive)
  (wally/buf-scroll-buffer 1 direction))

(defun wally/buf-scroll-last-buffer(direction)
  "scroll other buffer, typically last one
   if DIRECTION is default, then scroll up, else scroll down
   "
  (wally/buf-scroll-buffer (length (winum--available-numbers)) direction))

(defun wally/buf-switch-to-message-buffer()
  (interactive)
  (switch-to-buffer "*Messages*")
  (goto-char (point-max)))

(defun wally/buf-switch-to-org-note-buffer()
  (interactive)
  (switch-to-buffer "*note*")
  (org-mode))

(defun wally/buf-switch-to-temp-buffer()
  (interactive)
  (switch-to-buffer "*tmp*")
  (fundamental-mode))

(defun wally/buf-switch-to-message-buffer()
  (interactive)
  (switch-to-buffer "*Messages*")
  (goto-char (point-max)))

(defun wally/buffer-switch-to-agenda()
  (interactive)
  (let ((buffer "*Org Agenda*"))
    (if (not (get-buffer buffer))
        (message "no agenda buffer")
      (switch-to-buffer buffer))))

(defun wally/buf-kill-all-buffers-under (dir)
  (let ((count 0)
        (buffers (buffer-list))
        buffer-id
        )
    (dolist (buffer buffers)
      (set-buffer buffer)
      (if (s-starts-with-p "*" (buffer-name))
          (setq buffer-id (buffer-name))
        (setq buffer-id default-directory))
      (when (s-starts-with-p dir buffer-id)
        (setq count (1+ count))
        (kill-buffer buffer)))
    (message "Killed %i dired buffers." count)))

(defun wally/buf-kill-filtered-buffers(filter)
  "Kill filtered buffers."
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (funcall filter)
          (setq count (1+ count))
          (kill-buffer buffer)
          ))
      (message "Killed %i dired buffer(s)." count))))

(defun wally/buf-kill-all-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (wally/buf-kill-filtered-buffers '(lambda () (equal major-mode 'dired-mode)))
  )

(defun wally/buf-kill-all-temp-buffers ()
  "Kill all temp buffers."
  (interactive)
  (wally/buf-kill-filtered-buffers '(lambda () (s-starts-with-p "*tmp" (buffer-name))))
  (setq wally-temp-buffer-cursor 0))

(defun wally/buf-kill-all-children-buffers ()
  "Kill all children buffers."
  (interactive)
  (let ((parent-path default-directory))
    (wally/buf-kill-filtered-buffers '(lambda () (s-starts-with-p parent-path default-directory)))))

(setq wally-temp-buffer-cursor 0)

(defun wally/edit-copy-region-to-tmp-buffer ()
  (interactive)
  (let ((cur-buf (current-buffer))
        (buf (format "*tmp%d*" wally-temp-buffer-cursor))
        (content (buffer-substring (region-beginning) (region-end))))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (insert content))
    (cl-incf wally-temp-buffer-cursor)
    (message "send region to %s" buf)
    (switch-to-buffer cur-buf)))

(defun wally/edit-diff-temp-buffers ()
  (interactive)
  (let ((buf1 (format "*tmp%d*" (- wally-temp-buffer-cursor 2)))
        (buf2 (format "*tmp%d*" (- wally-temp-buffer-cursor 1))))
    (ediff-buffers buf1 buf2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/file-load-lines-to-list (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer (insert-file-contents filePath)
                    (split-string (buffer-string) "\n" t)))

(defun wally/file-mktemp-file(ext &optional root)
  (if (not root)
      (setq root default-directory))
  (format "%s.%s"
          (s-trim (shell-command-to-string (format "mktemp -u %s/XXXXXXXX" root)))
          ext))

(defun wally/file-mktemp-img()
  "generate a random name for a png file"
  (interactive)
  (let ((image-root (expand-file-name "~/Wally/Journal/figure")))
    (insert (format " %s" (wally/file-mktemp-file "png" image-root)))))

(setq wally-file-temp-dir "~/Desktop")

(defun wally/helm-temp-dir()
  (interactive)
  (let* ((source '((name . "temp dir")
                   (candidates . ("~/Desktop/"
                                  "~/Download/"
                                  "/tmp"
                                  ))
                   (action . (lambda (candidate)
                               (setq wally-file-temp-dir candidate)))))
         )
    (helm-other-buffer 'source "temp dir")
    ))

(defun wally/file-lastest-temp-file()
  (let ((sandbox wally-file-temp-dir) target)
    (setq target (car (car (sort (directory-files-and-attributes sandbox t "^[^\\.].*\\.[a-z]+$")
                                 #'(lambda (x y)
                                     (time-less-p (nth 6 y) (nth 6 x)))))))
    target))

(defun wally/file-trash-misc-files()
  (interactive)
  (if (and (boundp 'reserved-files)
           reserved-files)
      (f-entries default-directory (lambda (file)
                                     (if (not (member (f-filename file) reserved-files))
                                         (progn (move-file-to-trash file)
                                                (message "%s removed" file)))))))

(defun wally/file-quick-backup(filename)
  (f-copy filename (format "%s.%s" filename (format-time-string "%Y%m%d%H%M%S" (current-time)))))

(defun wally/file-quick-backup-current()
  (interactive)
  (let ((filename (buffer-file-name)))
    (wally/file-quick-backup filename)
    (message "backuped")))

(defun wally/dired-fetch-latest-file()
  (interactive)
  (let ((src-file (wally/file-lastest-temp-file))
        (dst-dir default-directory))
    (f-move src-file (format "%s/" dst-dir))
    (message "move %s to %s" src-file dst-dir)))

(defvar wally-snap-suffix ".snap")
(defun wally/snap-auto-delete ()
  (let ((filepath (buffer-file-name)))
    (when (s-ends-with-p wally-snap-suffix filepath)
      (save-buffer)
      (message "deleting %s" filepath)
      (f-delete filepath))))

(defun wally/snap-mirror ()
  "create a snap mirror of current file
TODO
- [ ] change major-mode automatically
- [ ] map mirror buffer with current buffer
"
  (interactive)
  (let* ((snap-dir "~/.snap")
         (src-file (buffer-file-name))
         (dst-file (f-join snap-dir (format "%s.snap" (f-filename src-file))))
         (content (buffer-substring (point-min) (point-max)))
         )
    (when (not (f-exists-p snap-dir))
      (f-mkdir snap-dir)
      (message "snap-dir created"))
    (when (f-exists-p dst-file)
      (f-delete dst-file)
      (message "remove dst-file(%s)" dst-file))
    (find-file dst-file)
    (insert content)
    (save-buffer)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/helm-quick-note ()
  (interactive)
  (let (source
        (notes (wally/file-load-lines-to-list (concat wally-data-dir "/notes")))
        )
    (setq source '((name . "helm quick notes")
                   (candidates . notes)
                   (action . (lambda (candidate)
                               (kill-new candidate)
                               (message "<%s> killed" candidate)
                               ))))
    (helm-other-buffer 'source "quick notes")))

(defun wally/helm-command()
  (interactive)
  (let ((source)
        (commands (wally/lines2list "~/.emacs.d/private/lists/commands")))
    (setq source '((name . "Replace Regexp")
                   (candidates . commands)
                   (action . (lambda(candidate)
                               (shell-command candidate nil nil)))))
    (helm-other-buffer 'source "wally command")))

;; TODO 太复杂，需要简化
(defun wally/helm-regexp()
  (interactive)
  (let (source)
    (setq source '((name . "Replace Regexp")
                   (candidates . (("orglink" . ("L" "\\(~.+\/\\)\\(.+\\)$" "[[\\1\\2][\\2]]")) ; replace * ~/path/to/file with org-link
                                  ("quotes" . ("L" "\"" "\'")) ; replace `"' with `'' mainly in python code
                                  ("html-block" . ("G" "#\\+BEGIN_HTML\n.+\n#\\+END_HTML" "\n")) ; remove HTML block in org-mode converted from html/url
                                  ("property-block" . ("G" " *:PROPERTIES:\n.+\n *:END:" "")) ; remove property block in org-mode converted from html/url
                                  ("multiple blank" . ("L" " +" " "))
                                  ("c-comment-symbol" . ("G" "\\\\\\\\ +\\(.+\\)$" "\\\\* \\1
*\\\\"))
                                  ("figure-path" . ("G" ":.**figure.download"
                                                    ":/home/wally/Wally/Journal/figure/download"))
                                  ("inbox-task-headins" . ("L" "^.+\\[\\(.+\\)\\]\\]$" "* TODO
\\1"))))
                   (action . (lambda(candidate)
                               (cond ((string-equal (car candidate) "L")
                                      (replace-regexp (nth 1 candidate)
                                                      (nth 2 candidate) nil
                                                      (line-beginning-position)
                                                      (line-end-position)))
                                     ((string-equal (car candidate) "G")
                                      (replace-regexp (nth 1 candidate)
                                                      (nth 2 candidate) nil (point-min)
                                                      (point-max)))
                                     ((string-equal (car candidate) "R")
                                      (replace-regexp (nth 1 candidate)
                                                      (nth 2 candidate) nil
                                                      (line-beginning-position)
                                                      (line-end-position))))))))
    (helm-other-buffer 'source "wally regexp")))

(defun wally/helm-special-characters()
  (interactive)
  (let (source)
    (setq source '((name . "helm special characters")
                   (candidates . (("①" . "①")
                                  ("②" . "②")
                                  ("③" . "③")))
                   (action . (lambda(candidate)
                               (insert candidate)))))
    (helm-other-buffer 'source "special characters")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/math-set-highest-bit(num)
  (let ((mask (ash 1 31)))
    (setq num (logior num mask))
    num))

(defun wally/math-clear-highest-bit(num)
  (let ((mask (lognot (logxor (ash 1 31)))))
    (setq num (logand num mask))
    num))

(defun wally/math-set-or-unset-highest-bit-at-point(arg)
  (interactive "p")
  (let* ((src (string-to-number (word-at-point))) dst)
    (if (equal arg 4)
        (setq dst (wally/math-clear-highest-bit src))
      (setq dst (wally/math-set-highest-bit src)))
    (message "%s [0x%x] casted from %d [0x%x]" dst dst src src)))

(defun wally/math-sub-two-nums-from-kill-ring()
  (interactive)
  (let ((num1 (string-to-number (substring-no-properties (nth 0 kill-ring))))
        (num2 (string-to-number (substring-no-properties (nth 1 kill-ring)))))
    (message "%d - %d = %d" num1 num2 (- num1 num2))))

(defun wally/math-rate(rate)
  (if (>= rate 0.7) "A" (if (>= rate 0.5) "B" "C")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/str-norm (str)
  (let ((rlt "")
        tmp
        )
    (cl-loop for c across (downcase str)
             do (cond
                 ((member c (list 32 ?- ?/ ?\ ?+ ?| ?. ?? ?, ?~ ?:  ?， ?。 ?？)) (setq tmp "_"))
                 ((member c (list ?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?) ?[ ?] ?{ ?} ?' ?` ?< ?< ?《 ?》)) (setq tmp ""))
                 (t (setq tmp (make-string 1 c)))
                 )
             (setq rlt (concat rlt tmp))
             )
    rlt))

(defun wally//str-replace-special-characters (str)
  (let ((spec-chars " `'\"#^%&*()[]{}?/|\\<>,.;:@!:[]~·…、「」【】！：？=+")
        (con-str "-")
        (result "")
        prev)
    (dolist (s (mapcar #'char-to-string str))
      (if (s-index-of s spec-chars)
          (setq s con-str))
      (if (not (and prev (s-equals-p s con-str)))
          (setq result (format "%s%s" result s)
                prev (s-equals-p s con-str))))
    (if (s-ends-with-p con-str result)
        (setq result (substring result 0 -1)))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; datetime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/date-yesterday()
  (format-time-string "%Y-%m-%d" (time-subtract (current-time)
                                                (seconds-to-time (* 24 (* 60 60))))))

(defun calc-count-days (start-date end-date &optional absence-periods exclude-weekends?)
  "Return the number of days between START-DATE and END-DATE,
minus any days covered in one or more ABSENCE-PERIODS. Dates
should be entered a string such as \"YYYY, MM, DD\".
ABSENCE-PERIODS should be a given as a list of pairs like
\(\(START-ABSENCE-DATE END-ABSENCE-DATE\) ...\).
EXCLUDE-WEEKENDS? can be set to `t' to exclude weekend days from
the calculated number of days."
  (let ((date-sub-func (if exclude-weekends? #'calcFunc-bsub #'calcFunc-sub)))
    (defun eval-calc-date (calc-date-string)
      (calc-eval (concat "(date(" calc-date-string "))") 'raw))
    (defun eval-absence-periods (absence-periods)
      (mapcar (lambda (x)
                (list (eval-calc-date (car x))
                      (eval-calc-date (cadr x)))) absence-periods))
    (defun calculate-absent-days (absences)
      (seq-reduce #'+ (mapcar (lambda (x)
                                (funcall date-sub-func (cadr x)
                                         (car x))) absences) 0))
    (let* ((start (eval-calc-date start-date))
           (end (eval-calc-date end-date))
           (absences (eval-absence-periods absence-periods))
           (absent-days (calculate-absent-days absences)))
      (calc-eval "$ - $$" nil (funcall date-sub-func end start) absent-days))))

(defun wally/org-count-days()
  (interactive)
  (let ((arch-note (f-join wally-gtd-dir "pros.org"))
        (note-id "fe289648-6494-47e2-a4a5-404431e333f2")
        (first-day (format "%s, 01, 01" (format-time-string "%Y" (current-time))))
        (today (format-time-string "%Y, %m, %d" (current-time))) delta percentage)
    (setq delta (+ 1 (string-to-number (calc-count-days first-day today))) percentage (/ delta 365.0))
    (find-file-noselect arch-note)
    (with-current-buffer (get-file-buffer arch-note)
      (org-id-goto note-id)
      (replace-regexp "[0-9]+/365" (format "%d/365" delta)
                      nil (line-beginning-position) (line-end-position))
      (save-buffer))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'wally/macro-copy-org-src-block
      (kmacro-lambda-form [?\C-c ?\' ?\; ?b ?Y ?\C-c ?\'] 0 "%d"))

(fset 'wally/macro-alternate-buffer
      (kmacro-lambda-form [?\; ?b ?b return] 0 "%d"))

(fset 'wally/macro-join-next-line
      (kmacro-lambda-form [?J ?$ ?a return escape ?k ?J ?0] 0 "%d"))

(fset 'wally/macro-sr-speedbar
      (kmacro-lambda-form [?\; ?s ?r ?\; ?w ?d ?\; ?w ?/ ?\; ?b ?b return] 0 "%d"))

(fset 'wally/macro-browse-db-item
      (kmacro-lambda-form [?, ?g ?r ?g ?p ?$ ?h ?h return ?\; ?f ?o ?q ?t ?d] 0 "%d"))

(fset 'wally/macro-agenda-done-with-clock
      (kmacro-lambda-form [?I ?O ?t ?d] 0 "%d"))

(fset 'wally/macro-agenda-todo-quit
      (kmacro-lambda-form [?t ?q] 0 "%d"))


(defun wally/macro-repeat-to-end ()
  (interactive)
  (kmacro-end-and-call-macro 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wally/file-save-clipboard-content-to-temp-file()
  (interactive)
  (let ((tmp-file "/tmp/a.txt"))
    (with-temp-buffer (evil-paste-after nil)
                      (write-region (point-min)
                                    (point-max) tmp-file))
    (message "saved to %s" tmp-file)))

(defun wally/copyq-read-items(count)
  "copy last count items in clipboard via copyq"
  (interactive "pCount:")
  (let ((cmd (format "copyq separator \"\n\" read {%d..0}" (- count 1)))
        output
        $items)
    (setq output (shell-command-to-string cmd)
          $items (s-split "\n" output))
    $items))

(defun wally/copyq-download-clipboard-image()
  (interactive)
  (let ((tmp-path (make-temp-file "" nil ".png")))
    (shell-command (format "copyq read image/png > %s" tmp-path))
    (wally/org-download-image-and-limit-size (format "file://%s" tmp-path))))

(defun wally/bookmark-add-url(url)
  (interactive "sBookmark URL: ")
  (if (assoc url bookmark-alist)
      (user-error "%s is already bookmarked" url)
    (push `(,url . ((handler . ,(lambda (bookmark)
                                  (browse-url (car bookmark))))))
          bookmark-alist)))

(defun wally/projectile-recent-file()
  (interactive)
  (let ((root-dir (projectile-acquire-root))
        (recent-file (car (projectile-recentf-files))))
    (find-file (f-join root-dir recent-file))))

(defun wally/log-next-recent-log()
  (interactive)
  (let ((pattern "\\[\\([a-zA-Z0-9_]+\\.[ch]\\):\\([0-9]+\\)\\]")
        filename
        current-line
        next-recent-line
        target-pos
        found
        (max-span 50))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward pattern (line-end-position) t 1)
        (setq filename (match-string-no-properties 1)
              current-line (string-to-number (match-string-no-properties 2))
              pattern (format "%s:\\([0-9]+\\)" filename))
        (end-of-line)
        (while (and (not found) (re-search-forward pattern nil t))
          (setq next-recent-line (string-to-number (match-string-no-properties 1)))
          (if (and (> next-recent-line current-line) (< next-recent-line (+ current-line max-span)))
              (setq found t
                    pos (point))))))
    (if pos
        (progn (point-to-register ?j)
               (goto-char pos)))))

(defun wally/eval-last-sexp-and-join-result()
  "eval last sexp an dinsert result as comment"
  (interactive)
  (save-excursion
    (save-excursion
      (evil-open-below 1)
      (evil-force-normal-state)
      (insert " ;; ")
      (eval-print-last-sexp)
      )
    (next-line)
    ;; joint comment
    (join-line)
    (next-line)
    ;; join result
    (join-line)
    (next-line)
    ;; join blank line
    (join-line)))

(defun wally/func-quick-bind-key(command)
  "bind current function to hotkey"
  (interactive "Ccommand: ")
  (spacemacs/set-leader-keys "." command))

(defun wally/func-quick-bind-key-at-point()
  (interactive)
  (spacemacs/set-leader-keys "." '(lambda()
                                    (interactive)
                                    (funcall (intern (wally/func-get-name-at-point))))))


(provide 'wally-utils)
