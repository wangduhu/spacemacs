(defun wally@julie()
  (interactive)
  (message "wally@julie!")
  )

(defun wally/org-download-image-and-limit-size (link)
  (interactive "sLink: ")
  (save-excursion
    (org-download-image link))
  (next-line)
  (end-of-line)
  (insert "\n#+attr_org: :width 800"))

;; (add-hook 'kill-buffer-hook 'wally/snap-auto-delete)

(defun wally/org-show-outline-path ()
  (interactive)
  (message (org-format-outline-path (org-get-outline-path))))

(defun wally/org-wrap-region(left-delime)
  "wrap region with prompted symbol"
  (interactive "sDELIME: ")
  (let ((begin (region-beginning))
        (end (region-end))
        (right-delime left-delime))
    (cond ((string-equal left-delime "[")
           (setq right-delime "]"))
          ((string-equal left-delime "(")
           (setq right-delime ")"))
          ((string-equal left-delime "{")
           (setq right-delime "}"))
          ((string-equal left-delime "<")
           (setq right-delime ">"))
          ((string-equal left-delime "\[")
           (setq right-delime "\]")))
    (save-excursion (goto-char end)
                    (if (or (= (char-after) 10)
                            (= (char-after) 32)) ; blank space
                        (insert right-delime)
                      (insert (format "%s " right-delime)))
                    (goto-char begin)
                    (if (or (= (char-before) 32)
                            (= (char-before) 10))
                        (insert left-delime)
                      (insert (format " %s" left-delime))))))

(defun wally/org-pandoc-from-markdown-inplace()
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-filename (format "%s.org" filename)))
    (shell-command-on-region (point-min)
                             (point-max) "pandoc -f markdown -t org" t t)
    (save-buffer)
    (rename-file filename new-filename)
    (set-visited-file-name new-filename)
    (set-buffer-modified-p nil)
    (org-mode)
    (goto-char (point-min))
    (while (re-search-forward "^ *:PROPERTIES:" nil t)
      (org-mark-element)
      (delete-region (region-beginning)
                     (region-end)))
    (save-buffer)))

(defun wally/org-helm-src-block-lang()
  (interactive)
  (let (source)
    (setq source '((name . "helm src block lang")
                   (candidates . ("emacs-lisp" "C++" "C" "python" "sh"))
                   (action . (lambda(candidate)
                               (setq wally-src-block-lang candidate)))))
    (helm-other-buffer 'source "src block lang")))

(defvar wally-src-block-lang nil)
(defun wally/org-replace-example-block-with-src()
  (interactive)
  (if (not wally-src-block-lang)
      (wally/helm-src-block-lang))
  (save-excursion (re-search-backward "^#\\+\\(BEGIN\\|begin\\)" nil t 1)
                  (search-forward "_EXAMPLE")
                  (replace-match "_SRC")
                  (insert (format " %s" wally-src-block-lang))
                  (search-forward "_EXAMPLE")
                  (replace-match "_SRC")))
(defun wally/org-yank-html-as-orgmode()
  (interactive)
  (with-temp-buffer (yank)
                    (shell-command-on-region (point-min)
                                             (point-max) "pandoc -f html -t org -" nil t)
                    (kill-new
                     (buffer-substring
                      (point-min)
                      (point-max))))
  (yank))




(defvar wally--org-eval-files nil)
(defvar wally--org-logbook-state-pattern "- State \"\\([A-Z]+\\)\" *from \"\\([A-Z]+\\)\" *\\[\\([0-9-]+\\) .+\\]")
(defvar wally--org-logbook-note-pattern "- Note taken on \\[\\([0-9-]+\\) .+\\].+\n *\\(.+\\)")

(defun wally/org-agenda-auto-quit-on-timeout()
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d" (current-time)))
         date
         task)
    (dolist (f wally--org-eval-files)
      (find-file-noselect f)
      (with-current-buffer (get-file-buffer f)
        (goto-char (point-min))
        (while (re-search-forward "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)[^>]+[1-3]d>" nil t)
          (setq date (match-string 1)
                task (org-get-heading t t t t))
          (message "%s" task)
          (if (not (string-greaterp date today))
              (save-excursion (outline-previous-heading)
                              (org-todo "QUIT")
                              (message "found timeout task(%s) scheduled at <%s>" task date)))
          (next-line))
        (save-buffer)))))

(defun _wally/org-norm-value-property (value)
  "转换数字或时间字符串为整数
e.g.
`30' -> 30
`00:30' -> 30
`23:45' -> -15
"
  (cond
   ((string-match "^[0-9]+$" value)
    (string-to-number value))
   ((string-match "^\\([0-9][0-9]\\)[:\\.]\\([0-9][0-9]\\)$" value)
    (let (hour minute total-minutes)
      (setq hour (string-to-number (match-string-no-properties 1 value))
            minute (string-to-number (match-string-no-properties 2 value))
            total-minutes (+ minute (* 60 hour)))
      (if (> total-minutes 1000)
          (setq total-minutes (- total-minutes 1440)))
      total-minutes))
   (t -1)))

(defun wally/org-task-save-and-refresh-value ()
  "存储heading的VALUE属性值后清零"
  (let* ((value (org-entry-get nil "VALUE"))
         (table (org-entry-get nil "TABLE")))
    (when (and value table (not (s-equals-p value "NIL")))
      (epc:call-sync wally-habit-epc 'record (list table (_wally/org-norm-value-property value) (format-time-string "%Y-%m-%d" (current-time))))
      (org-set-property "VALUE" "NIL")
      (save-buffer))))

(defun wally/org-auto-evaluate-daily()
  (interactive)
  (let (tmp-records
        task-records
        table
        (gtd-note (f-join wally-gtd-dir "routine.org"))
        (pros-note (f-join wally-gtd-dir "pros.org"))
        (report-id "A3ADC23B-96C8-41AE-8C21-9B74798F0505")
        )
    (evil-write-all nil)
    ;; gtd
    (dolist (f  wally--org-eval-files)
      (find-file-noselect f)
      (with-current-buffer (get-file-buffer f)
        (goto-char (point-min))
        (setq tmp-records (org-map-entries '(lambda ()
                                              (wally/org-task-save-and-refresh-value)
                                              (wally/org-evaluate-task))
                                           "WEIGHT>0")))
      (setq task-records (append task-records tmp-records))
      )
    ;; viz
    (setq table (wally//org-visualize-daily-routine task-records))
    ;; (wally/logseq-add-journal table)
    (org-id-goto report-id)
    (org-narrow-to-subtree)
    (org-show-subtree)
    (next-line)
    (re-search-forward "^\\*+ " nil t 1)
    (if (org-at-heading-p)
        (org-archive-subtree))
    (goto-char (point-max))
    (save-excursion
      (insert table))
    (next-line)
    (if (org-at-heading-p)
        (org-demote-subtree))
    (widen)))

(defun wally//org-visualize-daily-routine (records)
  (let ($table (total 0) (finished 0) score)
    (with-temp-buffer
      (insert (format "\n|%s|WEIGHT|STATE|\n|-+-|-+-|-+-|-+-|\n" (format-time-string "%Y-%m-%d" (current-time))))
      (dolist (record records)
        (if record
            (let ((weight (string-to-number (gethash "weight" record)))
                  (state (gethash "state" record)))
              (setq total (+ total weight))
              (if (string-equal state "DONE")
                  (setq finished (+ finished weight)))
              (insert (format "|%s|%d|%s|\n"
                              ;; (gethash "heading-id" record)
                              (gethash "title" record)
                              weight
                              state))
              )
          ))
      (setq score (/ (float finished ) (if (= total 0) 1 total)))
      (insert (format "|-+-|-+-|-+-|-+-|\n|Summary| %d/%d |%s(%.2f)|\n\n\n" finished total (wally//org-evaluate-rank score) score))
      (goto-char (point-min))
      (save-excursion
        (insert (format "\n* TODO [#%s] %s\n %s\n\n"
                        (wally//org-evaluate-rank score)
                        (format-time-string "%Y-%m-%d" (current-time))
                        (format-time-string "SCHEDULED: <%Y-%m-%d %A %H:%M>" (current-time))
                        )))
      (org-mode)
      (search-forward "* " nil t 1)
      (org-set-property "SCORE" (format "%.2f" score))
      (setq $table (buffer-substring-no-properties (point-min) (point-max)))
      )
    $table))

(defun wally//org-evaluate-rank (score)
  (cond
   ((> score 0.85) "A")
   ((> score 0.75) "B")
   ((> score 0.65) "C")
   (t "D")))

(defun wally/org-evaluate-task()
  (let ($record
        (updated-date (org-entry-get nil "LAST_REPEAT"))
        (today (format-time-string "%Y-%m-%d" (current-time)))
        )
    (when (string-equal (substring updated-date 1 11) today)
      (let* ((states (wally//org-parse-logbook-states (wally//org-get-logbook)))
             state)
        (when states
          (setq state (nth 0 states)
                $record (make-hash-table :test 'equal))
          (puthash "title" (wally/org-get-heading-no-progress) $record)
          (puthash "heading-id" (org-id-get-create) $record)
          (puthash "weight" (org-entry-get nil "WEIGHT") $record)
          (if (string-equal (gethash "date" state) today)
              (puthash "state" (gethash "new-state" state) $record)
            (puthash "state" "QUIT" $record)))))
    $record))


(defun wally/org-clear-logbook ()
  (let ((pattern "^- State.+\[0-9-]+.+\]$"))
    (dolist (f wally--org-eval-files)
      (find-file-noselect f)
      (with-current-buffer (get-file-buffer f)
        (goto-char (point-min))
        (flush-lines pattern)))))

(defun wally//org-parse-logbook-states (logbook  &optional count)
  (let ($states)
    (with-temp-buffer
      (insert logbook)
      (goto-char (point-min))
      (save-excursion (flush-lines "^:.+:$"))
      (save-excursion (flush-lines "^$"))
      (while (re-search-forward wally--org-logbook-state-pattern nil t)
        (let ((state (make-hash-table :test 'equal)))
          (puthash "new-state" (match-string 1) state)
          (puthash "old-state" (match-string 2) state)
          (puthash "date" (match-string 3) state)
          (setq $states (add-to-list '$states state)))))
    (setq $states (reverse $states))
    $states))

(defun wally//org-parse-logbook-notes (logbook &optional count)
  (let ($notes)
    (with-temp-buffer
      (insert logbook)
      (goto-char (point-min))
      (save-excursion (flush-lines "^:.+:$"))
      (save-excursion (flush-lines "^$"))
      (while (re-search-forward wally--org-logbook-note-pattern nil t)
        (let ((note (make-hash-table :test 'equal)))
          (puthash "new-state" (match-string 1) note)
          (puthash "old-state" (match-string 2) note)
          (puthash "date" (match-string 3) note)
          (puthash "note" (match-string 5) note)
          (setq $notes (add-to-list '$notes note)))))
    (setq $notes (reverse $notes))
    $notes))

(defun wally//org-get-logbook ()
  (let ($logbook)
    (org-mark-subtree)
    (save-excursion
      (when (re-search-forward "^ *:LOGBOOK:$" (region-end) t 1)
        (deactivate-mark)
        (org-mark-element)
        (setq $logbook (buffer-substring-no-properties (region-beginning) (region-end)))))
    (deactivate-mark)
    $logbook))

(defun wally/org-get-heading-no-progress()
  (interactive)
  (let (($heading (org-get-heading t t t t))
        (pattern "\\(.+\\) +\\(\\[.+\\]\\)\\{1,2\\}$"))
    (when (string-match pattern $heading)
      (setq $heading (match-string 1 $heading)))
    $heading))

(defun wally/org-update-progress-by-refresh-subitem()
  "update org heading progress if it has sub task items"
  (let ((heading (org-get-heading t t t t))
        (pattern ".+\\[\\([0-9/%]*\\)\\]$")
        (beg  (point))
        end
        old-process
        new-process)
    (when (string-match pattern heading)
      (message "Enter wally/org-update-progress-by-refresh-subitem<%s>" heading)
      (setq old-process (match-string-no-properties 1 heading))
      (save-excursion
        (end-of-line)
        (if (re-search-forward "^\\*+ " nil t 1)
            (setq end (point))
          (setq end (point-max))))
      (message "%d" end)
      (save-excursion
        (if (re-search-forward "^- \\[[X\\ ]\\] ?.*$" end t 1)
            (progn (org-ctrl-c-ctrl-c)
                   (org-ctrl-c-ctrl-c)
                   (message "sub item refreshed"))
          (message "found no sub item")))
      (setq heading (org-get-heading t t t t))
      (string-match pattern heading)
      (setq new-process (match-string-no-properties 1 heading))
      (if (not (string-equal old-process new-process))
          (message "update process of <%s> to <%s>" heading new-process)))))

(defun wally/org-agenda-done-with-note()
  (interactive)
  ;; (org-agenda-todo 'done)
  (org-agenda-add-note)
  )


(defun wally/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))


(defun wally/daily-report ()
  (interactive)
  (let ((id "BF55CF1E-1527-47FD-B551-43FDC4A34A55"))
    ;; (wally/dice-daily)
    (wally/org-auto-evaluate-daily)
    ;; export
    ;; (org-id-goto id)
    ;; (wally/org-export-to-hugo)
    )
  )

(defun wally/org-export-to-html ()
  "publish certain org files as html files"
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-export-to-html)
    ))

(defun wally/org-export-to-hugo()
  "publish certain org files as html files"
  (interactive)
  (let ((org-confirm-babel-evaluate nil))
    (org-hugo-export-to-md)
    ))

(defun wally/org-refile-local()
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 2))))
    (org-refile)
    ))

(defun wally/org-agenda-set-value (val)
  "set property `VALUE' from agenda buffer"
  (interactive "sVALUE: ")
  (when (eq major-mode 'org-agenda-mode)
    (wally/org-agenda-set-property "VALUE" val)))

(defun wally/org-agenda-set-property (prop val)
  "Set a property for the current headline."
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (org-set-property prop val)))))

(defun wally/org-guess-db-id-from-heading ()
  (let ((link (wally/org-heading-get-link))
        filepath)
    (setq filepath (url-filename (url-generic-parse-url link)))
    filepath))

(defun wally/org-load-data-from-db ()
  "import data from db as org properties"
  (interactive)
  (let ((db (org-entry-get nil "DB" t))
        (db-tbl (org-entry-get nil "DB_TBL" t))
        (db-keys (org-entry-get nil "DB_KEYS" t))
        (db-id-key (org-entry-get nil "DB_ID_KEY" t))
        alias
        conn
        record
        item
        value
        )
    (if (or (not db) (not db-keys) (not db-id-key))
        (error "no db or db-keys specified!"))
    (if db-id-key
        (setq iid (org-entry-get nil (upcase db-id-key))))
    (if (not iid)
        (setq iid (wally/org-guess-db-id-from-heading)))
    ;; 对于URL字符串，约定使用相对路径作为主键值
    (when (s-starts-with-p "http" iid)
      (setq iid (url-filename (url-generic-parse-url iid))))
    (setq db-keys (s-split " " db-keys))
    (setq conn (emacsql-sqlite db))
    ;; key:alias
    (dolist (key db-keys)
      (if (not (s-index-of ":" key))
          (setq alias (upcase key))
        (setq key (s-split ":" key)
              alias (car (cdr key))
              key (car key))
          )
      (setq record (emacsql conn [:select $i1 :from $i2 :where (= $i3 $r4)]
                            (make-symbol key) (make-symbol db-tbl) (make-symbol db-id-key) iid))
      (setq value "")
      (setq record (car record))
      (dolist (item record)
        (cond
         ((numberp item) (setq value (number-to-string item)))
         ((symbolp item) (setq value (concat value (symbol-name item))))
         ((listp item) (setq value (concat value (s-join "" (mapcar (lambda (x) (symbol-name x)) item)))))
         (t (message "invalid type %s:%s" (type-of item) item)
            (setq value nil))
         )
        (org-set-property alias value)))
    (emacsql-close conn)))

(defun wally/org-scramy-item ()
  (interactive)
  (let ((link (wally/org-heading-get-link))
        (scramy "~/Project/scramy"))
    (if (not link)
        (setq link (org-entry-get nil "LINK")))
    (if (not link)
        (error "no link specified!"))
    (let ((default-directory scramy))
      (shell-command (format "scrapy parse --pipelines %s" link)))))
(defun wally/db-retrive-all-by-main-key (conn table key)
  (let (records items)
    (setq records (emacsql conn [:select $i1 :from $i2]
                         (make-symbol key) (make-symbol table)))
    (dolist (rec records)
      (add-to-list 'items (symbol-name (car rec))))
    items))

(defun wally/logb-new-post (post table title records &optional baseurl)
  "generate a new post with data from db"
  (if (not baseurl) (setq baseurl ""))
  (find-file-noselect post)
  (with-current-buffer (get-file-buffer post)
    (goto-char (point-max))
    (org-mode)
    (save-excursion
      (insert (format "\n* %s%s\n" table title))
      (org-set-property "EXPORT_FILE_NAME" (concat table (f-base title)))
      (dolist (rec records)
        (insert (format "\n%s\n" (concat baseurl (symbol-name (car rec)))))))
    (next-line)
    (org-todo "DONE")
    (save-buffer)
    (org-hugo-export-to-md nil t)))

(defun wally/logb-retrive-from-scramy (post table main-key item-key &optional baseurl)
  (let ((database "~/Project/scramy/scramy.sqlite3")
        items
        records
        choice
        conn
        (today (format-time-string "%Y-%m-%d" (current-time)))
        )
    (setq conn (emacsql-sqlite database))
    (setq items (wally/db-retrive-all-by-main-key conn table main-key))
    ;; 查询history表, 找到首个没有查看过的记录
    (setq choice
          (dolist (item items)
            (setq records (emacsql conn [:select * :from history :where (and (= tablename $r1) (= mainkey $s2))]
                                   table (make-symbol item)))
            (if (not records)
                (return item))))

    (setq records nil)
    ;; 记录history，并从目标表中查询数据
    (when choice
      (emacsql conn [:insert :into history :values ([$s1 $s2 $s3 $s4])]
               (make-symbol table) (make-symbol choice) 0 (make-symbol today))
      (setq records (emacsql conn [:select $i1 :from $i2 :where (= $i3 $r4)]
                             (make-symbol item-key)
                             (make-symbol table)
                             (make-symbol main-key)
                             choice)))
    ;; 写入文件
    (when records
      (wally/logb-new-post post table choice records baseurl)
      (message "new post: %s%s" table choice))
    (emacsql-close conn)))

(defun wally/logb-auto-routine ()
  (interactive)
  (wally/logb-retrive-from-scramy "~/Project/logb/logb/orgposts/netbian.org" "netbian" "page" "url" "https://pic.netbian.com/")
  (wally/logb-retrive-from-scramy "~/Project/logb/logb/orgposts/vmgirls.org" "vmgirls" "page" "img")
  (let ((default-directory "~/Project/logb/logb"))
    (shell-command "hugo --buildDrafts"))
  )

(defun wally/logseq-add-journal (note)
  (let* ((journal-root "~/Wally/logseq/journals")
         (note-file (f-join journal-root
                            (format-time-string "%Y_%m_%d.org"
                                                (time-add (current-time) (seconds-to-time (* 24 (* 60 60))))))))
    (find-file-noselect note-file)
    (with-current-buffer (get-file-buffer note-file)
      (goto-char (point-min))
      (insert note)
      (save-buffer))))

(defun wally/logseq-init-journal ()
  "按org-journal日期格式初始化次日笔记"
  (wally/logseq-add-journal
   (format-time-string "* %A, %B %d %Y"
                       (time-add (current-time) (seconds-to-time (* 24 (* 60 60)))))))

(defun wally/logseq-normalize ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\\*\\*\\([0-9:]+\\)\\*\\*" "*\\1*")
    (goto-char (point-min))
    (replace-regexp "^\\(\\* .+\\) \\(\\[\\[...assets.+\\(jpg\\|jpeg\\|png\\|webp\\)\\]\\]\\)" "\\1\n\n#+attr_org: :width 600px\n\\2\n")
    (goto-char (point-min))
    (replace-regexp "^\\(\\*[0-9\\.]+\\* .+\\)" "* \\1")
    (goto-char (point-min))
    (replace-regexp "^\\(\\*\\* \\*?[0-9\\.\\.]+\\*?\\) \\(.+\\)" "\\1\n\n\\2\n")
    (goto-char (point-min))
    (replace-regexp "\n\n\n" "\n\n")
    (goto-char (point-min))
    (replace-regexp "\\[\\[quick capture\\]\\]" "quick capture")))

(defun wally/org-task-refresh-subitems()
  (interactive)
  (save-excursion (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (replace-regexp "^- \\[X\\]" "- [ ]")
                  (widen)))

(defun wally/org-hiden-subtree()
  (interactive)
  (outline-previous-heading)
  (org-cycle nil))
(defun wally/org-clear-logbook()
  (interactive)
  (save-excursion (while (re-search-forward "^:LOGBOOK:" nil t)
                    (org-mark-element)
                    (delete-region (region-beginning)
                                   (region-end)))))

(defun wally/org-goto-element-by-name(name)
  (goto-char (point-min))
  (if (not (re-search-forward (format "#\\+NAME: *%s" name) nil t 1))
      (message "element <%s> not found in current buffer")))

(defun wally/org-insert-sub-headings(headings)
  (message "Enter wally/org-insert-sub-headings")
  (org-insert-subheading nil)
  (insert (car headings))
  (dolist (heading (cdr headings))
    (org-insert-heading-after-current)
    (insert heading))
  (message "Exit wally/org-insert-sub-headings"))

(defun wally/org-remove-agenda-info()
  (interactive)
  (save-excursion
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (let ((heading (wally/org-get-heading-no-progress))
          (pattern "^\\(\\*+\\) .+"))
      ;; heading
      (re-search-forward pattern nil t 1)
      (replace-match "\\1 ")
      (insert (format "%s" heading))
      ;; timestamp [2021-08-08 日]
      (flush-lines "^\\[[0-9-]\\{10\\} .+\\]$")
      ;; CLOSED/SCHEDULED
      (flush-lines "^.*\\(CLOSED\\|SCHEDULED\\):.+$")
      ;; logbook
      (save-excursion
        (when (re-search-forward "^:LOGBOOK:$" nil t 1)
          (org-mark-element)
          (delete-region (region-beginning) (region-end))))
      ;; property
      (save-excursion
        (when (re-search-forward "^:PROPERTIES:$" nil t 1)
          (org-mark-element)
          (delete-region (region-beginning) (region-end)))))
    (widen)))

(defun todo-to-int (todo)
  (first (-non-nil
          (mapcar (lambda (keywords)
                    (let ((todo-seq
                           (-map (lambda (x) (first (split-string  x "(")))
                                 (rest keywords))))
                      (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                  org-todo-keywords))))

(defun wally/org-sort-key ()
  (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
         (todo (org-entry-get (point) "TODO"))
         (todo-int (if todo (todo-to-int todo) todo-max))
         (priority (org-entry-get (point) "PRIORITY"))
         (priority-int (if priority (string-to-char priority) org-default-priority)))
    (format "%03d %03d" todo-int priority-int)
    ))

(defun wally/org-sort-entries ()
  (interactive)
  (org-sort-entries nil ?f #'wally/org-sort-key))

(defun wally//org-get-title ()
  (let (title)
    (pcase (org-collect-keywords '("TITLE"))
      (`(("TITLE" . ,val))
       (setq title (car val))))
    title))

(defun wally/org-archive-preprocess ()
  (interactive)
  (while (not (org-at-heading-p))
    (org-up-element))
  (let ((heading (nth 4 (org-heading-components)))
        (level (nth 0 (org-heading-components)))
        )
    (save-excursion
      (re-search-forward "^$"  nil t 1)
      (insert (format "\n%s %s\n" (make-string level ?*) heading)))))

(defun wally/org-erase-subtree ()
  "remove subtree content, but keep properties and logbooks"
  (interactive)
  (org-mark-subtree) ;; mark the current subtree
  (re-search-forward "^:END:$" nil t)
  (delete-region (region-beginning) (region-end)) ;; delete the rest
  (deactivate-mark)
  )

(defun wally/org-get-title-by-id (id &optional with-desc)
  "get title by id, if with-desc, get desc property instead"
  (let ((curbuf (current-buffer))
        title
        desc)
    (org-id-goto id)
    (when (s-equals-p id (org-id-get))
      (setq title (nth 4 (org-heading-components))
            desc (org-entry-get nil "DESC"))
      (if (and with-desc desc)
          (setq title desc)))
    (switch-to-buffer curbuf)
    title))

(defun wally/org-heading-get-link ()
  (let* ((heading (nth 4 (org-heading-components)))
         filepath
         )
    (with-temp-buffer
      (insert heading)
      (setq filepath (org-element-property :raw-link (org-element-context))))
    filepath))

(defun wally/org-export-heading-to-org-file ()
  "export a heaing to single file"
  (interactive)
  (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME"))
        (export-dir (org-entry-get nil "EXPORT_DIR" t)))
    (if (not (org-at-heading-p))
        (error "not a heading"))
    (if (not export-file)
        (if (not export-dir)
            (error "export filename or directory not specfiled")
          (setq export-file (f-join export-dir (format "%s.org" (nth 4 (org-heading-components)))))))
    (message "exporting note to file <%s>" export-file)
    (org-narrow-to-subtree)
    (org-set-property "EXPORT_FILE_NAME" export-file)
    (org-org-export-to-org nil t)
    (widen)))


(defun wally/org-add-new-subheading ()
  "add a subheading expanded by template snippet"
  (interactive)
  (let ((snippet (org-entry-get nil "SNIPPET" t)))
    (if (not snippet)
        (setq snippet (org-table-get-constant "ITEM_SNIPPET")))
    (if (not snippet)
        (error "no snippet specified"))
    (insert snippet)
    (yas-expand)))

(defun wally/org-complete-properties ()
  "add required properties for given heading"
  (interactive)
  (if (not (org-at-heading-p))
      (error "currrent position is not a heading"))
  (let ((keys (org-entry-get nil "KEYS" t))
        value)
    (if (not keys)
        (error "no kyes specified for properties"))
    (setq keys (s-split " " keys))
    (dolist (key keys)
      (setq value (org-entry-get nil key))
      (if value
          (org-delete-property key)
        (setq value "NA"))
      (org-set-property key value))))

(defun wally/org-add-arhive-tag ()
  (interactive)
  (let ((tags (org-get-tags)))
    (add-to-list 'tags "ARCHIVE")
    (org-set-tags tags)))

(defun wally/org-add-domain-info-to-url-description-at-point()
  (interactive)
  (let ((line)
        (url)
        (desc)
        (domain))
    (setq line
          (buffer-substring
           (line-beginning-position)
           (line-end-position)))
    (save-match-data (and (string-match ".*\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\].*" line)
                          (setq url (match-string 1 line))
                          (setq desc (match-string 2 line)))
                     (if (not url)
                         (message "unmatched url-link")
                       (setq domain (wally/extract-domain-from-url url))
                       (replace-regexp "\\[\\[\\(.+\\)\\]\\[\\(.+\\)\\]\\]" (format
                                                                             "[[\\1][%s :: \\2]]"
                                                                             domain) nil
                                                                             (line-beginning-position)
                                                                             (line-end-position))))))
(defun wally/extract-domain-from-url(url)
  (let ((domain))
    (save-match-data (and (string-match
                           "https?...\\([a-z]+\\.\\)?\\([a-z\\-]+\\)\\.\\(cn\\|com\\|net\\|org\\).*"
                           url)
                          (setq domain (match-string 2 url))))))

(defun wally/org-link-convert-relative-path-to-absolute-path-at-point()
  (interactive)
  (let ((pattern "\\[\\(\\[.+]\\)?\\[\\(.+\\)\\]\\]")
        src
        dst)
    (beginning-of-line)
    (when (re-search-forward pattern (line-end-position) t 1)
      (setq src (match-string 2)
            dst (f-join (f-dirname (expand-file-name src)) (format "%s.png" (f-base src))))
      (replace-string src dst nil (line-beginning-position) (line-end-position)))))

(defun wally/org-delink-at-point()
  (interactive)
  (save-excursion (search-backward "[[")
                  (re-search-forward "\\[\\(\\[.+\\]\\)?\\[\\([^:]+\\)\\]\\]")
                  (replace-match "\\2")))

(defun wally/org-table-get-current-cell-index()
  (interactive)
  (let (row col index-str)
    (setq row (org-table-current-line) col (org-table-current-column) index-str (format "@%d$%d" row
                                                                                        col))
    (kill-new index-str)
    (message "%s" index-str)))

(defun wally/org-table-eval-formulas()
  (interactive)
  (save-excursion (beginning-of-line)
                  (while (and (= (char-after) ?#) (= (char-after (+ 2 (point))) ?T))
                    (org-ctrl-c-ctrl-c)
                    (next-line))))
(defun wally/org-copy-link-at-point()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (link))
    (save-excursion (beginning-of-line)
                    (re-search-forward "\\[\\[\\(.+\\)\\]\\(\\[.+\\]\\)?\\]" end nil)
                    (setq link (match-string 1))
                    (if (not link)
                        (message "no org-link at point")
                      (kill-new link)
                      (message link)))))

(defun wally/org-table-goto-last-line()
  (next-line)
  (beginning-of-line)
  (while (= (char-after) ?|)
    (next-line))
  (previous-line))

(defun wally/org-table-kill-cell()
  (interactive)
  (when (org-at-table-p)
    (kill-new
     (string-trim
      (substring-no-properties(org-table-get-field))))
    (org-table-blank-field)))

(setq org-pomodoro-length 30
      org-pomodoro-short-break-length 7
      org-pomodoro-long-break-length 13)

(add-hook 'org-pomodoro-started-hook
          (lambda()
            (start-process-shell-command
             "zenity notice" nil
             "zenity --info --title 'Pomodoro Started' --text '调整座椅'")))

(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (start-process-shell-command
             "zenity notice" nil
             (format "zenity --info --title 'Pomodoro' --text 'Put first thing first'"))))

(add-hook 'org-pomodoro-break-finished-hook
          (lambda()
            (start-process-shell-command
             "zenity notice" nil
             "zenity --info --title 'Pomodoro Started' --text 'Relaxation over!'"))
          )

(defun wally/org-pomodoro-contine-current-task()
  (interactive)
  (let ((note-buffer "inbox.org"))
    (with-current-buffer note-buffer
      (save-excursion
        (org-clock-goto)
        (org-pomodoro)))))

(defun wally/pros-init-directory(root-dir)
  (let((flag-files '(".projectile" "README.org"))
       (subdirs '("tmp" "arch")))
    (make-directory root-dir)
    (dolist (subdir subdirs)
      (make-directory (s-join "/" (list root-dir subdir))))
    (dolist (flag-file flag-files)
      (write-region "" nil (s-join "/" (list root-dir flag-file))))))

(defun wally/pros-init-git-repo(root-dir)
  (write-region "tmp\nbuild" nil (s-join "/" (list root-dir ".gitignore")))
  (let ((default-directory root-dir)
        (project-source-repo nil))
    (shell-command "git init")
    (shell-command "git add .")
    (shell-command "git commit -m 'init project'")
    (if project-source-repo (progn (shell-command (format "git submodule add %s source"
                                                          project-source-repo))
                                   (shell-command "git add .")
                                   (shell-command "git commit -m 'add source submodule'")))))

(defun wally/pros-init-note(project root-dir)
  (let ((ready (string-equal "pros.org" (buffer-name)))
        (dummy-properties '("PROS_REPO")))
    (if (not  ready)
        (message "Current buffer is not pros.org. Exit")
      (save-excursion (goto-char (point-min))
                      (re-search-forward "^\\* " nil t 1)
                      (previous-line)
                      (insert (format "\n* TODO [#B] %s [%%]\n" project))
                      (dolist (property dummy-properties)
                        (org-set-property (car dummy-properties) ""))
                      (org-id-get-create)
                      (org-set-property "PROS_START_DATE" (format-time-string "%Y-%m-%d"
                                                                              (current-time)))
                      (org-set-property "PROS_ROOT" root-dir)
                      (org-set-property "PROS_ABBR" project)
                      (org-set-property "PROS_VIZ_IMAGE" (substring-no-properties (s-trim
                                                                                   (shell-command-to-string
                                                                                    "mktemp -u XXXXXXXX.png"))))
                      (org-deadline 0)
                      (insert "\n:PROG:\n")
                      (wally/pros-format-basic-info-at-point)
                      (insert "\n\n")
                      (wally/pros-init-progress-data-table)
                      (insert "\n:END:\n"))) ready))

(defun wally/pros-init(project)
  (interactive "sPROJECT NAME: ")
  (save-excursion (let ((root-dir (s-join "/" (list wally-pros-root project))))
                    (if (file-exists-p root-dir)
                        (message "project<%s> exists!" root-dir)
                      (find-file wally-pros-note)
                      ;; note first
                      (if (wally/pros-init-note project root-dir)
                          (progn
                            ;; directory structure
                            (wally/pros-init-directory root-dir)
                            ;; git
                            (wally/pros-init-git-repo root-dir)
                            (message "Project<%s> is ready" project)))))))

(setq wally-pros-root (expand-file-name "~/Project"))
(defun wally/pros-get-current-project-name()
  (interactive)
  (save-excursion (next-line)
                  (re-search-backward
                   "^\\* \\(TODO\\|DONE\\) \\(\\[#[ABC]\\] \\)?\\([^ ]+\\)\\( \\[.+\\]\\)*" nil t 1)
                  (substring-no-properties (match-string 3))))

(defun wally/pros-goto-root(arg)
  ""
  "open project root or note file
  "
  ""
  (interactive "p")
  (let* ((project (wally/pros-get-current-project-name))
         (target (s-join "/" (list wally-pros-root project))))
    (if arg
        (setq target (s-join "/" (list target "README.org"))))
    (find-file target)))

(defun wally/pros-update-progress()
  (interactive)
  (find-file wally-pros-note)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries
     'wally/org-update-progress-by-refresh-subitem
     "LEVEL=1")))

(defun wally/pros-update-datetime()
  (message "Enter wally/pros-update-datetime")
  (interactive)
  (find-file wally-pros-note)
  (save-excursion (goto-char (point-min))
                  (org-map-entries '(lambda()
                                      (let ((deadline (substring (org-entry-get nil "DEADLINE") 1
                                                                 11))
                                            (start-date (org-entry-get nil "PROS_START_DATE"))
                                            (today (format-time-string "%Y-%m-%d" (current-time))))
                                        (setq today (date-to-day (concat today " 00:00:00"))
                                              start-date (date-to-day (concat start-date
                                                                              "  00:00:00"))
                                              deadline (date-to-day (concat deadline "  00:00:00")))
                                        (org-set-property "PROS_DAYS_PASSED" (format "%d" (- today
                                                                                             start-date)))
                                        (org-set-property "PROS_TIME_PROGRESS" (format "%d%%" (* (/
                                                                                                  (float
                                                                                                   (-
                                                                                                    today
                                                                                                    start-date))
                                                                                                  (-
                                                                                                   deadline
                                                                                                   start-date))
                                                                                                 100)))))
                                   "-IGNORE"))

  (message "Exit wally/pros-update-datetime"))

(defun wally/pros-concat-progress-images()
  (interactive)
  (message "Enter wally/pros-concat-progress-images")
  (let (images
        (output (expand-file-name "~/Share/mobile/timeflies.png"))
        input
        )
    (find-file wally-pros-note)
    (goto-char (point-min))
    (setq images
          (org-map-entries
           '(lambda ()
              ;; (message "%s" (org-id-get))
              (let ((img (org-entry-get nil "PROS_VIZ_IMAGE")))
                img
                ))
           "LEVEL=1"))
    (dolist (img (reverse images))
      (if img
          (add-to-list 'input (f-join (expand-file-name "~/Share/mobile") img))))
    (shell-command (format "convert %s -append %s" (s-join " " input) output)))
  (message "Exit wally/pros-concat-progress-images"))

(defun wally/pros-snapshot()
  (interactive)
  (message "Enter wally/pros-snapshot")
  (let ()
    (find-file wally-pros-note)
    (goto-char (point-min))
    (dolist (head-id (org-map-entries
                       '(lambda ()
                          (org-id-get))
                       "-IGNORE"))
             (if head-id
                 (wally/pros-update-progress-table-data head-id))))
  (message "Exit wally/pros-snapshot"))

(defun wally/pros-update-progress-table-data(head-id)
  (interactive)
  (let ((progress (wally/pros-get-current-progress head-id)) pros-tbl-name)
    (org-narrow-to-subtree)
    (when (re-search-forward "#\\+NAME: \\(TBL_DATA_[A-Z0-9_]+\\)$" nil t 1)
      (setq pros-tbl-name (substring-no-properties (match-string 1)))
      (wally/org-goto-element-by-name pros-tbl-name)
      ;; (wally/org-table-goto-last-line)
      (re-search-forward "^#" nil t 1)
      (beginning-of-line)
      (insert (format "|%s|\n" (s-join "|" progress)))
      (org-table-align)
      (search-forward "#+CALL: VIZPROG")
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-execute-maybe)))
    (org-hide-drawer-all)
    (widen)))

(defun wally/pros-get-current-progress(head-id)
  (let (passed-days time-progress task-progress)
    (find-file wally-pros-note)
    (org-id-goto head-id)
    (setq passed-days (org-entry-get nil "PROS_DAYS_PASSED") time-progress (org-entry-get nil
                                                                                          "PROS_TIME_PROGRESS")
          task-progress (wally/pros-get-task-progress-at-point) result (list passed-days (substring
                                                                                          time-progress
                                                                                          0 -1)
                                                                             task-progress))
    result))

(defun wally/pros-init-progress-data-table()
  (interactive)
  (let* ((info (wally/pros-get-basic-info-at-point))
         (abbr (gethash "abbr" info))
         (headers (list "no" "time" "progress")))
    (insert (format "\n#+NAME: TBL_DATA_%s" abbr))
    (insert (format "\n|%s|" (s-join "|" headers)))
    (insert (format "\n|%s|" (s-join "|" (make-list (length headers) "-+-"))))
    (insert (format "\n|%s|" (s-join "|" (make-list (length headers) "0"))))
    (org-table-align)
    (insert (format "\n#+CALL: VIZPROG(TBL_DATA_%s, TBL_INFO_%s) :results file :exports results\n"
                    abbr abbr))))

(defun wally/pros-format-basic-info-at-point()
  (interactive)
  (let ((info (wally/pros-get-basic-info-at-point))
        (keys (list "title" "abbr" "start-date" "deadline" "priority" "viz-img")))
    (insert (format "\n#+NAME: TBL_INFO_%s" (gethash "abbr" info)))
    (insert (format "\n|%s|" (s-join "|" keys)))
    (insert (format "\n|%s|" (s-join "|" (make-list (length keys) "-+-"))))
    (insert (format "\n|%s|" (s-join "|" (mapcar '(lambda (key)
                                                    (gethash key info)) keys))))
    (org-table-align)))

(defun wally/pros-get-basic-info-at-point()
  (let ((title (wally/pros-get-current-project-name))
        (abbr (org-entry-get nil "PROS_ABBR"))
        (start-date (org-entry-get nil "PROS_START_DATE"))
        (priority (org-entry-get nil "PRIORITY"))
        (deadline (substring-no-properties (org-entry-get nil "DEADLINE") 1 11))
        (viz-img (org-entry-get nil "PROS_VIZ_IMAGE"))
        (result (make-hash-table :test 'equal)))
    (puthash "title" title result)
    (puthash "abbr" abbr result)
    (puthash "start-date" start-date result)
    (puthash "priority" priority result)
    (puthash "deadline" deadline result)
    (puthash "viz-img" viz-img result) result))

(defun wally/pros-get-task-progress-at-point()
  "get task progess of heading at point"
  (let ((progress 0)
        (pattern "^\\*+ .+\\[\\([0-9]*\\)%\\]")
        (heading
         (buffer-substring-no-properties
          (line-beginning-position)
          (line-end-position))))
    (if (string-match pattern heading)
        (setq progress (match-string 1 heading))
      (message "unmatched heading %s" heading)) progress))

(defun wally/pros-cfw-view()
  (interactive)
  (let ((org-agenda-files (list (concat wally-gtd-dir "pros.org"))))
    (cfw:open-org-calendar)))

(defun wally/pros-subtask-done()
  (interactive)
  (let ((heading-id (org-table-get-constant "PARENT_TASK_ID"))
        (title (wally/org-get-heading-no-progress)))
    (find-file wally-pros-note)
    (org-id-goto heading-id)
    (re-search-forward "^- \\[ \\]\\( UNTITLED\\)?$" nil t 1)
    (replace-match (format "- [ ] %s" title))
    (org-ctrl-c-ctrl-c)))

(defun wally/pros-auto-update-progress-by-subitems()
  (interactive)
  (message "Enter wally/pros-auto-update-progress-by-subitems")
  (find-file wally-pros-note)
  (save-excursion
    (goto-char (point-min))
    (org-map-entries
     'wally/org-update-progress-by-refresh-subitem
     "-IGNORE"))
  (message "Exit wally/pros-auto-update-progress-by-subitems"))

(defun wally/pros-dateval-sync-image()
  (interactive)
  (message "Enter wally/org-dateval-sync-image")
  (let ((dateval-note (f-join wally-gtd-dir "dateval.org"))
        (tmp-html "/tmp/dateval.html")
        (img (expand-file-name "~/Share/mobile/dateval.png")))
    (find-file dateval-note)
    (goto-char (point-max))
    (wally/org-heading-to-html tmp-html)
    (wally/img-render-from-page (format "file://%s" tmp-html) img "table"))
  (message "Exit wally/org-dateval-sync-image"))

(defun wally/org-export-note(arg)
  (interactive "p")
  (let* ((id (org-id-get-create))
         (lid (downcase id))
        content
        beg
        end
        title
        tags
        (level 0)
        roam-note
        card-note
        (origin-buffer (current-buffer))
        )
    (org-mark-subtree)
    (org-set-property "LID" lid)
    (setq beg (region-beginning)
          end (region-end))
    (deactivate-mark)
    (goto-char beg)
    (setq title (wally/org-get-heading-no-progress)
          tags (org-get-tags))
    (while (=  (char-after) ?*)
      (setq level (1+ level))
      (forward-char)
      )
    (search-forward ":END:" end t 1)
    (setq beg (1+ (point))
          content (buffer-substring-no-properties beg end))

    ;; roam note, 加前缀则不导出
    (when (= arg 4)
      (setq roam-note (wally/org-roam-export lid title level tags content))
      (find-file roam-note))

    ;; anki note
    (setq card-note (wally/org-anki-export id title level tags content))
    (find-file card-note)
    (anki-editor-mode t)

    )
  )

(defun wally//org-roam-name-file (title)
  (format "%s-%s.org" (format-time-string "%Y%m%d%H%M%S" (current-time))
          (wally//str-replace-special-characters title)))

(defun wally/org-roam-export (id title level tags content)
  (let* ((filename (wally//org-roam-name-file title))
         (filepath (f-join org-roam-directory filename)))
    (with-temp-buffer
      (org-mode)
      (insert (format ":PROPERTIES:
:ID:       %s
:END:

#+title: %s

%s %s

%s

"
                      id
                      title
                      (make-string level ?*)
                      title
                      content
                      ))
      (goto-char (point-min))
      (re-search-forward "^\\*" nil t 1)
      (while (> level 1)
        (org-promote-subtree)
        (setq level (1- level)))
      (org-set-tags tags)
      (write-region (point-min) (point-max) filepath)
      (message "export roam note to <%s>" filepath)
      )
    filepath))


(defun wally/org-anki-export (id title level tags content)
  (let* ((filename (format-time-string "%Y%m%d%H%M%S.org" (current-time)))
         (filepath (f-join (f-parent wally-journal-dir) "data" "card" filename))
         myid)
    (with-temp-buffer
      (org-mode)
      (insert (format "* %s
:PROPERTIES:
:NOTE_ID:   %s
:ANKI_DECK: Notes
:ANKI_NOTE_TYPE: Note
:ANKI_TAGS: %s
:END:

** pros

%s %s

%s
"
                      title
                      id
                      (s-join " " tags)
                      (make-string level ?*)
                      title
                      content
                      ))
      (goto-char (point-min))
      (search-forward "pros" nil t 1)
      (re-search-forward "^\\*" nil t 1)
      (while (> level 3)
        (org-promote-subtree)
        (setq level (1- level)))
      (while (< level 3)
        (org-demote-subtree)
        (setq level (1+ level))
        )
      (goto-char (point-max))
      (insert "\n** cons\n\n")
      (insert (format "- [[org-protocol://org-id?id=%s][inner note link]]\n\n" id))
      (write-region (point-min) (point-max) filepath)
      (message "export anki note to <%s>" filepath)
      )
    (find-file-noselect filepath)
    (with-current-buffer (get-file-buffer filepath)
      (goto-char (point-min))
      (setq myid (org-id-get-create))
      (goto-char (point-max))
      (insert (format "- [[org-protocol://org-id?id=%s][inner anki link]]\n\n" myid))
      )
    filepath))

(defun wally/org-everorg-post-process ()
  (interactive)
  (goto-char (point-min))
  ;; special characters
  (save-excursion (replace-string " " " "))
  (save-excursion (replace-string "　" " "))
  (save-excursion (replace-regexp "\\([。]\\)\\*" "\\1* "))
  ;; multi lines
  (delete-trailing-whitespace)
  (save-excursion (replace-regexp "\n\n\n+" "\n\n"))
  ;; image link
  (save-excursion (replace-regexp "\\./我的笔记-attachments" "../figure/evernote"))
  (save-excursion (replace-regexp "file:image/\\(.+\\)" "../figure/evernote/\\1"))
  (save-excursion (replace-regexp "\\[\\[.+\\]\\[\\[\\[../figure/evernote/\\([a-zA-Z0-9-_\\.]+\\)\\]\\]\n\\]\\]" "\n\n[[../figure/evernote/\\1]]\n\n"))
  (save-excursion (replace-regexp "^[\\* ]+\\[\\[../figure/evernote/\\(.+\\)\\]\\]\\*?" "[[../figure/evernote/\\1]]"))
  ;; links
  (save-excursion (replace-regexp "\\[\\[\\]\\[\\(.+\\)\\]\\]" "\\1"))
  ;; tag
  ;; fig
  (save-excursion (replace-string ".webp" ".jpg"))
  )

(defun wally/org-everorg-save-as-roam-note (title url)
  (let (dst id tags tag)
    (setq dst (f-join org-roam-directory (wally//org-roam-name-file title)))
    (goto-char (point-min))
    (write-region (point-min) (point-max) dst)
    (find-file-noselect dst)
    (with-current-buffer (get-file-buffer dst)
      (goto-char (point-min))
      (save-excursion
        (insert (format ":PROPERTIES:\n:END:\n#+TITLE: %s\n" title)))
      (setq id (org-id-get-create))
      (org-set-property "EVERNOTE_LINK" url)
      (re-search-forward "^\\*" nil t 1)
      (setq tags (org-get-tags))
      (if (not tags)
          (save-excursion
            (while (re-search-forward "^=\\([^=]+\\)=" nil t 1)
              (setq tag (match-string 1))
              (if (or (s-starts-with-p "<" tag) (s-starts-with-p "[" tag))
                  (setq tag (substring tag 1 -1)))
              (setq tags (add-to-list 'tags tag)))))
      (org-set-tags tags)
      (save-buffer)
      )
    (message "add roam note: %s" dst)
    dst))

(defun wally/org-convert-evernote-with-evernote2md ()
  (interactive)
  (let* ((default-directory (expand-file-name "~/Desktop"))
         (src (f-join default-directory "我的笔记.enex"))
         (dst-dir (f-join default-directory "notes"))
         (attachment-dir (f-join dst-dir "image"))
         (fig-dir (expand-file-name (f-join wally-journal-dir "figure" "evernote/")))
         f
         (cmd (format "evernote2md %s" src)))
    (if (not (f-exists-p src))
        (error "no note"))
    (if (f-exists-p dst-dir)
        (f-delete dst-dir t))
    (shell-command cmd nil nil)
    (dolist (f (directory-files dst-dir))
      (if (s-ends-with-p ".md" f)
          (setq f (f-join dst-dir f))))
    ;; (if (not f)
    ;;     (error "no converted org note"))
    (if (f-exists-p attachment-dir)
        (dolist (f (directory-files attachment-dir))
          (if (not (s-starts-with-p "." f))
              (f-move (f-join attachment-dir f) fig-dir))))
    ))

(defun wally//evernote-get-note-list ()
  (let ((evernote-index-file "/Users/wally/Wally/Journal/dice/evernote.org")
        notes)
    (find-file-noselect evernote-index-file)
    (with-current-buffer (get-file-buffer evernote-index-file)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(https:..app.yinxiang.+\\)\\]\\[\\(.+\\)\\]\\]" nil t)
        (add-to-list 'notes (cons (match-string-no-properties 1) (match-string-no-properties 2)))))
    notes))


(setq wally-evenote-list (wally//evernote-get-note-list))

(defun wally//evernote-query-uri (title)
  (let ((notes wally-evenote-list)
        uri
        dup)
    (dolist (note notes)
      (when (or (s-equals-p (cdr note) title)
                (s-equals-p (wally//str-replace-special-characters (cdr note))
                            (wally//str-replace-special-characters title))
                (s-equals-p (if (length> title 13) (substring title 2 12) "x")
                            (if (length> (cdr note) 13) (substring (cdr note) 2 12) ""))
                )
        (if (not uri)
            (setq uri (car note))
          (setq dup t)
          (message "duplicate note: %s" title))))
    (if dup
        (setq uri nil))
    uri))

(defun wally/org-roam-batch-import-evernote-notes ()
  (interactive)
  (let ((rootdir "~/Desktop/notes")
        (fail-note-dir "~/Desktop/note")
        (figdir "~/Wally/Journal/assets/evernote")
        cmd
        title
        uri
        roam-note)
    (if (not (f-exists-p fail-note-dir))
        (mkdir fail-note-dir))
    (dolist (f (f-files (f-join rootdir "image")))
      (if (s-ends-with-p ".webp" f)
          (progn
            (setq cmd (format "dwebp %s -o %s" f (f-join figdir (format "%s.jpg" (f-base f)))))
            (shell-command cmd)
            (move-file-to-trash f)
            )
        (f-move f (f-join figdir (f-filename f)))))
    (dolist (f (f-files rootdir))
      (when (s-ends-with-p ".md" f)
        (message "handling %s" f)
        (with-temp-buffer
          (insert-file-contents f)
          (shell-command-on-region (point-min) (point-max)
                                   "pandoc -f markdown -t org" t t)
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^\\*" nil t 1)
          (setq title (org-get-heading t t t t))
          (if (not title)
              (message "FALTA: fail to get title: %s" title)
            (setq uri (wally//evernote-query-uri title))
            (if (not uri)
                (message "FALTA: no note<%s>" title)
              (wally/org-everorg-post-process)
              (setq roam-note (wally/org-everorg-save-as-roam-note title uri))
              (kill-new roam-note)
              (move-file-to-trash f))))))
    (dolist (f (f-files rootdir))
      (when (s-ends-with-p ".md" f)
        (f-move f (f-join fail-note-dir (f-filename f))))))
  )

(defun wally//org-random-subheading (&optional match)
  (interactive)
  (let ((line-beg (line-number-at-pos)))
    (org-goto-line (seq-random-elt (org-map-entries (lambda ()
                                                      (+ (1- line-beg) (line-number-at-pos)))
                                                    match 'tree)))))


(defun wally//dice-roam-note (count)
  (let ((note-dir org-roam-directory)
        (total-num 0)
        notes note)
    (dolist (f (directory-files note-dir))
      (when (s-ends-with-p ".org" f)
        (setq total-num (1+ total-num))
        (add-to-list 'notes f)))
    (while (> count 0)
      (setq note (nth (random total-num) notes))
      (setq note (f-join note-dir note))
      (find-file-noselect note)
      (with-current-buffer (get-file-buffer note)
        (goto-char (point-min))
        (re-search-forward "^\\* " nil t 1)
        (replace-match "* TODO [#C] ")
        (message "INFO roam note selected: %s" (buffer-substring (line-beginning-position) (line-end-position)))
        (org-schedule nil "+1d")
        (save-buffer)
        )
      (setq count (1- count)))))

(defun wally//org-format-id-with-protocal (id title)
  (format "[[id:%s][%s]]  [[org-protocol://org-id?id=%s][op]]" id title id)
  )

(defun wally/org-search-reference ()
  (interactive)
  (let ((ref-buf (get-buffer "reference.org")))
    (switch-to-buffer ref-buf)
    (org-match-sparse-tree)))

(defun wally/org-heading-to-html(filepath)
  (let ((temp-file (format "%s.org" filepath)))
    (org-mark-subtree)
    (write-region (region-beginning) (region-end) temp-file)
    (shell-command (format "pandoc %s -f org -t html -o %s" temp-file filepath))
    (deactivate-mark)))

(defun wally/pandoc-markdown-to-org-inplace()
  (interactive)
  (let* ((filename (buffer-file-name))
         (new-filename (format "%s.org" filename))
         )
    (shell-command-on-region (point-min) (point-max)
                             "pandoc -f markdown -t org" t t)
    (save-buffer)
    (rename-file filename new-filename)
    (set-visited-file-name new-filename)
    (set-buffer-modified-p nil)
    (org-mode)
    (goto-char (point-min))
    (while (re-search-forward "^ *:PROPERTIES:" nil t)
      (org-mark-element)
      (delete-region (region-beginning) (region-end)))
    (setq-local pangu-spacing-real-insert-separtor nil)
    (save-buffer)))

(defun wally/org-convert-region-to-md ()
  (interactive)
  (let ((string (buffer-substring (region-beginning) (region-end))))
    (with-temp-buffer
      (insert string)
      (let ((org-inhibit-startup t))
        (org-mode))
      (kill-new (org-export-as 'md)))))

(defun org-id-protocol-goto-org-id (info)
  "This handler simply goes to the org heading with given id using emacsclient.

      INFO is an alist containing additional information passed by the protocol URL.
      It should contain the id key, pointing to the path of the org id.

        Example protocol string:
        org-protocol://org-id?id=309A0509-81BE-4D51-87F4-D3F61B79EBA4"
  (when-let ((id (plist-get info :id)))
    (org-id-goto id))
  nil)

(defun wally/spacemacs-insert-function-list-under-current-heading()
  (interactive)
  (wally/org-insert-sub-headings (wally/spacemacs-get-function-list-under-current-heading)))

(defun wally/spacemacs-get-function-list-under-current-heading()
  (message "Enter wally/spacemacs-get-function-list-under-current-heading")
  (let ($func-list (pattern "^ *(defun \\([^()]+\\)(.*)$"))
    (org-narrow-to-subtree)
    (save-excursion (goto-char (point-max))
                    (save-match-data (while (re-search-backward pattern nil t)
                                       (add-to-list '$func-list (match-string-no-properties 1)))))
    (widen)
    (message "Exit wally/spacemacs-get-function-list-under-current-heading") $func-list))

;; 功能重合
(defun wally/spacemacs-get-custom-tail-config ()
  (let ((dotspacemacs "~/.spacemacs")
        custom-config)
    (setq custom-config (shell-command-to-string
                         (format "sed -ne '/^.defun dotspacemacs.emacs-custom-settings/,$ p' %s" dotspacemacs)))
    custom-config))


(defun wally/spacemacs-babel-tangle ()
  (let ((dotspacemacs "~/.spacemacs")
        (custom-config (wally/spacemacs-get-custom-tail-config)))
    (f-copy dotspacemacs (format-time-string "~/.spacemacs.%Y%m%d%H%M%S" (current-time)))
    (org-babel-tangle)
    (find-file-noselect dotspacemacs)
    (with-current-buffer (get-file-buffer dotspacemacs)
      (goto-char (point-min))
      (if (re-search-forward "^.defun dotspacemacs.emacs-custom-settings" nil t 1)
          nil
        (goto-char (point-max))
        (insert "\n")
        (insert custom-config)
        (save-buffer)))))

(defun wally/anki-archive-org-heading()
  (interactive)
  (message "arch note heading to anki")
  (let* ((temp-arch-file "/tmp/a.org")
         (org-archive-location (format "%s::** pros" temp-arch-file)) head-outline content)
    (write-region
     "* ITEM\n:PROPERTIES:\n:ANKI_DECK: Notes\n:ANKI_NOTE_TYPE: Note\n:END:\n\n** pros\n\n** cons\n"
     nil temp-arch-file)
    (save-excursion (if (not (= (char-after) ?*))
                        (outline-previous-heading))
                    (setq head-outline (org-format-outline-path (org-get-outline-path t)))
                    (org-mark-subtree)
                    (setq content
                          (buffer-substring
                           (region-beginning)
                           (region-end)))
                    (org-archive-subtree)
                    (evil-undo 1))
    (find-file temp-arch-file)
    (org-mode)
    (goto-char (point-min))
    (search-forward "** pros")
    (next-line)
    (insert (format "#+OPTIONS: ^:nil\n%s\n" head-outline))
    (anki-editor-mode)
    (anki-editor-push-notes)
    ;; (erase-buffer)
    (save-buffer)
    (kill-buffer)))

(setq wally-anki-customed-deck nil)

(defun wally/anki-helm-custom-deck()
  (interactive)
  (let (source)
    (setq source '((name . "anki decks")
                   (candidates . ( "INBOX"))
                   (action . (lambda(candidate)
                               (setq wally-anki-customed-deck candidate)))))
    (helm-other-buffer 'source "anki decks")))

(defun wally/mindmap-add(title)
  (interactive "sTITLE: ")
  (let* ((index-file (f-join wally-mindmap-dir "index.org"))
         (template (expand-file-name "~/.local/share/.mindmap.org")))
    (setq wally-mindmap-title title wally-mindmap-filename (s-trim (shell-command-to-string
                                                                    "mktemp -u XXXXXXXX")))
    (find-file index-file)
    (goto-char (point-min))
    (re-search-forward "^\\*\\* " nil t 1)
    (beginning-of-line)
    (newline 2)
    (previous-line)
    (insert "mindmap-template")
    (yas-expand)))

(defun wally/mindmap-substitude-org-linkis-with-description-in-region()
  "利用 orgmode 自动生成目录的功能做小结"
  (interactive)
  (save-excursion (narrow-to-region (region-beginning)
                                    (region-end))
                  (goto-char (point-min))
                  (while (re-search-forward "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" nil t 1)
                    (replace-match "\\2"))
                  (widen)))

(defun wally/mindmap-convert()
  (interactive)
  (let (content filename)
    (org-mark-subtree)
    (setq content
          (buffer-substring
           (region-beginning)
           (region-end)))
    (with-temp-buffer (insert "%s" content)
                      (goto-char (point-min))
                      (delete-region (line-beginning-position)
                                     (line-end-position))
                      (re-search-forward "http.+\\([a-zA-Z0-9_]\\{8\\}\\)\\.xmind" nil t 1)
                      (setq filename (f-join wally-mindmap-dir (format "%s.org" (match-string 1))))
                      (delete-region (line-beginning-position)
                                     (line-end-position))
                      (replace-regexp "^- " "#+TITLE: " nil (point-min)
                                      (point-max))
                      (replace-regexp "^  - " "* " nil (point-min)
                                      (point-max))
                      (replace-regexp "^    - " "** " nil (point-min)
                                      (point-max))
                      (replace-regexp "^      - " "*** " nil (point-min)
                                      (point-max))
                      (goto-char (point-min))
                      (flush-lines "^$")
                      (write-region (point-min)
                                    (point-max) filename)
                      (find-file filename)
                      (org-freemind-export-to-freemind))))

(defun wally/org-convert-evernote-with-evernote2md ()
  (let* ((default-directory (expand-file-name "~/Desktop"))
         (src (f-join default-directory "我的笔记.enex"))
         (dst-dir (f-join default-directory "notes"))
         (attachment-dir (f-join dst-dir "image"))
         (fig-dir (expand-file-name (f-join wally-journal-dir "figure" "evernote/")))
         f
         (cmd (format "evernote2md %s" src)))
    (if (not (f-exists-p src))
        (error "no note"))
    (if (f-exists-p dst-dir)
        (f-delete dst-dir t))
    (shell-command cmd nil nil)
    (dolist (f (directory-files dst-dir))
      (if (s-ends-with-p ".md" f)
          (setq f (f-join dst-dir f))))
    (if (not f)
        (error "no converted org note"))
    (if (f-exists-p attachment-dir)
        (dolist (f (directory-files attachment-dir))
          (if (not (s-starts-with-p "." f))
              (f-move (f-join attachment-dir f) fig-dir))))
    f))



(defun wally//evernote-get-note-list ()
  (let ((evernote-index-file "/Users/wally/Wally/Journal/dice/evernote.org")
        notes)
    (find-file-noselect evernote-index-file)
    (with-current-buffer (get-file-buffer evernote-index-file)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(https:..app.yinxiang.+\\)\\]\\[\\(.+\\)\\]\\]" nil t)
        (add-to-list 'notes (cons (match-string-no-properties 1) (match-string-no-properties 2)))))
    notes))


(defun wally//evernote-query-uri (title)
  (let ((notes (wally//evernote-get-note-list))
        uri
        dup)
    (dolist (note notes)
      (when (or (s-equals-p (cdr note) title)
                (s-equals-p (wally//str-replace-special-characters (cdr note))
                            (wally//str-replace-special-characters title))
                (s-equals-p (if (length> title 13) (substring title 2 12) "x")
                            (if (length> (cdr note) 13) (substring (cdr note) 2 12) ""))
                )
        (if (not uri)
            (setq uri (car note))
          (setq dup t)
          (message "duplicate note: %s" title))))
    (if dup
        (setq uri nil))
    uri))

(defun wally/org-roam-batch-import-evernote-notes ()
  (interactive)
  (let ((rootdir "~/Desktop/notes")
        (fail-note-dir "~/Desktop/note")
        (figdir "~/Wally/Journal/assets/evernote")
        cmd
        title
        uri
        roam-note)
    (if (not (f-exists-p fail-note-dir))
        (mkdir fail-note-dir))
    (dolist (f (f-files (f-join rootdir "image")))
      (if (s-ends-with-p ".webp" f)
          (progn
            (setq cmd (format "dwebp %s -o %s" f (f-join figdir (format "%s.jpg" (f-base f)))))
            (shell-command cmd)
            (move-file-to-trash f)
            )
        (f-move f (f-join figdir (f-filename f)))))
    (dolist (f (f-files rootdir))
      (when (s-ends-with-p ".md" f)
        (message "handling %s" f)
        (with-temp-buffer
          (insert-file-contents f)
          (shell-command-on-region (point-min) (point-max)
                                   "pandoc -f markdown -t org" t t)
          (org-mode)
          (goto-char (point-min))
          (re-search-forward "^\\*" nil t 1)
          (setq title (org-get-heading t t t t))
          (if (not title)
              (message "FALTA: fail to get title: %s" title)
            (setq uri (wally//evernote-query-uri title))
            (if (not uri)
                (message "FALTA: no note<%s>" title)
              (wally/org-everorg-post-process)
              (setq roam-note (wally/org-everorg-save-as-roam-note title uri))
              (kill-new roam-note)
              (move-file-to-trash f))))))
    (dolist (f (f-files rootdir))
      (when (s-ends-with-p ".md" f)
        (f-move f (f-join fail-note-dir (f-filename f))))))
  )

(defun wally/evernote-convert-url()
  (interactive)
  (let ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^<li>.+evernote....view/\\([0-9]+\\)/\\([a-z0-9]+\\)/\\([0-9a-z-]+\\)/[^>]+>\\(.+\\)<.a><.li>$" nil t 1)
        (replace-match "** [[https://app.yinxiang.com/shard/\\2/nl/\\1/\\3/][\\4]]")))))


(defun wally/evernote-load-new-items(src)
  (interactive "Fsrc: ")
  (let ((script "/home/wally/Wally/Journal/utils/evernote-url-converter.py")
        (dst (f-join wally-dice-dir "inbox" "evernote.org")))
    (shell-command (format "python3 %s %s %s" script src dst))))

(defun wally/math-auto-increase-following-numbers()
  (interactive)
  (let (number)
    (save-excursion (while (s-numeric-p (word-at-point))
                      (if (not number)
                          (setq number (number-at-point)))
                      (wally/text-replace-word-at-point (format "%d" number))
                      (setq number (+ number 1))
                      (next-line)))))


(defvar pdf-sel-mode-map nil
  "Keymap for `pdf-sel-mode'.")

(setq pdf-sel-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map [double-mouse-1] 'pdf-sel-mouse)
        map))

(define-minor-mode pdf-sel-mode
  "\\<pdf-sel-mode-map>Just binding \\[pdf-sel-mouse] to `pdf-sel-mouse'.
`pdf-sel-mouse' selects the text at point and copies it to `kill-ring'."
  :keymap pdf-sel-mode-map)

(defvar pdf-view-active-region) ;; defined in "pdf-view.el"

(defun pdf-sel-mouse (ev)
  "Select word at mouse event EV and copy it to `kill-ring'."
  (interactive "@e")
  (let* ((posn (event-start ev))
         (xy (posn-object-x-y posn))
         (size (pdf-view-image-size))
         (page (pdf-view-current-page))
         (x (/ (car xy) (float (car size))))
         (y (/ (cdr xy) (float (cdr size)))))
    (setq pdf-view-active-region (pdf-info-getselection page (list x y x y) 'word))
    (pdf-view-display-region pdf-view-active-region)
    (kill-new (pdf-info-gettext page (list x y x y) 'word))))

(defun get-ip-address (&optional dev)
  "Get the IP-address for device DEV (default: eth0) of the current machine."
  (let ((dev (if dev dev "eth0")))
    (format-network-address (car (network-interface-info dev)) t)))

(defun wally/image-shrink-current()
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position)))
        (img-root (expand-file-name "~/Wally/Journal/figure"))
        (filepath))
    (save-match-data
      (and (string-match ".*figure.\\(.*\\).\\(png\\|jpg\\).*" line)
           (setq filepath (format "%s/%s.%s" img-root (match-string 1 line) (match-string 2 line)))))
    (if (and filepath (file-exists-p filepath))
        (shell-command (format "convert -resize 50%%x50%% %s %s" filepath filepath) nil))))

(setq wally-picture-root
      (if (not (string-equal system-type "gnu/linux"))
          (expand-file-name "~/Pictures") ; mac
        (expand-file-name "~/Picture")))

(defun wally/image-download-at-point(root)
  (let ((pattern "\\[\\(.*\\)\\[\\(.+\\)\\]\\]")
        (line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        url
        dst
        )
    (cond ((string-match pattern line)
           (setq url (match-string 2 line)))
          ((string-equal "http" (substring line 0 4))
           (setq url line))
          ((string-equal "file" (substring line 0 4))
           (setq url line))
          )
    (when url
      (setq dst (wally/file-mktemp-file (f-ext url) root))
      (url-copy-file url dst)
      (message "download to <%s> from <%s>" dst url)
      (with-temp-buffer
        (insert (format "%s\t%s\n" dst url))
        (append-to-file (point-min) (point-max) (f-join wally-picture-root "log.txt"))))))

(defun wally/image-download()
  (interactive)
  (let (source)
    (setq source '((name . "image subdir")
                   (candidates . ("wallpapers" "gif" "untitled"))
                   (action . (lambda(candidate)
                               (wally/image-download-at-point
                                (f-join  wally-picture-root candidate))))))
    (helm-other-buffer 'source "download image at point")))

(defun wally/img-render-from-page(url dst &optional tag)
  (let* ((render (expand-file-name "~/.local/bin/render-page"))
         (cmd (format "%s %s %s" render url dst)))
    (if tag
        (setq cmd (format "%s %s" cmd tag)))
    (shell-command cmd)))

(defun wally/image-convert-webp-at-point()
  (interactive)
  (let ((pattern "\\[\\(\\[.+]\\)?\\[\\(file:\\)?\\(.+webp\\)\\]\\]")
        src
        dst
        cmd
        )
    (beginning-of-line)
    (when (re-search-forward pattern (line-end-position) t 1)
      (setq src (match-string 3)
            dst (f-join (f-dirname src) (format "%s.png" (f-base src)))
            cmd (format "dwebp %s -o %s" src dst)
            )
      (message "run <%s>" cmd)
      (shell-command cmd)
      (beginning-of-line)
      (replace-string "webp" "png" nil (line-beginning-position) (line-end-position))
      (message "replace webp with png"))))

(setq wally-video-upper-limit 100000000) ; 100M
(setq wally-video-root "~/Video/collections")

(defun wally/video-download()
  (interactive)
  (find-file wally-video-note)
  (org-map-entries '(lambda ()
                      (let ((title (org-get-heading))
                            (url (org-entry-get nil "URL"))
                            (size (string-to-number (org-entry-get nil "SIZE")))
                            (filename (org-entry-get nil "FILENAME")) filepath)
                        (setq filepath (f-join wally-video-root filename))
                        (if (f-exists-p (format "%s.mp4" filepath))
                            (message "video <%s> already exists." filepath)
                          (if (> size wally-video-upper-limit)
                              (message "skip big video <%s>" title)
                            (message "downloading video <%s>" title)
                            (async-shell-command (format "you-get %s -O %s" url filepath))))))
                   "LEVEL=2"))

(defun wally/video-update-info()
  (interactive)
  (find-file wally-video-note)
  (goto-char (point-min))
  (org-map-entries '(lambda ()
                      (let ((title (org-get-heading))
                            (url (org-entry-get nil "URL"))
                            (size (org-entry-get nil "SIZE")))
                        (if (not size)
                            (progn (message "updating info for <%s>" title)
                                   (setq size (wally/video-you-get-video-info url))
                                   (org-set-property "SIZE" size))))) "LEVEL=2"))

(defun wally/video-import-bilibi(page)
  (interactive "Fpath to page: ")
  (let ((existing-urls (wally/video-fetch-existing-url)))
    (dolist (record (wally/video-parse-bilibili-favorite-page page))
      (setq wally-video-title (nth 0 record) wally-video-url (nth 1 record))
      (if (not (member wally-video-url existing-urls))
          (progn (message "add video <%s>" wally-video-title)
                 (wally/video-add-item))))))

(defun wally/video-add-item()
  (setq wally-video-filename (s-trim (shell-command-to-string "mktemp -u XXXXXXXX")))
  (find-file wally-video-note)
  (goto-char (point-min))
  (re-search-forward "^\\*\\*" nil t 1)
  (beginning-of-line)
  (insert (format "%s" "video-template"))
  (yas-expand))

(defun wally/video-parse-bilibili-favorite-page(filepath)
  (let* ((root (with-temp-buffer (insert-file-contents filepath)
                                 (libxml-parse-html-region (point-min)
                                                           (point-max)))) title url result)
    (dolist (record (dom-by-class root "small-item"))
      (setq title (dom-attr (car (dom-by-class record "title")) 'title) url (dom-attr (car
                                                                                       (dom-by-tag
                                                                                        record 'a))
                                                                                      'href))
      (add-to-list 'result (list title (format "https:%s" url)))) result))

(defun wally/video-fetch-existing-url()
  (let (urls)
    (with-temp-buffer (insert-file-contents wally-video-note)
                      (org-mode)
                      (org-map-entries '(lambda ()
                                          (add-to-list 'urls (org-entry-get nil "URL"))) "LEVEL=2"))
    urls))

(defun wally/video-you-get-video-info(url)
  (let (size)
    (with-temp-buffer (shell-command (format "you-get -i %s" url) t)
                      ;; (message "%s" (buffer-string))
                      (goto-char (point-min))
                      (re-search-forward "\\([0-9]+\\) bytes" nil t 1)
                      (setq size (match-string 1)))
    (message "%s bytes" size) size))


(defun wally/video-get-info (filepath)
  (let (($info (make-hash-table :test 'equal))
        duration
        ff-output)
    (setq ff-output (shell-command-to-string (format "ffmpeg -i %s" filepath)))
    (string-match "Duration: \\([0-9:]+\\)" ff-output)
    (setq duration (match-string 1 ff-output))
    (puthash "duration" duration $info)
    (puthash "size" (file-size-human-readable (f-size filepath)) $info)
    (puthash "checksum" (f-size filepath) $info)
    (message "%s" $info)
    $info))

(defun wally/evil-rmv ()
  "remove evil directory at point"
  (interactive)
  (let* ((filepath (wally/org-heading-get-link)))
    (if (not filepath)
        (error "error: %s" filepath))
    (if (f-file-p filepath)
        (setq filepath (f-parent filepath)))
    (when (= 8 (length (nth 0 (reverse (f-split filepath)))))
      (message "remove %s" filepath)
      (f-delete filepath t))))

(defun wally/evil-rmv-duplicate ()
  "remove duplicate of current item by org-id"
  (interactive)
  (let ((id (org-entry-get nil "ID"))
        start end)
    (save-excursion
      (search-forward id nil t 2)
      (org-mark-subtree)
      (setq start (region-beginning)
            end (region-end))
      (message "%s %s %s" id start end)
      (if (> (- end start) 500)
          (error "region too large. quit")
        (message "deleting at %d" start)
        (delete-region start end)))))

(defun wally/evil-meta ()
  "add meta info of current item"
  (interactive)
  (let ((filepath (org-element-property :path (org-element-context)))
        info
        (artist (org-entry-get nil "ARTIST"))
        (rate (org-entry-get nil "RATE"))
        (area (org-entry-get nil "AREA"))
        (desc (org-entry-get nil "DESC"))
        (designation (org-entry-get nil "DESIGNATION"))
        (producer (org-entry-get nil "PRODUCER"))
        (level (org-entry-get nil "XLEVEL"))
        )
    (if (not (f-exists-p filepath))
        (error "file %s does not exist." filepath))
    (setq info (wally/video-get-info filepath))
    (org-set-property "DURATION" (gethash "duration" info))
    (org-set-property "SIZE" (gethash "size" info))
    (org-set-property "CHECKSUM" (format "%s" (gethash "checksum" info)))

    (org-set-property "RATE" (if rate rate 0))
    (org-set-property "ARTIST" (if artist artist "NA") )
    (org-set-property "AREA" (if area area "NA"))
    (org-set-property "DESC" (if desc desc "NA"))
    (org-set-property "DESIGNATION" (if designation designation "NA"))
    (org-set-property "PRODUCER" (if producer producer "NA"))
    (org-set-property "XLEVEL" (if level level "X"))
    )
  )


(defun wally/evil-screenshot ()
  "add  a screenshot of current item"
  (interactive)
  (let* ((root "~/Pictures/Screenshots")
         (fig (car (car (sort (directory-files-and-attributes root  t "^[^\\.].+") #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x)))))))
         )
    (insert (format "[[file:%s]]" fig))))

(defun wally/evil-replace-figure-path()
  (interactive)
  (let (src dst)
    (while (re-search-forward "\\[\\[file:\\(.+Screenshots.+\\)\\]\\]" nil t)
      (setq src (match-string 1)
            dst (make-temp-file "" nil (format ".%s" (f-ext src)))
            dst (concat "../assets/.private/xflm/" (f-filename dst)))
      (when (f-exists-p src)
        (f-move src dst)
        (replace-match (format "file:%s" dst))))))

(defun wally/evil-db-new-item (item)
  (if (emacsql mydb [:select path :from evil :where (= path $s1)] item)
      (message "%s already exists." item)
    (emacsql mydb [:insert :into evil :values ([$s1 0])] item)))

(defun wally/evil-db-upgrade-item (item)
  (let (rate)
    (setq rate (emacsql mydb [:select rate :from evil :where (= path $s1)] item))
    (if (not rate)
        (wally/evil-db-new-item))
    (setq rate (car (car rate)))
    (emacsql mydb [:update evil :set (= rate $s1) :where (= path $s2)] (1+ rate) item)))

(defun wally/evil-db-degrade-item (item)
  (let (rate)
    (setq rate (emacsql mydb [:select rate :from evil :where (= path $s1)] item))
    (if (not rate)
        (wally/evil-db-new-item item))
    (setq rate (car (car rate)))
    (emacsql mydb [:update evil :set (= rate $s1) :where (= path $s2)] (1- rate) item)))

(defun wally/evil-generate-index (subdir leafdir)
  (let* ((root (f-join wally-evil-file-root subdir leafdir))
         (default-directory root)
         (suffixes (if (s-equals-p "images" subdir)
                       (list "png" "jpg" "jpeg" "gif")
                     (list "mp4" "avi" "kmv")))
         (index-org "index.org")
         items)
    ;; check
    (if (not (f-exists-p root))
        (message "root <%s> not exists!" root)
      ;; list files
      (dolist (f (sort (f-files ".") 's-less-p))
        (when (member (f-ext f) suffixes)
          (add-to-list 'items (f-filename f))))
      (setq items (reverse items))
      ;; generate index.org
      (find-file-noselect index-org)
      (with-current-buffer (get-file-buffer index-org)
        (erase-buffer)
        (insert (format "[[%s/evil/trash/%s/%s][rmv]]" wally-emacs-server-proxy subdir leafdir))
        (insert (format "\t\t[[%s/evil/like/%s/%s][like]]\n\n\n\n" wally-emacs-server-proxy subdir leafdir))
        (dolist (item items)
          (insert (format "file:%s/%s/%s/%s\n\n" wally-evil-file-server subdir leafdir item))
          (insert (format "[[%s/evil/trash/%s/%s?leaf=%s][rmv]]" wally-emacs-server-proxy subdir leafdir item))
          (insert (format "\t\t[[%s/evil/like/%s/%s?leaf=%s][like]]\n\n" wally-emacs-server-proxy subdir leafdir item))
          )
        (save-buffer)
        (org-html-export-to-html))
      ;; update mydb
      (wally/evil-db-new-item (format "%s/%s" subdir leafdir))
      (format "%s/%s/%s" wally-evil-file-server subdir leafdir))))


(defun wally/finance-convert-orgheading-to-ledger-item ()
  (interactive)
  (if (not (org-at-heading-p))
      (error "not a org heading"))
  (let* ((heading  (nth 4 (org-heading-components)))
        (pattern "\\(.+\\) +:\\$\\(.+\\):\\$\\(.+\\):")
        (date (decode-time (org-get-scheduled-time nil)))
        (value (org-entry-get nil "VALUE"))
        dest
        src
        dst
        first-account
        second-account
        (alias (make-hash-table :test 'equal))
        snippet
        (ledger (f-join wally-journal-dir "private" "account.ledger.gpg"))
        )
    (dolist (pair (list '("weixin" . "Assets:Checking:WEIXIN")
                        '("zhaohang" . "Assets:Checking:CMB")
                        '("yuebao" . "Assets:Checking:ALIPAY")
                        '("gonghang" . "Assets:Checking:ICBC")
                        '("huabei" . "Liabilities:Ant:HUABEI")
                        '("food" . "Expense:Diet:Food")
                        '("taxi" . "Expense:Travelling:Taxi")
                        '("wenju" . "Expense:Learnging:Stationery")
                        '("salary" . "Income:Salary")
                        '("subway" . "Expense:Travelling:PublicTransport")
                        '("waterelectricity" . "Expense:Shelter:WaterElectricity")
                        '("@" . "Expense:Socializing:Love")
                        '("julie" . "Creditors:Friends:Julie")
                        '("clothes" . "Expense:Clothing:Clothes")
                        '("mobile" . "Expense:Communication:CallCharge")
                        '("baitiao" . "Liabilities:JD:BAITIAO")
                        '("taxes" . "Expense:Others:Taxes")
                        '("haircut" . "Expense:Clothing:Haircut")
                        '("commodity" . "Expense:Shelter:DailySupplies")
                        '("it" . "Expense:Technique:IT")
                        '("treatment" . "Expense:Health:Treatment")
                        '("railway" . "Expense:Travelling:Railway")
                        '("" . "")
                        ))
      (puthash (car pair) (cdr pair) alias))
    (if (not (string-match pattern heading))
        (error "invalid org heading"))
    (setq desc (match-string 1 heading)
          src (match-string 2 heading)
          dst (match-string 3 heading))
    (setq src (gethash src alias)
          dst (gethash dst alias))
    ;; most cases
    (setq first-account src
          second-account dst)
    (cond
     ((s-starts-with-p "Expense" src)
      nil)
     ((s-starts-with-p "Expense" dst)
      (setq first-account dst
            second-account src))
     ;; 工资收入等
     ((and (s-starts-with-p "Assets" src) (s-starts-with-p "Income" dst))
      nil
      )
     ;; 转账
     ((and (s-starts-with-p "Assets" src) (s-starts-with-p "Assets" dst))
      (setq value (concat "-" value)))
     ;; 信用卡还款
     ((and (s-starts-with-p "Assets" src) (s-starts-with-p "Liabilities" dst))
      (setq value (concat "-" value)))
     ;; 信用卡借款
     ((and (s-starts-with-p "Liabilities" src) (s-starts-with-p "Assets" dst))
      (setq value (concat "-" value)))
     ;; 借钱
     ((and (s-starts-with-p "Assets" src) (s-starts-with-p "Creditors" dst))
      (setq value (concat "-" value)))
     ;; 不支持的情况
     (t
      (error "invalid org-heading"))
     )
    (setq snippet (format "\n%4d-%02d-%02d %s\n    %s  %s CNY\n    %s\n" (nth 5 date) (nth 4 date) (nth 3 date) desc
                          first-account value second-account))
    (find-file-noselect ledger)
    (with-current-buffer (get-file-buffer ledger)
      (goto-char (point-max))
      (insert snippet)
      (save-buffer))
    (org-archive-subtree)
    (save-buffer)
    (message "add ledger item: %s(%s)" desc value)))

(defun wally/source-goto-tag-at-point()
  (interactive)
  (let ((entry-file "entry.c")
        (tag (word-at-point)))
    (find-file entry-file)
    (helm-gtags-find-tag tag)))

(defun wally/func-get-name-at-point()
  (let ((pattern "^ *(defun \\([^()]+\\)(.*)$") func-name)
    (save-excursion (save-match-data (if (re-search-backward pattern nil t 1)
                                         (setq func-name (match-string-no-properties 1)))))
    func-name))

(defun wally/func-insert-entry-info-at-point()
  (interactive)
  (let ((func-name (wally/func-get-name-at-point)))
    (when func-name (insert (format "(message \"Enter %s\")" func-name))
            (indent-for-tab-command)
            (insert (format "\n(message \"Exit %s\")" func-name))
            (indent-for-tab-command))))

(defun wally/git-auto-save-repos()
  (interactive)
  (let ((repos '("~/Wally/logseq")))
    (dolist (repo repos)
      (let ((default-directory repo))
        (shell-command "git add -u")
        (shell-command "git commit -m 'chore: ci'")
        (message "git commit in %s" default-directory)))))

(defun wally/helm-gitignore()
  (interactive)
  (let ((ignore-dir (expand-file-name "~/Project/gitignore"))
        ignore-files
        ignore-candidates
        source
        )
    (setq ignore-files (f-files ignore-dir))
    (dolist (filename ignore-files)
      (add-to-list 'ignore-candidates (f-base filename)))
    (setq ignore-candidates (reverse ignore-candidates))
    (setq source '((name . "git ignores")
                   (candidates . ignore-candidates)
                   (action . (lambda(candidate)
                               (switch-to-buffer "*gitignore*")
                               (erase-buffer)
                               (insert-file-contents (format "%s/%s.gitignore" ignore-dir candidate))))
                   ))
    (helm-other-buffer 'source "helm gitignore")))

(defun wally/git-get-current-commit-id ()
  (interactive)
  (let ((cmt (shell-command-to-string "git rev-parse HEAD")))
    (setq cmt (s-trim cmt))
    (kill-new cmt)
    (message "%s" cmt)))

(defun wally/git-get-current-message ()
  (interactive)
  (let ((msg (shell-command-to-string "git log -1 --pretty=%B")))
    (setq msg (s-trim msg))
    (kill-new msg)
    (message "%s" msg)))

(defun wally/leetcode-crawl-problem(problem)
  (interactive "sTitle: ")
  (async-shell-command (format "python3 ~/.local/bin/crawl-leetcode %s" problem)))

(defun wally/leetcode-new-leetcode(url)
  (interactive "sURL: ")
  (let* ((proj-dir "~/Project/leetcode")
         (note-file (f-join  proj-dir "README.org"))
         (src-dir (f-join proj-dir "src" "cpp"))
         name target)
    (when (string-match "https://leetcode-cn.com/problems/\\(.+\\)/" url)
      (setq name (match-string 1 url) target (format "%s/%s.cpp" src-dir name))
      (wally/leetcode-crawl-problem name)
      (find-file note-file)
      (wally/leetcode-new-item name)
      (message "%s created" target))))

(defun wally/leetcode-random-one-problem()
  (interactive)
  (browse-url "https://leetcode-cn.com/problems/random-one-question/all"))

(defun wally/leetcode-get-current-problem()
  (interactive)
  (let (problem)
    (save-excursion (next-line)
                    (re-search-backward
                     "^\\*\\* \\(TODO\\|DONE\\|QUIT\\)? ?\\(\\[#[ABC]\\] \\)?\\(.+\\)" nil t 1)
                    (setq problem (substring-no-properties (match-string 3)))
                    (kill-new problem)
                    (message "%s" problem) problem)))

(defun wally/leetcode-cmake()
  (interactive)
  (cmake-ide-run-cmake))

(defun wally/leetcode-compile-project()
  (interactive)
  (cmake-ide-compile))

(defun wally/leetcode-compile()
  (interactive)
  (let ((default-directory cmake-ide-build-dir)
        (target (f-base (f-filename (buffer-file-name))))
        )
    (shell-command (format "make %s" target))))

(defun wally/leetcode-source-done()
  (interactive)
  (let* ((note-file (concat (projectile-acquire-root) "README.org"))
         (filename (f-base (buffer-file-name)))
         (pattern (format "^- \\[ \\] .+%s" filename)))
    (find-file note-file)
    (goto-char (point-min))
    (when (re-search-forward pattern nil t 1)
      (org-ctrl-c-ctrl-c))))


(defun wally/auto-dateval-view()
  (interactive)
  (message "Enter wally/auto-dateval-view")
  (let ((dateval-file (f-join wally-gtd-dir "dateval.org"))
        scores
        (loop 1)
        (tmp-data-file "/tmp/a.dat")
        (plt-script "~/.local/share/dateview.plt")
        )
    (find-file dateval-file)
    (goto-char (point-min))
    (setq scores (org-map-entries
                            '(lambda ()
                               (let ($score))
                               (setq $score (org-entry-get nil "SCORE"))
                               $score)
                            "LEVEL=2"))
    (with-temp-buffer
      (dolist (score scores)
        (insert (format "%d %s %.2f\n" loop score 0.7))
        (setq loop (+ loop 1))
        )
      (write-region (point-min) (point-max) tmp-data-file))
    (shell-command (format "gnuplot -c %s" plt-script))))

(defun wally/auto-misc-tasks ()
  (let ()
    (shell-command "dot_clean /Users/wally")
    ))

(defun wally/auto-routine()
  (interactive)
  (wally/logb-auto-routine)
  (wally/ann-crawl-info)
  (evil-write-all nil)
  (wally/logseq-init-journal)
  (call-interactively 'wally/git-auto-save-repos)
  (call-interactively 'wally/org-agenda-auto-quit-on-timeout)
  (call-interactively 'wally/org-count-days)
  (call-interactively 'wally/daily-report)
  (wally/dice-rouinte)
  (wally/auto-misc-tasks)
  (evil-write-all nil)

  ;; pros snapshot
  ;; (call-interactively 'wally/pros-auto-update-progress-by-subitems)
  ;; (call-interactively 'wally/pros-update-datetime)
  ;; (call-interactively 'wally/pros-snapshot)
  ;; (call-interactively 'wally/pros-concat-progress-images)

  ;; (wally/dice-routine-auto)
  ;; (call-interactively 'wally/auto-dateval-view)
  ;; (call-interactively 'wally/pros-dateval-sync-image)

  (run-at-time "5 sec" nil (lambda()
                             (evil-write-all t)
                             (message "ROUTINE DONE"))))
