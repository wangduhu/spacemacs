
(defun wally/tmp ()
  (interactive)

  (epc:stop-epc wally-epc)
  (setq wally-epc (epc:start-epc "python3.9" (list (expand-file-name "~/Project/empyc/source/epcsrv.py"))))
  (wally/org-convert-jd-detail-to-properties)

  ;; (save-excursion
  ;;   (replace-regexp "^\\* \\*?\\([0-9.:]+\\)\\*?$" "** \\1"))
  ;; (wally/org-export-note 4)
  ;; (wally/extract-jd-kv)
  ;; (wally/org-check-required-properties)
  ;; (wally/video-open-at-piont)
  ;; (wally/org-image-remove-item-at-point)
  ;; (wally/page-fetch-new-item)
  ;; (wally/page-open-at-point-by-external-app)
  ;; (wally/page-update-meta-info)
  ;; (org-map-entries 'wally/org-evenote-export-to-anki
  ;;                  "LEVEL=2"
  ;;                  'tree
  ;;                  )
  ;; (wally/image-collectiong-update-meta-info)
  )



(let ((default-directory "/tmp"))
  (async-shell-command "you-get 'https://www.bilibili.com/video/BV1pc411Z7Ju/' -O 1andzqDJ.mp4")
  )

(f-executable-p)
(f-exists-p "/Users/wally/Wally/Journal/assets/video/1andzqDJ/1andzqDJ.mp4")

(wally/get-sys-clipboard-text)

(f-base "ab.c")

(require 'elquery)

(let (content)
  (dolist (f (f-files "/Volumes/Wally/Media/.party/.private/pages"))
    (message "%s" f)
    (when (s-ends-with-p "html" f)
      (add-to-list 'content (format "
** TODO %s
:PROPERTIES:
:ADD_DATE: 20230809
:SOURCE_URI: %s
:END:" (s-trim (shell-command-to-string "mktemp -u XXXXXXXX")) f))))
  (kill-new (s-join "\n" content)))

;; %s/Wally.Journal.assets.mpv.\([^-]+\)\(.+\)/.snap\/.private\/films\/\1\/\1\2/

;; %s/.icao.me.\([0-9]+.mp4\)\n.+\n.+\n.+\n.+\n.+\n.+\n.+\n.+\n.+\n.+\n.+\n:END:\n/\0\n[[file:~\/Wally\/Journal\/assets\/mpv\/\1.jpg]]\n/

;; %s/^\[\[\/tmp/[[~\/.snap
;; %s/\[\[\/Users\/wally/[[~/

(mpv-play "/tmp/.private/films/b0f86imp/1295_720p.mp4")
(let ((first-day (format "2014, 10, 22"))
      (today (format-time-string "%Y, %m, %d" (current-time))) delta percentage)
  (setq delta (+ 1 (string-to-number (calc-count-days first-day today))) percentage (/ delta 365.0))
  (message "%s" delta))

(defun wally/org-check-required-properties ()
  (let ((keys (org-entry-get nil "REQUIRED_KEYS" t))
        val)
    (dolist (key (s-split " " keys))
      (setq val (org-entry-get nil key))
      (if (or (not val) (s-equals-p val "NA"))
          (error "missing key: %s" key))
      )
    (message "required keys ok")
    )
  )

(setq

 org-download-method 'directory
 org-download-image-dir (concat wally-journal-dir "assets/download")
 org-download-heading-lvl nil
 org-download-timestamp "%Y-%m-%d-%H-%M-%S"
 )


(require 'org-fs-tree)

(require 'easy-kill)
(require 'ssh-deploy)

(boundp 'wally/tmp)
(boundp)
(fboundp (intern "wally/tmp"))
(intern "test")

(type-of 'wally/tmp)

(wally/org-count-days)
(boundp (intern "foobar"))

(wally/anki-init-conn)
(wally/anki-sync-meta-data 1680136725979)
(wally/anki-get-note 1680109156711)
(wally/anki-get-note 1680109029307)
(wally/anki-copy-media "img5202746775959020293.jpg")

(append nil 1)
(system-name)
(setq

 org-agenda-files (append (list (f-join wally-refs-dir "index.org")
                                (f-join wally-journal-dir "data" "reference.org")
                                )
                          (directory-files wally-gtd-dir t ".+\.org")
                          (directory-files wally-note-dir t ".+\.org"))
 )

(toggle-frame-maximized)


(defun wally/tmp ()
  (interactive)
  ;; (wally/pros-weekly-analyse-target (encode-time '(0 0 0 1 9 2023 0 nil 28800)))
  ;; (wally/pros-weekly-summary (encode-time '(0 0 0 23 1 2023 0 nil 28800)))

  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         srcfile
         dstfile
         heading
         filename
         (root (org-entry-get nil "PARENT_DIR" t))
         (priority (org-entry-get nil "PRIORITY"))
         )

    (with-temp-buffer
      (insert line)
      (setq srcfile (org-element-property :raw-link (org-element-context))))
    (if (not srcfile)
        (error "found no file link"))
    (if (s-starts-with-p "file:" srcfile)
        (setq srcfile (substring srcfile 5)))

    (evil-delete-line (line-beginning-position) (line-end-position))
    (delete-blank-lines)
    (org-up-element)
    (evil-delete-line (line-beginning-position) (line-end-position))
    (wally/org-add-new-subheading)

    (setq heading (wally/org-get-heading-no-progress))
    (setq dstfile (f-join root (format "%s.%s" heading (f-ext srcfile))))
    (message "%s %s" srcfile dstfile)
    (f-move srcfile dstfile)

    (org-set-property "FILENAME" (f-filename dstfile))
    (org-set-property "PRIORITY" priority)

    )
  )

(require 'ssh-deploy)
(spacemacs/set-leader-keys "cz" 'ssh-deploy-prefix-map)
(define-key ssh-deploy-prefix-map "f" 'ssh-deploy-upload-handler-forced)


(setq org-refile-targets '(' :maxlevel . 1))

;; __eval__

(defun wally//org-evaluate-rank (score)
  (cond
   ((> score 0.85) "A")
   ((> score 0.75) "B")
   ((> score 0.65) "C")
   (t "D")))

(defun wally/org-task-save-and-refresh-value ()
  "存储heading的VALUE属性值后清零"
  )

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



(defun wally/anki-is-note-existed (title)
  "判断指定title的anki note是否已存在于数据库中，存在返回note id，否则返回nil"
  (wally/with-tmp-anki-db
      (epc:call-sync wally-anki-epc 'query_note (list title))))

(defun wally/logb-retrive-from-scramy (post table handler &optional baseurl)
  (let (
        choice
        url
        mainkey
        )
    (setq choice (epc:call-sync my-epc handler nil))
    (when choice
      (setq url (nth 5 choice))
      (epc:call-sync my-epc 'record (list table url))))

  ;; 写入文件
  ;; (when records
  ;;   (wally/logb-new-post post table choice records baseurl)
  ;;   (message "new post: %s%s" table choice))
  ;; (emacsql-close conn)
  ))

(defun wally/leetcode-goto-target()
  (interactive)
  (let* ((templates '("template.cpp" "template"))
         template
         (target (wally/leetcode-get-current-problem))
         )
    (if (boundp 'project-source-dir)
        (setq default-directory project-source-dir)
      (message "project-source-dir is not specified. use cwd <%s>" default-directory))
    (message default-directory)
    (dolist (temp templates)
      (when (and (not template) (f-exists-p temp))
        (setq template temp)
        (if (f-ext template)
            (setq target (format "%s.%s" target (f-ext template))))
        ))
    (when (not (f-exists-p target))
      (if (not template)
          (message "found no templates when creating new item")
        (f-copy template target)
        (message "%s created" target)))
    (if (not (f-exists-p target))
        (message "no item")
      (find-file target))))
