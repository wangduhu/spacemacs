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


(defvar __projectile__ nil)

(defun wally/projectile-recent-file()
  (interactive)
  (let ((root-dir (projectile-acquire-root))
        (recent-file (car (projectile-recentf-files))))
    (find-file (f-join root-dir recent-file))))

(defun wally/projectile-switch-to-last-readme ()
  (interactive)
  (let ((buffers (buffer-list)))
    (dolist (buffer buffers)
      (when (s-index-of "README.org" (buffer-name buffer))
        (switch-to-buffer buffer)
        (cl-return)))))

(defun wally/projectile-note-list ()
  (let (project-note-paths path)
    (dolist (project projectile-known-projects)
      (setq path (f-join (f-expand project) "README.org"))
      (if (f-exists-p path)
          (add-to-list 'project-note-paths path)))
    project-note-paths))

;; (setq org-agenda-files (append (wally/projectile-note-list) org-agenda-files))


(provide 'wally-pros)
