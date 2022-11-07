(defconst wally-dice-db (expand-file-name "~/Wally/data/db/dice.sqlite3"))
(defvar wally-dice-epc nil)
(defvar wally-dice-epc-srv (expand-file-name "~/Project/empyc/srv/dice/epcsrv.py"))


(defun wally/dice-init-epc-srv ()
  (when wally-dice-epc
    (epc:stop-epc wally-dice-epc)
    (setq wally-dice-epc nil))
  (setq wally-dice-epc (epc:start-epc "python3.9" (list wally-dice-epc-srv))))


(defmacro wally/with-dice-epc (cond &rest body)
  "建立epc连接
TODO 不需要 cond参数，还不会写宏，参考http://0x100.club/wiki_emacs/elisp-macro.html
"
  (declare (indent 1) (debug t))
  (wally/dice-init-epc-srv)
  `(if ,cond
       (progn ,@body)))


(defun wally/dice-epc-dice (table count)
  (epc:call-sync wally-dice-epc 'dice (list table count)))


(defun wally/dice-epc-query(table title)
  (epc:call-sync wally-dice-epc 'query (list table title)))


(defun wally/dice-epc-rate(table recid rate)
  (if (epc:call-sync wally-dice-epc 'rate (list table recid rate))
      (message "rate %d on %s<%d>" rate table recid)))


(defun wally/dice-epc-add-item(table fields)
  (if (epc:call-sync wally-dice-epc 'add_item (list table fields))
      (message "add to table<%s>: %s" table fields)))


(defun wally/dice-make-kv (key val)
  (list (make-symbol (format ":%s" key)) val))


(defun wally/dice-add-item ()
  "将当前heading添加到数据库中"
  (interactive)
  (let ((table (org-entry-get nil "DB_TBL" t))
        (keys (org-entry-get nil "DB_KEYS" t))
        (id-key (org-entry-get nil "DB_ID_KEY" t))
        (title (nth 4 (org-heading-components)))
        val
        fields)
    (if id-key
        (setq fields (append fields (wally/dice-make-kv id-key title))))
    (when keys
      (setq keys (s-split " " keys))
      (dolist (key keys)
        (setq val (org-entry-get nil (upcase key)))
        (if key
            (setq fields (append fields (wally/dice-make-kv key val))))))
    (if (and table fields)
        (wally/dice-epc-add-item table fields))))


(defun wally/dice-rate ()
  (let ((todo-status (org-entry-get nil "TODO"))
        (priority (org-entry-get nil "PRIORITY"))
        (rate 0))
    (cond
     ((s-equals-p "QUIT" todo-status) (setq rate -1))
     ((s-equals-p "DONE" todo-status) (setq rate (- ?C (string-to-char priority)))))
    rate))


(defun wally/dice-archive ()
  "根据优先级和TODO状态刷新dice数据"
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (rate (wally/dice-rate))
        (table (org-entry-get nil "DB_TBL" t))
        (recid (org-entry-get nil "RECID")))
    (when table
      (if recid
          (setq recid (string-to-numer recid))
        (setq recid (wally/dice-epc-query table title)))
      (if (not recid)
          (message "rec not found: %s" title)
        (wally/dice-epc-rate table recid rate)))))


(defun wally/dice-items(items)
  (let (table count choice choices)
    (wally/with-dice-epc t
      (dolist (item items)
        (setq table (car item)
              count (cdr item)
              choice (wally/dice-epc-dice table count))
        (add-to-list 'choices (cons table choice))))
    choices))


(defun wally/dice-update-dashboard (dashboard-id items)
  (let (content)
    (org-id-goto dashboard-id)
    (org-mark-subtree)
    (setq content (buffer-substring-no-properties (region-beginning) (region-end)))
    (with-temp-buffer
      (insert content)
      (org-mode)
      (goto-char (point-min))
      ;; keep properties
      (next-line)
      (org-mark-element)
      (delete-region (region-end) (point-max))
      (deactivate-mark)
      (goto-char (point-max))
      (dolist (item items)
        (insert (format "- [ ] %s\n" item)))
      (setq content (buffer-substring-no-properties (point-min) (point-max)))
      )
    (kill-region (region-beginning) (region-end))
    (insert content)
    (insert "\n")
    (deactivate-mark)
    (save-buffer)))


(defun wally/dice-daily ()
  (let ((candidates '(("csproverb" . 1)
                      ("quotation" . 1)
                      ("lines" . 1)
                      ("readingnote" . 1)
                      ("poem" . 1)
                      ("stagephoto" . 1)
                      ;; ("houlang" . 1)
                      ("song" . 1)
                      ("painting" . 1)
                      ("speech" . 1))))
    (wally/dice-items candidates)))


(defun wally/dice-weekly ()
  (let ((candidates '(("movie" . 1)
                      ("album" . 1)
                      ;; ("documentary" . 1)
                      ("origami" . 1)
                      ("teleplay" . 1)
                      ("prose" . 1))))
    (wally/dice-items candidates)))


(defun wally/dice-monthly ()
  (let ((candidates '(("friends" . 1)
                      ;; ("reading" . 1)
                      )))
    (wally/dice-items candidates)))


(defun wally/dice-rouinte ()
  (interactive)
  (let* ((date (decode-time (current-time)))
         (weekday (nth 6 date))
         (monthday (nth 3 date)))
    (when (equal monthday 1)
      (wally/dice-update-dashboard "423825F1-6E75-4684-9886-863E7198FD22" (wally/dice-monthly)))
    (when (equal weekday 0)
      (wally/dice-update-dashboard "2323282C-640E-40B5-9A79-B35ACDBA86BF" (wally/dice-weekly)))
    (wally/dice-update-dashboard "8DAB48D4-1890-49DC-BACF-95DE275310A2" (wally/dice-daily))))


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
        (save-buffer))
      (setq count (1- count)))))


(defun wally/evil-view ()
  (interactive)
  (let* ((path (wally/org-heading-get-link))
         (leafdir (nth 1 (reverse (f-split path))))
         (subdir (nth 2 (reverse (f-split path))))
         )
    (message "%s %s %s" path subdir leafdir)
    (when (f-exists-p (f-join wally-evil-file-root subdir leafdir))
      (wally/evil-generate-index subdir leafdir)
      (browse-url (format "%s/%s/%s" wally-evil-file-server subdir leafdir)))))

(defun wally//dice-evil ()
  (let (candidates)
    (dolist (cursor '(
                      ("529B65F1-24F6-4795-A6C9-B9A73E59C421" . 8) ; ximg
                      ("AE658D47-D92D-47E0-864A-4C1DA99CA897" . 5) ; xsht
                      ("726CE563-13E0-43EB-B8DF-AD8C678B9341" . 3) ; xflm
                      ))
      (add-to-list 'candidates (wally//dice-item (car cursor) (cdr cursor))))
    candidates))

(defun wally//post-evil-dice (evil-candicates)
  (let ((pattern "\\(images\\|films\\|videos\\)/\\([a-zA-Z0-9_]\\{8\\}\\)")
        url
        ids
        (dashboard "~/Wally/Journal/gtd/inbox.org")
        items
        )
    (dolist (candidate evil-candicates)
      (setq ids (append ids candidate)))
    (dolist (id ids)
      (setq title (wally/org-get-title-by-id id))
      (when (string-match pattern title)
        (setq url(wally/evil-generate-index (match-string 1 title) (match-string 2 title)))
        (org-id-goto id)
        (org-set-property "LINK" url)
        (org-mark-subtree)
        (add-to-list 'items (buffer-substring-no-properties
                             (region-beginning)
                             (region-end)))
        (save-buffer)))
    (find-file dashboard)
    (goto-char (point-max))
    (dolist (item items)
      (insert "\n")
      (insert item))
    (save-buffer)))


(provide 'wally-dice)
