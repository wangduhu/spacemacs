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


(defun wally/dice-items(items)
  (let (table count choice choices)
    (wally/with-dice-epc t
      (dolist (item items)
        (setq table (car item)
              count (cdr item)
              choice (wally/dice-epc-dice table count))
        (add-to-list 'choices (cons table choice))))
    choices))


(defun wally/dice-daily ())


(defun wally/dice-update-dashboard ()
  "更新dice面板"
  )


(defun wally/dice-rate ()
  (let ((todo-status (org-entry-get nil "TODO"))
        (priority (org-entry-get nil "PRIORITY"))
        (rate 0))
    (cond
     ((s-equals-p "QUIT" todo-status) (setq rate -1))
     ((s-equals-p "DONE" todo-status) (setq rate (- ?C (string-to-char priority)))))
    rate))


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


(defun wally//dice-item (head-id &optional count)
  (let (dice-repeat
        ids)
    (if (not count)
        (setq count 1))
    (org-id-goto head-id)
    (setq dice-repeat (org-entry-get nil "DICE_REPEAT"))
    (while (> count 0)
      (setq found nil
            count (1- count))
      (if dice-repeat
          (progn
            (wally//org-random-subheading)
            (org-schedule nil "+0d")
            (add-to-list 'ids (org-id-get-create))
            )
        (let ((max-try 7)
              found
              todo-state)
          (while (and (not found) (> max-try 0))
            (save-excursion
              (wally//org-random-subheading)
              (setq todo-state (org-get-todo-state))
              (when (string-equal todo-state "TODO")
                (setq found t)
                (org-schedule nil "+0d")
                (add-to-list 'ids (org-id-get-create))
                )
              (setq max-try (1- max-try)))))))
    (save-buffer)
    ids))

(defun wally//dice-daily ()
  (wally//dice-roam-note 3)
  (let (candidates)
    (dolist (cursor '(
                      ("10485761-D155-4171-AC1C-FCFFDB0788C9" . 1) ; csquote
                      ("BA64DE5E-2569-429E-BF48-7B8710AEE192" . 1) ; quote
                      ("B2893492-3272-4DA0-A4D6-664C18CD5D47" . 1) ; lines
                      ("9C8AFF18-C5AB-40A9-9A0A-049AB363EF9D" . 1) ; reading note
                      ("D986C24E-AB0F-4FE4-B355-F39A5287C20D" . 1) ; poem
                      ("E04A19F8-1307-4FA0-97EE-42C9544C6972" . 1) ; video
                      ("749ADC0E-A002-44D2-9145-9666C868A4F8" . 1) ; stage photo
                      ("825753B4-933F-41E6-A050-D8929AD70FB4" . 1) ; houlang
                      ("647BC8DA-4EC0-4400-BD49-840AC15632C2" . 1) ; song
                      ("9D72FA91-19DB-4B04-9F8F-849E10E18D65" . 1) ; painting
                      ("6A7E9D3C-201E-45E2-B75B-1737F825992E" . 1) ; speech
                      ("0582D866-5DC3-4548-BE14-9764964540E5" . 1) ; zhihu
                      ))
      (add-to-list 'candidates (wally//dice-item (car cursor) (cdr cursor))))
    candidates))

(defun wally//dice-weekly ()
  (let (candidates)
    (dolist (cursor '(
                      ("3CEF3415-8C56-4534-AD0C-0D8D285E5C4A" . 3) ; movie
                      ("0D2A1946-DB3A-498D-821D-5CEAED412A6F" . 1) ; album
                      ("FC768912-0F4B-44C9-806A-B14125F03DCB" . 1) ; documentary
                      ("4D04E979-9465-4902-BCD9-45CD37B35BEF" . 3) ; origami
                      ("41BEC8FD-CD89-4C3F-9B04-19DBDBD6733E" . 1) ; tv series
                      ("D077E770-B2C4-45DD-84CC-411CD44899B6" . 1) ; prose
                      ))
      (add-to-list 'candidates (wally//dice-item (car cursor) (cdr cursor))))
    candidates))

(defun wally//dice-monthly ()
  (let (candidates)
    (dolist (cursor '(
                      ("3D907909-4EA0-4D6D-AF96-2B35893D2B8D" . 1) ; friends
                      ("9BF6028D-2687-449D-9335-5244DFEE1EB0" . 2) ; reading
                      ))
      (add-to-list 'candidates (wally//dice-item (car cursor) (cdr cursor))))
    candidates))

(defun wally/dice-rouinte ()
  (interactive)
  (let ((evil (= 0 (mod (random 100) 73))))
    (when evil
      (wally//post-evil-dice (wally//dice-evil))))
  (let* ((date (decode-time (current-time)))
         (weekday (nth 6 date))
         (monthday (nth 3 date)))
    (when (equal monthday 1)
      (wally//dice-monthly))
    (when (equal weekday 0)
      (wally//dice-weekly))
    (wally//dice-daily)))


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
