(defun wally/logb-has-scramy-item-visited (table value)
  "查看表table中指定value的记录是否被访问过
访问历史由表`history'记录"
  (wally/with-epc t
    (epc:call-sync wally-epc 'has_visited (list table value))))


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
  (let ((database wally-scramy-db)
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

(provide 'wally-logb)
