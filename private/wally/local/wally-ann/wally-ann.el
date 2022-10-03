(defvar wally-ann-source-list "/tmp/ann.txt")
(defvar wally-ann-scrapy-dir "~/Project/scramy")
(defvar wally-ann-scrapy-spider "beike")

(defun wally/ann-export-source-list ()
  "export urls to soruce list file from `LINK' properties"
  (interactive)
  (org-id-goto "D8223D16-E95A-459F-AE3F-B73386327CCE")
  (write-region "" nil wally-ann-source-list)
  (org-map-entries '(lambda ()
                      (let ((state (org-get-todo-state))
                            (link (org-entry-get nil "LINK")))
                        (when (and link
                                   (s-starts-with-p "https://bj.ke.com" link)
                                   (not (member "ARCHIVE" (org-get-tags)))
                                   (string-equal "1" (org-entry-get nil "在售"))
                                   )
                          (append-to-file (format "%s\n" link) nil wally-ann-source-list))))
                   "LEVEL=2"
                   'tree))

(defun wally/ann-crawl-info ()
  "@DEPRECATED fetch housing page from Desktop"
  (let ((default-directory wally-ann-scrapy-dir))
    (shell-command-to-string (format "scrapy crawl %s" wally-ann-scrapy-spider))
    (message "finish crawl beike info")))

(defun wally/ann-parse-properties (content)
  "@DEPRECATED 从文本中解析房源基本信息"
  (let ((keys (list "房屋户型" "所在楼层" "建筑面积" "户型结构" "建筑类型"
                    "房屋朝向" "建筑结构" "装修情况" "梯户比例" "供暖方式"
                    "挂牌时间" "交易权属" "上次交易" "房屋用途" "房屋年限"
                    "产权所属" "抵押信息"))
        value properties)
    (with-temp-buffer
      (insert content)
      (dolist (key keys)
        (goto-char (point-min))
        (if (re-search-forward (format "^ +%s\\(.+\\)$" key) nil t 1)
            (setq value (match-string 1))
          (setq value "NA"))
        (add-to-list 'properties (cons key value)))
      )
    (setq properties (reverse properties))
    properties))

(defun wally/json-flat-dict (dict)
  "去除json dict树状结构，只保留叶子key-val"
  (let (key val result)
    (dolist (p dict)
      (setq key (car p)
            val (cdr p))
      (setq key (symbol-name key))
      (if (stringp val)
          (add-to-list 'result (cons key val))
        (setq result (append result (wally/json-flat-dict val)))))
    result))

(defun wally/ann-load-properties-from-json ()
  "@DEPRECATED load data form json"
  (let* ((db (json-read-file "/Users/wally/Wally/ann/housing/beike.json"))
         iid key val properties
         )
    (if (not (org-at-heading-p))
        (error "not on a heading"))
    (setq iid (nth 4 (org-heading-components)))
    (seq-doseq (record db)
      (setq record (car record))
      (message "%s" (car record))
      (when (string-equal iid (car record))
        (message "record found for %s" iid)
        (setq properties (wally/json-flat-dict (cdr record)))
        (dolist (p properties)
          (org-set-property (upcase (car p)) (cdr p)))))))


(defmath self (x)
  x
  )

(defmath AEdummy (val)
  1.0)

(defmath AEfav (fav)
  "ann eval fav property"
  (if (stringp fav)
      (setq fav (string-to-number fav)))
  (cond
   ((> fav 100) 1.0)
   ((> fav 50) 0.7)
   ((> fav 20) 0.2)
   (t 0.01)))

(provide 'wally-ann)
