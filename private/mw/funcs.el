
;; snap
(defun wally/snap-delete-file-on-close ()
  "当关闭临时文件时，删除之"
  (let ((filepath (buffer-file-name)))
    (when (and filepath
               (s-contains-p wally-snap-dir filepath))
      (save-buffer)
      (f-delete filepath)
      (message "deleting %s" filepath))))

(defun wally/snap-mirror ()
  "在临时目录中为当前文件生成一个镜像文件"
  (interactive)
  (let* ((src-file (buffer-file-name))
         (dst-file (f-join wally-snap-dir
                           (f-filename src-file)))
         (content (buffer-substring (point-min)
                                    (point-max))))
    (when (not (f-exists-p wally-snap-dir))
      (f-mkdir wally-snap-dir)
      (message "wally-snap-dir created"))
    (if (s-ends-with-p ".gpg" dst-file)
        (setq dst-file (substring dst-file 0 -4)))
    (when (f-exists-p dst-file)
      (f-delete dst-file)
      (message "remove dst-file(%s)" dst-file))
    (find-file dst-file)
    (insert content)
    (save-buffer)))


;; ledger
(defun wally/finance-convert-orgheading-to-ledger-item ()
  (interactive)
  (if (not (org-at-heading-p))
      (error "not a org heading"))
  (let* ((heading (nth 4
                       (org-heading-components)))
         (pattern "\\(.+\\) +:\\$\\(.+\\):\\$\\(.+\\):") ; TODO @REFACTOR 看不懂这是啥？
         (date (decode-time (org-get-scheduled-time nil)))
         (value (org-entry-get nil "VALUE"))
         dest
         src
         dst
         first-account
         second-account
         (alias (make-hash-table :test 'equal))
         snippet
         (ledger (f-join wally-journal-dir "private" "account.ledger.gpg")))
    ;; TODO @REFACTOR 列表不要定义在代码里，改为定义在文件里(可以由org-mode文件管理，导出到临时文件中)
    (dolist (pair (list '("weixin" . "Assets:Checking:WEIXIN")
                        '("zhaohang" . "Assets:Checking:CMB")
                        '("yuebao" . "Assets:Checking:ALIPAY")
                        '("mw" . "Assets:Checking:MW")
                        '("zhifubao" . "Assets:Checking:ALIPAY")
                        '("gonghang" . "Assets:Checking:ICBC")
                        '("cash" . "Assets:Cash:CASH")
                        '("huabei" . "Liabilities:Ant:HUABEI")
                        '("food" . "Expense:Diet:Food")
                        '("taxi" . "Expense:Travelling:Taxi")
                        '("wenju" . "Expense:Learnging:Stationery")
                        '("salary" . "Income:Salary")
                        '("gongjijin" . "Income:Gongjijin")
                        '("subway" . "Expense:Travelling:PublicTransport")
                        '("banche" . "Expense:Travelling:PublicTransport")
                        '("waterelectricity" . "Expense:Shelter:WaterElectricity")
                        '("@" . "Expense:Socializing:Love")
                        '("julie" . "Creditors:Friends:Julie")
                        '("QuXiao" . "Liabilities:Friends:QuXiao")
                        '("wangjun" . "Creditor:Friends:WangJun")
                        '("clothes" . "Expense:Clothing:Clothes")
                        '("clothing" . "Expense:Clothing:Clothes")
                        '("mobile" . "Expense:Communication:CallCharge")
                        '("baitiao" . "Liabilities:JD:BAITIAO")
                        '("taxes" . "Expense:Others:Taxes")
                        '("haircut" . "Expense:Clothing:Haircut")
                        '("commodity" . "Expense:Shelter:DailySupplies")
                        '("fang" . "Expense:Shelter:House")
                        '("baby" . "Expense:ChildCare")
                        '("it" . "Expense:Technique:IT")
                        '("treatment" . "Expense:Health:Treatment")
                        '("medicine" . "Expense:Health:Medicine")
                        '("railway" . "Expense:Travelling:Railway")
                        '("beijingbank" . "Assets:Checking:BEIJINGBANK")
                        '("beijingcard" . "Liabilities:CreditCard:BEIJINGBANK")
                        '("zhaohangcard" . "Liabilities:CreditCard:ZHAOHANG")
                        '("yibao" . "Income:HealthInsurance")
                        '("invest" . "Income:Investment:Bank")
                        '("otherincome" . "Income:others")
                        '("brother" . "Liabilities:Family:BROTHER")
                        '("sister" . "Liabilities:Family:SISTER")
                        '("drink" . "Expense:Diet:Drink")
                        '("health" . "Expense:Health:Health")
                        '("pet" . "Expense:Entertainment:Pet")
                        '("leisure" . "Expense:Entertainment:Leisure")
                        '("socity" . "Expense:Socializing:Others")
                        '("family" . "Expense:Socializing:Family")
                        '("commissioncharge" . "Expense:Socializing:Others")
                        '("accommodation" . "Expense:Shelter:Accommodation")
                        '("flight" . "Expense:Travelling:Flight")
                        '("hongbao" . "Income:RedPacket")
                        '("motor" . "Expense:Travelling:Motor")
                        '("marriage" . "Expense:Socializing:Marriage")
                        '("furniture" . "Expense:Shelter:House:Furniture")
                        '("gongdai" . "Expense:Shelter:House:FundLoan")
                        '("zhuangxiu" . "Expense:Shelter:House")
                        '("shangdai" . "Expense:Shelter:House:CommercialLoan")
                        '("喜礼" . "Income:WeddingWhipRound")
                        '("" . "")))
      (puthash (car pair)
               (cdr pair)
               alias))
    (if (not (string-match "\\(.+\\) +:\\$\\(.+\\):\\$\\(.+\\):" (format "%s" heading)))
        (error "invalid org heading: %S" heading))
    (setq desc (match-string 1 heading)
          src
          (match-string 2 heading)
          dst
          (match-string 3 heading))
    (setq src (gethash src alias)
          dst
          (gethash dst alias))
    ;; most cases
    (setq first-account src second-account dst)
    (cond
     ((s-starts-with-p "Expense" src) nil)
     ((s-starts-with-p "Expense" dst)
      (setq first-account dst second-account src))
     ;; 工资收入等
     ((and (s-starts-with-p "Assets" src)
           (s-starts-with-p "Income" dst)) nil)
     ;; 转账
     ((and (s-starts-with-p "Assets" src)
           (s-starts-with-p "Assets" dst))
      (setq value (concat "-" value)))
     ;; 信用卡还款
     ((and (s-starts-with-p "Assets" src)
           (s-starts-with-p "Liabilities" dst))
      (setq value (concat "-" value)))
     ;; 信用卡借款
     ((and (s-starts-with-p "Liabilities" src)
           (s-starts-with-p "Assets" dst))
      (setq value (concat "-" value)))
     ;; 借钱
     ((and (s-starts-with-p "Assets" src)
           (s-starts-with-p "Creditors" dst))
      (setq value (concat "-" value)))
     ;; 不支持的情况
     (t (error "valid but unsupported org-heading")))
    ;; TODO @REFACTOR 是否可以使用 `ledger-add-transaction'
    (setq snippet (format "\n%4d-%02d-%02d %s\n    %s  %s CNY\n    %s\n"
                          (nth 5 date)
                          (nth 4 date)
                          (nth 3 date)
                          desc
                          first-account
                          value
                          second-account))
    (find-file-noselect ledger)
    (with-current-buffer (get-file-buffer ledger)
      (goto-char (point-max))
      (insert snippet)
      (save-buffer))
    ;; TODO @REFACTOR ledger文件操作和orgmode文件操作分离，封装成不同函数
    (org-archive-subtree)
    (save-buffer)
    (message "add ledger item: %s(%s)" desc value)))
