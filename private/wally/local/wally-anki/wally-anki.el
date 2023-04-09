(defvar wally-anki-db (expand-file-name "~/Wally/data/db/anki.sqlite3"))
(defconst wally-tmp-anki-db (concat wally-anki-db ".snap"))
(defconst wally-anki-dir (expand-file-name "~/Wally/data/card"))
;; (setq wally-anki-db (expand-file-name "~/Wally/data/db/veil.anki.sqlite3"))

(defvar wally-anki-epc nil)
(defvar wally-anki-epc-srv (expand-file-name "~/Project/empyc/srv/anki/epcsrv.py"))


(defun wally/anki-init-conn ()
  (when wally-anki-epc
    (epc:stop-epc wally-anki-epc)
    (setq wally-anki-epc nil))
  (if (f-exists-p wally-tmp-anki-db)
      (f-delete wally-tmp-anki-db))
  (f-copy wally-anki-db wally-tmp-anki-db)
  (setq wally-anki-epc (epc:start-epc "python3" (list wally-anki-epc-srv))))

(defmacro wally/with-tmp-anki-db (cond &rest body)
  "复制anki db并建立epc连接
TODO 不需要 cond参数，还不会写宏，参考http://0x100.club/wiki_emacs/elisp-macro.html
"
  (declare (indent 1) (debug t))
  `(if ,cond
       (progn ,@body)))


(defun wally/anki-is-note-existed (title)
  "判断指定title的anki note是否已存在于数据库中，存在返回note id，否则返回nil"
  (wally/with-tmp-anki-db t
    (epc:call-sync wally-anki-epc 'query_note (list title))))

(defun wally/anki-get-note (id)
  "判断指定title的anki note是否已存在于数据库中，存在返回note id，否则返回nil"
  (wally/with-tmp-anki-db t
    (epc:call-sync wally-anki-epc 'get_note (list id))))


(defun wally/anki-get-exported-card ()
  "获取所有已导出的anki卡片文件"
  (let ((default-directory wally-anki-dir)
        (pattern "\\([0-9]+\\.org\\)::ANKI_NOTE_ID: [0-9]+$")
        output
        exported-cards)
    (setq output (shell-command-to-string "grep 'ANKI_NOTE_ID' *.org"))
    (mapcar (lambda (l) (if (string-match pattern l) (add-to-list 'exported-cards (match-string 1 l))))
            (s-split "\n" output))
    exported-cards))


(defun wally/anki-export-current-card ()
  "导出当前文件为anki卡片"
  (let (card-id)
    (goto-char (point-min))
    (re-search-forward "^\\* " nil t 1)
    (setq card-id (org-entry-get nil "ANKI_NOTE_ID"))
    (unless card-id
      (message "unexported note: %s" (buffer-file-name))
      (setq card-id (wally/anki-is-note-existed (nth 4 (org-heading-components))))
      (if card-id
          (org-set-property "ANKI_NODE_ID" (format "%s" card-id))
        (anki-editor-push-notes))
      (save-buffer))))


(defun wally/anki-export-all-left-cards ()
  "导出card文件夹下所有文件为anki卡片"
  (interactive)
  (mapcar (lambda (f)
            (let ((exported-cards (wally/anki-get-exported-card))
                  (filename (f-filename f)))
              (message "scanning %s" filename)
              (when (and (s-starts-with-p "2" filename) (string-match "^[0-9]+\\.org$" filename) (not (member filename exported-cards)))
                (find-file-noselect f)
                (with-current-buffer (get-file-buffer f)
                  (wally/anki-export-current-card))
                (kill-buffer (get-file-buffer f)))))
          (f-files wally-anki-dir (lambda (f) (string-match "^[0-9]+$" (f-base f))))))


(defconst _wally-anki-card-head-template "* %s
:PROPERTIES:
:NOTE_ID:   %s
:ANKI_DECK: Notes
:ANKI_NOTE_TYPE: Note
:ANKI_TAGS: %s
:END:

")


(defun wally/anki-expand-card (id title level tags content)
  "根据模板，生成 anki card内容"
  (with-temp-buffer
    (org-mode)
    (insert (format _wally-anki-card-head-template title id (s-join " " tags)))
    (insert "** pros\n\n")
    (save-excursion (insert (format "%s %s\n\n%s\n" (make-string level ?*) title content)))
    (while (> level 3)
      (org-promote-subtree)
      (setq level (1- level)))
    (while (< level 3)
      (org-demote-subtree)
      (setq level (1+ level)))
    (goto-char (point-max))
    (insert "** cons\n\n")
    (insert (format "- [[org-protocol://org-id?id=%s][inner note link]]\n\n" id))
    (buffer-substring-no-properties (point-min) (point-max))))


(defun wally/anki-export-org-heading (id title level tags content)
  (let* ((card-content (wally/anki-expand-card id title level tags content))
         (filename (format-time-string "%Y%m%d%H%M%S.org" (current-time)))
         (filepath (f-join (f-parent wally-journal-dir) "data" "card" filename))
         myid)
    (if (f-exists-p filepath)
        (message "card(%s) is already existed! quick")
      (write-region card-content nil filepath)
      filepath)))

(defun wally/org-get-item-filepath ()
  (let ((parent-dir (org-entry-get nil "PARENT_DIR" t))
        (filename (org-entry-get nil "FILENAME"))
        (title (wally/org-get-heading-no-progress))
        (suffix (org-entry-get nil "FILE_SUFFIX" t))
        filepath)
    (if (not parent-dir)
        (message "no property: PARENT_DIR")
      (if (not (f-absolute-p parent-dir))
          (setq parent-dir (expand-file-name (f-join default-directory parent-dir))))
      (if (not filename)
          (setq filename (wally/org-get-heading-no-progress)))
      (if suffix
          (setq filename (format "%s.%s" filename suffix)))
      (setq filepath (f-join parent-dir filename))
      (if (f-exists-p filepath)
          filepath))))

(defun wally/org-get-heading-content ()
  (let (content)
    (org-mark-subtree)
    (setq content (buffer-substring-no-properties (region-beginning) (region-end)))
    (deactivate-mark)
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (search-forward ":END:" nil t)
      (setq content (buffer-substring-no-properties (line-end-position) (point-max))))
    content))

(defvar wally-yas-args nil)

(cl-defun wally/anki-export-simple-note ()
  "从org文件中导出简单卡片，内容只有标题"
  (interactive)
  (let ((card-buffer "*anki-card*")
        (title (wally/org-get-heading-no-progress))
        (content (wally/org-get-heading-content))
        (formatter (org-entry-get nil "FORMATTER" t))
        (deck (org-entry-get nil "ANKI_DECK" t))
        (anki-id (org-entry-get nil "ANKI_NOTE_ID"))
        (anki-snippet (org-entry-get nil "ANKI_SNIPPET" t))
        (anki-snippet-keys (org-entry-get nil "ANKI_SNIPPET_KEYS" t))
        (update-existed (org-entry-get nil "ANKI_FORCE_UPDATE" t))
        (org-use-tag-inheritance t)
        tags
        value
        )
    (if (not (and deck anki-snippet anki-snippet-keys))
        (error "no deck/snippet/keys specified "))
    (when (and anki-id (not update-existed))
      (message "skip existed card: %s" anki-id)
      (cl-return-from wally/anki-export-simple-note))
    (setq tags (org-get-tags))
    (setq wally-yas-args nil)
    (dolist (key (s-split " " anki-snippet-keys))
      (if (s-starts-with-p "." key)
          (setq key (substring key 1)
                value (or (org-entry-get nil key t) key))
        (setq value (org-entry-get nil key t)))
      (if (not value)
          (error "property <%s> not set" key))
      (add-to-list 'wally-yas-args value))
    (setq wally-yas-args (reverse wally-yas-args))
    (when formatter
      (setq formatter (intern formatter))
      (setq title (apply formatter (list title))))
    (with-current-buffer card-buffer
      (erase-buffer)
      (org-mode)
      (save-excursion
        (insert anki-snippet)
        (yas-expand))
      (org-set-property "ANKI_DECK" deck)
      (org-set-property "ANKI_NOTE_TYPE" "Note")
      (if anki-id
          (org-set-property "ANKI_NOTE_ID" anki-id))
      (if tags
          (org-set-property "ANKI_TAGS" (s-join " " tags)))
      (anki-editor-mode t)
      (anki-editor-push-notes)
      (setq anki-id (org-entry-get nil "ANKI_NOTE_ID" t)))
    (when anki-id
      (org-set-property "ANKI_NOTE_ID" anki-id)
      (message "export to card: %s" anki-id))))

(defun wally/format-lines (content)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (replace-string "\\" "\n")
    (buffer-substring-no-properties (point-min) (point-max))))

(defun wally/anki-query-and-set-anki-id ()
  (let ((title (wally/org-get-heading-no-progress))
        iid)
    (setq iid (wally/anki-is-note-existed (substring-no-properties title)))
    (if iid
        (org-set-property "ANKI_NOTE_ID" (format "%d" iid)))))

(defun wally/anki-tag-trigger-cover (content)
  (let ((pattern "img src=\"\\(.+\\)\">")
        img)
    (unless (org-entry-get nil "COVER")
      (when (string-match  pattern content)
        (setq img (match-string 1 content))
        (wally/anki-copy-media img)
        (org-set-property "COVER" (format "anki/%s" img))))))

(defun wally/anki-tag-trigger-link (content)
  (let ((douban-pattern "\\(https:..[a-z]+.douban.com.subject.[0-9]+.\\)")
        link)
    (cond
     ((string-match douban-pattern content) (org-set-property "DOUBAN" (match-string 1 content))))))

(defun wally/anki-tag-trigger-act (tag content)
  (let (name func)
    (message "functional tag %s" tag)
    (setq tag (substring tag 1)
          name (format "wally/anki-tag-trigger-%s" tag)
          func (intern name))
    (if (fboundp func)
        (apply func (list content)))))

(cl-defun wally/anki-sync-meta-data ()
  (interactive)
  (let ((card-id (org-entry-get nil "ANKI_NOTE_ID"))
        note
        tags
        flags
        content
        )
    (unless card-id
      (message "not an Anki card")
      (cl-return-from wally/anki-sync-meta-data))
    (setq card-id (string-to-number card-id))
    (setq note (wally/anki-get-note card-id)
          tags (s-trim (nth 0 note))
          flags (nth 1 note)
          content (nth 2 note))
    (message tags)
    (unless (s-equals-p tags "")
      (dolist (tag (s-split " " tags))
        (if (s-starts-with-p "_" tag)
            (wally/anki-tag-trigger-act tag content)
          (org-set-tags (append (org-get-tags) (list tag))))))))

(cl-defun wally/anki-copy-media (filename &optional user)
  (let (anki-dir
        src-file
        (dst-file (f-join (expand-file-name "~/Wally/data/assets/anki") filename))
        (username user-login-name))
    (if user
        (setq username user))
    (if (equal system-type 'darwin)
        (setq anki-dir (expand-file-name "~/Library/Application Support/Anki2"))
      (setq anki-dir (expand-file-name "~/.local/share/Anki2")))
    (setq src-file (f-join anki-dir username "collection.media" filename))
    (unless (f-exists-p src-file)
      (message "%s not exists!" src-file)
      (cl-return-from wally/anki-copy-media))
    (unless (f-exists-p dst-file)
      (f-copy src-file dst-file)
      (message "copy %s to %s" src-file dst-file))))


;; @DEPRECATED
(defvar __wally-anki-customed-deck nil)

;; @DEPRECATED
(defun __wally/anki-helm-custom-deck()
  (let (source)
    (setq source '((name . "anki decks")
                   (candidates . ( "INBOX"))
                   (action . (lambda(candidate)
                               (setq wally-anki-customed-deck candidate)))))
    (helm-other-buffer 'source "anki decks")))


;; @DEPRECATED
(defun __wally/anki-archive-org-heading()
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


(provide 'wally-anki)
