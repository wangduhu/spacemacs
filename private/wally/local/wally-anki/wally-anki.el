(defconst wally-anki-db (expand-file-name "~/Wally/data/db/anki.sqlite3"))
(defconst wally-tmp-anki-db (concat wally-anki-db ".snap"))
(defconst wally-anki-dir (expand-file-name "~/Wally/data/card"))

(defvar wally-anki-epc nil)
(defvar wally-anki-epc-srv (expand-file-name "~/Project/empyc/srv/anki.py"))


(defmacro wally/with-tmp-anki-db (cond &rest body)
  "复制anki db并建立epc连接
TODO 不需要 cond参数，还不会写宏，参考http://0x100.club/wiki_emacs/elisp-macro.html
"
  (declare (indent 1) (debug t))
  (if wally-anki-epc
      (epc:stop-epc wally-anki-epc))
  (if (f-exists-p wally-tmp-anki-db)
      (f-delete wally-tmp-anki-db))
  (f-copy wally-anki-db wally-tmp-anki-db)
  (setq wally-anki-epc (epc:start-epc "python3.9" (list wally-anki-epc-srv)))
  `(if ,cond
       (progn ,@body)))


(defun wally/anki-is-note-existed (title)
  "判断指定title的anki note是否已存在于数据库中，存在返回note id，否则返回nil"
  (wally/with-tmp-anki-db t
                          (epc:call-sync wally-anki-epc 'query_note (list title))))


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
