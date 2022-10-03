(defun wally/leetcode-save-problem-meta ()
  "save meta info of current problem to `wally-leetcode-current-problem'"
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (if (not (string-match "[^0-9]+\\(.+\\)" line))
        (error "unmatch leetcode line `%s'" line))
    (setq line (match-string-no-properties 1 line))
    (setq wally-leetcode-current-problem (s-split "  +" line))
    (message "get meta info `%s'" wally-leetcode-current-problem)))

(defun wally/leetcode-save-problem-desc ()
  "save problem descriptions"
  (interactive)
  (let ((raw-content (buffer-substring-no-properties (point-min) (point-max)))
        seqno
        title
        desc
        line
        (root "~/Project/codelet/problems")
        target
        )
    (with-temp-buffer
      (insert raw-content)
      (goto-char (point-min))
      (setq line (buffer-substring (line-beginning-position) (line-end-position)))
      (if (not (string-match "^\\([0-9]+\\)\\. \\([a-zA-Z0-9 ()]+\\)$" line))
          (error "unmatched title line"))
      (setq seqno (string-to-number (match-string 1 line))
            title (s-replace " " "-" (downcase (match-string 2 line))))
      (re-search-forward "^\\(Easy\\|Medium\\|Hard\\)" nil t 1)
      (next-line 2)
      (setq desc (buffer-substring (line-beginning-position) (point-max)))
      (message "saving `%s'" title)
      (setq target (f-join root (format "%04d_%s.txt" seqno title)))
      (find-file-noselect target)
      (with-current-buffer (get-file-buffer target)
        (insert desc)
        (save-buffer)))))

(setq wally-leetcode-current-problem nil)

(defun wally/leetcode-save-current-problem ()
  "save leetcode's description to local text file and add a record to note file"
  (interactive)
  (let ((note-id "7870A778-2059-4BCA-BF7E-9BFB1390E99D"))
    (if (not wally-leetcode-current-problem)
        (wally/leetcode-save-problem-meta)
      (wally/leetcode-save-problem-desc)
      (org-id-goto note-id)
      (next-line)
      (re-search-forward "^\\*" nil t 1)
      (backward-char)
      (save-excursion
        (wally/org-add-new-subheading))
      (org-set-tags (s-split ", " (s-replace "-" "_" (nth 4 wally-leetcode-current-problem))))
      (setq wally-leetcode-current-problem nil))))

(defun wally/leetcode-run()
  (interactive)
  (let* ((default-directory cmake-ide-build-dir)
         (relative-path (substring (buffer-file-name) (length (expand-file-name cmake-ide-project-dir))))
         (problem (file-name-sans-extension relative-path)))
    (if (not (f-exists-p problem))
        (setq problem (f-filename problem)))
    (shell-command (format "%s/%s" local-bin-dir problem))))


(defun wally/leetcode-start-c-or-cpp ()
  "start leetcode session with specified language, default C"
  (interactive)
  (leetcode-set-prefer-language)
  ;; (setq leetcode-directory (f-join "~/Project/codelet/source/solutions" (if (string-equal "cpp" leetcode-prefer-language) "C++" "C")))
  ;; (leetcode-refresh)
  )

(defun wally/leetcode-switch-to-test ()
  "switch to testcase from source"
  (interactive)
  (let* ((testdir "~/Project/codelet/tests")
         (filename (f-base (buffer-file-name)))
         (testfile (f-join testdir (format "test-%s.cc" filename))))
    (find-file testfile)
    (when (not (f-exists-p testfile))
      (save-buffer)
      (insert "leetcodetest")
      (yas-expand))
    ))

(defun wally/leetcode-switch-to-cpp ()
  "switch to cpp source file from c source file"
  (interactive)
  (let* ((cpp-dir "~/Project/codelet/source/solutions/C++")
         (filename (f-base (buffer-file-name)))
         (cpp-file (f-join cpp-dir (format "%s.cpp" filename))))
    (find-file cpp-file)))
(defun wally/leetcode-switch-to-test ()
  "switch to testcase from source"
  (interactive)
  (let* ((testdir "~/Project/codelet/tests")
         (filename (f-base (buffer-file-name)))
         (testfile (f-join testdir (format "test-%s.cc" filename))))
    (find-file testfile)
    (when (not (f-exists-p testfile))
      (save-buffer)
      (insert "leetcodetest")
      (yas-expand))
    ))

(defun wally/leetcode-export-to-anki-card ()
  "export problem heading to anki card"
  (interactive)
  (if (not (org-at-heading-p))
      (error "not a problem heading"))
  (let* ((desc-dir "problems")
         (c-src-dir "source/solutions/C")
         (cpp-src-dir "source/solutions/C++")
         (title (nth 4 (org-heading-components)))
         (note-id (org-id-get-create))
         (seqno (string-to-number (org-entry-get nil "SEQNO")))
         filename
         (card-dir "~/Wally/data/card")
         (card-name (f-join card-dir (f-filename (make-temp-file "alg" nil ".org"))))
         )
    (setq filename (format "%04d_%s" seqno (s-replace " " "-" (downcase title))))
    (setq leetcode-card-id (f-base card-name)
          leetcode-card-note-id note-id
          leetcode-card-title title
          leetcode-card-level (org-entry-get nil "LEVEL")
          leetcode-card-tags (s-join " " (org-get-tags))
          leetcode-card-desc (f-read-text (f-join desc-dir (format "%s.txt" filename)))
          leetcode-card-c-code (f-read-text (f-join c-src-dir (format "%s.c" filename)))
          leetcode-card-cpp-code (if (f-exists-p (f-join cpp-src-dir (format "%s.cpp" filename)))
                                     (f-read-text (f-join cpp-src-dir (format "%s.cpp" filename))))
          )
    (org-set-property "ANKI_CARD" leetcode-card-id)
    (find-file-noselect card-name)
    (with-current-buffer (get-file-buffer card-name)
      (erase-buffer)
      (insert "leetcodecard")
      (yas-expand)
      (goto-char (point-min))
      (next-line)
      (replace-regexp "^\\* " "- ")
      (save-buffer))
    (switch-to-buffer (get-file-buffer card-name))))

(spacemacs/set-leader-keys
  )

(provide 'wally-leetcode)
