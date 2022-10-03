(fset 'wally/macro-copy-org-src-block
      (kmacro-lambda-form [?\C-c ?\' ?\; ?b ?Y ?\C-c ?\'] 0 "%d"))

(fset 'wally/macro-alternate-buffer
      (kmacro-lambda-form [?\; ?b ?b return] 0 "%d"))

(fset 'wally/macro-join-next-line
      (kmacro-lambda-form [?J ?$ ?a return escape ?k ?J ?0] 0 "%d"))

(fset 'wally/macro-sr-speedbar
      (kmacro-lambda-form [?\; ?s ?r ?\; ?w ?d ?\; ?w ?/ ?\; ?b ?b return] 0 "%d"))

(fset 'wally/macro-browse-db-item
      (kmacro-lambda-form [?, ?g ?r ?g ?p ?$ ?h ?h return ?\; ?f ?o ?q ?t ?d] 0 "%d"))

(fset 'wally/macro-agenda-done-with-clock
      (kmacro-lambda-form [?I ?O ?t ?d] 0 "%d"))

(fset 'wally/macro-agenda-todo-quit
      (kmacro-lambda-form [?t ?q] 0 "%d"))


(defun wally/macro-repeat-to-end ()
  (interactive)
  (kmacro-end-and-call-macro 0))

(provide 'wally-macros)
