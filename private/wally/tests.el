(ert-deftest example-test ()
  (should (equal (+ 9 2) 11)))

(ert-deftest test:wally/org-norm-value-property ()
  (should (= (_wally/org-norm-value-property "30") 30))
  (should (= (_wally/org-norm-value-property "00:00") 0))
  (should (= (_wally/org-norm-value-property "00.45") 45))
  (should (= (_wally/org-norm-value-property "23.30") -30))
  )

(ert-deftest test:wally/anki-is-note-existed ()
  (should (= (wally/anki-is-note-existed "bose qc35") 1651384178161))
  (should-not (wally/anki-is-note-existed "bose qc53")))

(ert-deftest test:wally/logb-has-scramy-item-visited ()
  (should (wally/logb-has-scramy-item-visited "netbian" "/tupian/29521.html"))
  (should-not (wally/logb-has-scramy-item-visited "foo" "bar"))
  )

(ert-deftest test:wally/dice-items ()
  (wally/dice-init-epc-srv)
  ;; (should (= (length (wally/dice-items (list (cons "album" 1)))) 1)) ; no mock
  (should (= (wally/dice-epc-query "book" "Flipped") 5))
  ;; (should (wally/dice-epc-rate "book" 5 2)) ; no mock
  (should-not (wally/dice-epc-rate "not_a_table" 5 2))
  )

(ert-deftest test:wally/dice-make-kv ()
  (let ((item (wally/dice-make-kv "foo" "bar")))
    (should (equal (symbol-name (car item)) ":foo"))
    (should (equal (car (cdr item)) "bar"))))

(ert t)
