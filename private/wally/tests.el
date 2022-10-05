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

(ert t)
