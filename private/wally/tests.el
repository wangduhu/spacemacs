(ert-deftest example-test ()
  (should (equal (+ 9 2) 11)))

(ert-deftest test:_wally/org-norm-value-property ()
  (should (= (_wally/org-norm-value-property "30") 30))
  (should (= (_wally/org-norm-value-property "00:00") 0))
  (should (= (_wally/org-norm-value-property "00.45") 45))
  (should (= (_wally/org-norm-value-property "23.30") -30))
  )


(ert t)
