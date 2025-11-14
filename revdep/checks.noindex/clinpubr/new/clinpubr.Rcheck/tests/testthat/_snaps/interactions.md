# interaction_p_value returns valid p-values

    Code
      p_logistic_rob
    Output
      [1] 0.9413102

# interaction_scan returns valid data frame and saves table

    Code
      res_cox
    Output
        predictor group.by nvalid linear.p.int  rcs.p.int linear.p.adj  rcs.p.adj
      2       age  ph.ecog    227    0.0349543 0.03808796    0.1048629 0.07617591
      1       age      sex    228    0.6183372 0.83647048    0.7552521 0.83647048
      3       sex  ph.ecog    227    0.7552521         NA    0.7552521         NA

---

    Code
      res_logistic
    Output
        predictor group.by nvalid linear.p.int  rcs.p.int linear.p.adj  rcs.p.adj
      2       age  ph.ecog    227  0.004496487 0.03671699   0.01348946 0.07343398
      3       sex  ph.ecog    227  0.062415586         NA   0.09362338         NA
      1       age      sex    228  0.956186078 0.51523865   0.95618608 0.51523865

