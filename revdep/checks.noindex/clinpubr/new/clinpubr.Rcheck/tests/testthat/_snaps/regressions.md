# regression_basic_results works for Cox regression

    Code
      results$table
    Output
                           Terms Count            Crude Crude           Model1 Model1
      1                age (All)   227               HR     P               HR      P
      2               Continuous    NA  1.02(1.00,1.04) 0.041  1.01(0.99,1.03)  0.158
      3      Continuous, per 0.1    NA  1.00(1.00,1.00) 0.041  1.00(1.00,1.00)  0.158
      4       Continuous, per 10    NA  1.21(1.01,1.45) 0.041  1.14(0.95,1.38)  0.158
      5      Continuous, per 100    NA 6.56(1.08,40.03) 0.041 3.80(0.60,24.23)  0.158
      6     Continuous, per 1 SD    NA  1.19(1.01,1.40) 0.041  1.13(0.95,1.34)  0.158
      7    Continuous, logarithm    NA  3.03(1.02,9.06) 0.047  2.20(0.72,6.72)  0.165
      8     Grouped by Quartiles    NA             <NA>  <NA>             <NA>   <NA>
      9                       Q1    49    1 (Reference)  <NA>    1 (Reference)   <NA>
      10                      Q2    56  1.08(0.68,1.72) 0.754  1.11(0.70,1.77)  0.658
      11                      Q3    55  1.02(0.64,1.63) 0.929  0.94(0.58,1.51)  0.787
      12                      Q4    67  1.39(0.89,2.15) 0.145  1.25(0.80,1.96)  0.325
      13             P for trend    NA             <NA> 0.160             <NA>  0.426
      14 Grouped by Median Value    NA             <NA>  <NA>             <NA>   <NA>
      15                     Low   105    1 (Reference)  <NA>    1 (Reference)   <NA>
      16                    High   122  1.16(0.85,1.58) 0.354  1.04(0.75,1.43)  0.833

# regression_basic_results works for logistic regression

    Code
      results$table
    Output
                           Terms Count               Crude Crude             Model1
      1                age (All)   227                  OR     P                 OR
      2               Continuous    NA     1.04(1.00,1.07) 0.025    1.03(1.00,1.06)
      3      Continuous, per 0.1    NA     1.00(1.00,1.01) 0.025    1.00(1.00,1.01)
      4       Continuous, per 10    NA     1.44(1.05,2.00) 0.025    1.34(0.96,1.87)
      5      Continuous, per 100    NA 39.29(1.61,1034.37) 0.025 18.70(0.69,532.68)
      6     Continuous, per 1 SD    NA     1.40(1.04,1.88) 0.025    1.31(0.97,1.77)
      7    Continuous, logarithm    NA    9.14(1.41,61.13) 0.021   5.96(0.87,41.79)
      8     Grouped by Quartiles    NA                <NA>  <NA>               <NA>
      9                       Q1    49       1 (Reference)  <NA>      1 (Reference)
      10                      Q2    56     1.33(0.58,3.05) 0.501    1.28(0.55,2.97)
      11                      Q3    55     1.29(0.57,2.98) 0.540    1.23(0.53,2.85)
      12                      Q4    67     2.01(0.88,4.68) 0.100    1.68(0.72,3.98)
      13             P for trend    NA                <NA> 0.119               <NA>
      14 Grouped by Median Value    NA                <NA>  <NA>               <NA>
      15                     Low   105       1 (Reference)  <NA>      1 (Reference)
      16                    High   122     1.41(0.79,2.53) 0.252    1.26(0.70,2.30)
         Model1
      1       P
      2   0.083
      3   0.083
      4   0.083
      5   0.083
      6   0.083
      7   0.069
      8    <NA>
      9    <NA>
      10  0.568
      11  0.634
      12  0.234
      13  0.271
      14   <NA>
      15   <NA>
      16  0.440

# regression_basic_results works for linear regression

    Code
      results$table
    Output
                           Terms Count              Crude Crude
      1                age (All)   214        Coefficient     P
      2               Continuous    NA   0.05(-0.14,0.25) 0.579
      3      Continuous, per 0.1    NA  0.005(-0.01,0.02) 0.579
      4       Continuous, per 10    NA   0.55(-1.39,2.48) 0.579
      5      Continuous, per 100    NA 5.46(-13.90,24.81) 0.579
      6     Continuous, per 1 SD    NA   0.50(-1.28,2.28) 0.579
      7    Continuous, logarithm    NA  4.01(-7.45,15.46) 0.491
      8     Grouped by Quartiles    NA               <NA>  <NA>
      9                       Q1    54      1 (Reference)  <NA>
      10                      Q2    43   3.28(-2.03,8.59) 0.224
      11                      Q3    63   0.82(-4.00,5.64) 0.738
      12                      Q4    54   0.39(-4.61,5.39) 0.878
      13             P for trend    NA               <NA> 0.914
      14 Grouped by Median Value    NA               <NA>  <NA>
      15                     Low    97      1 (Reference)  <NA>
      16                    High   117  -0.84(-4.40,2.73) 0.644

# regression_scan returns expected structure

    Code
      res
    Output
        predictor nvalid  original.HR original.pval original.padj logarithm.HR
      4   ph.ecog    227 1.609532e+00  2.692234e-05   0.000242301           NA
      6 pat.karno    225 9.803456e-01  2.823960e-04   0.001270782    0.2709544
      3       sex    228 5.880028e-01  1.491229e-03   0.004473688           NA
      5  ph.karno    227 9.836863e-01  4.957861e-03   0.011155187    0.3184168
      2       age    228 1.018897e+00  4.185313e-02   0.075335636    3.0256773
      1      inst    227 9.903692e-01  3.459838e-01   0.518975713    0.9292046
      7  meal.cal    181 9.998762e-01  5.929402e-01   0.762351652    0.9141580
      8   wt.loss    214 1.001320e+00  8.281974e-01   0.931722040           NA
      9      dead    228 3.760837e+08  9.923502e-01   0.992350186           NA
        logarithm.pval logarithm.padj categorized.HR categorized.pval
      4             NA             NA             NA     0.0001529594
      6   0.0003071163    0.001535582      0.5755627     0.0006607697
      3             NA             NA      0.5880028     0.0014912292
      5   0.0079467639    0.019866910      0.6352465     0.0077669568
      2   0.0466925552    0.077820925      1.1440790     0.3910647289
      1   0.3181431914    0.397678989      0.8384047     0.2600039692
      7   0.6128094775    0.612809477      0.8620604     0.3957557555
      8             NA             NA      1.3190185     0.0909098385
      9             NA             NA             NA               NA
        categorized.padj rcs.overall.pval rcs.overall.padj rcs.nonlinear.pval
      4      0.001223676               NA               NA                 NA
      6      0.002643079      0.002584773       0.01550864         0.59089516
      3      0.003976611               NA               NA                 NA
      5      0.015533914      0.012846171       0.03853851         0.23079609
      2      0.395755755      0.082544702       0.16508940         0.34241226
      1      0.346671959      0.817527653       0.87071306         0.98397049
      7      0.395755755      0.870713064       0.87071306         0.82272559
      8      0.145455742      0.112890717       0.16933608         0.05149364
      9               NA               NA               NA                 NA
        rcs.nonlinear.padj best.var.trans
      4                 NA       original
      6          0.8863427       original
      3                 NA    categorized
      5          0.6848245       original
      2          0.6848245       original
      1          0.9839705    categorized
      7          0.9839705    categorized
      8          0.3089618  rcs.nonlinear
      9                 NA       original

# regression_fit returns model results

    Code
      fit
    Output
        term estimate   std.error statistic    p.value conf.low conf.high
      1  age 1.018897 0.009199207  2.034978 0.04185313  1.00069  1.037434

# fit_model handles different analysis types

    Code
      summary(fit_cox)$coefficients
    Output
                coef exp(coef)    se(coef)        z   Pr(>|z|)
      age 0.01872018  1.018897 0.009199207 2.034978 0.04185313

---

    Code
      anova(fit_cox)
    Output
                      Wald Statistics          Response: Surv(time, status) 
      
       Factor     Chi-Square d.f. P     
       age        4.14       1    0.0419
       TOTAL      4.14       1    0.0419

---

    Code
      summary(fit_logistic)$coefficients
    Output
                    Estimate Std. Error   z value   Pr(>|z|)
      (Intercept) -1.3094866 1.01742651 -1.287058 0.19807419
      age          0.0367706 0.01644903  2.235426 0.02538937

# regression_fit works cluster

    Code
      fit_cluster
    Output
          term estimate   std.error statistic     p.value conf.low conf.high
      age  age 1.014301 0.005186644  2.737812 0.006184951 1.004043  1.024665

---

    Code
      fit_cluster2
    Output
             term  estimate  std.error statistic    p.value  conf.low conf.high
      age     age 1.1000422 0.04953593  1.924837 0.05424982 0.9982614  1.212200
      age'   age' 0.8591691 0.11424446 -1.328638 0.18396737 0.6868052  1.074790
      age'' age'' 2.1824513 0.61967902  1.259440 0.20787137 0.6478475  7.352183

