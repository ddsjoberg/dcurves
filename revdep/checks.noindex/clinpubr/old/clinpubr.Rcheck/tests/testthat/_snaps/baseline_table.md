# get_var_types correctly classifies variables

    Code
      res
    Output
      $factor_vars
      [1] "status"  "sex"     "ph.ecog"
      
      $exact_vars
      [1] "ph.ecog"
      
      $nonnormal_vars
      [1] "inst"      "time"      "ph.karno"  "pat.karno" "meal.cal"  "wt.loss"  
      
      $omit_vars
      NULL
      
      $strata
      [1] "sex"
      
      attr(,"class")
      [1] "var_types"

# baseline_table generates correct output files 2

    Code
      read.csv("baseline.csv", check.names = FALSE)
    Output
                                    Overall          vs: 0          vs: 1      p
      1                    n             32             18             14       
      2      mpg (mean (SD))     20.1 (6.0)     16.6 (3.9)     24.6 (5.4) <0.001
      3              cyl (%)                                              <0.001
      4                    4      11 (34.4)        1 (5.6)      10 (71.4)       
      5                    6       7 (21.9)       3 (16.7)       4 (28.6)       
      6                    8      14 (43.8)      14 (77.8)        0 (0.0)       
      7     disp (mean (SD))  230.7 (123.9)  307.1 (106.8)   132.5 (56.9) <0.001
      8       hp (mean (SD))   146.7 (68.6)   189.7 (60.3)    91.4 (24.4) <0.001
      9  drat (median [IQR]) 3.7 [3.1, 3.9] 3.2 [3.1, 3.7] 3.9 [3.7, 4.1]  0.013
      10      wt (mean (SD))      3.2 (1.0)      3.7 (0.9)      2.6 (0.7)  0.001
      11    qsec (mean (SD))     17.8 (1.8)     16.7 (1.1)     19.3 (1.4) <0.001
      12          am = 1 (%)      13 (40.6)       6 (33.3)       7 (50.0)  0.556
      13            gear (%)                                               0.001
      14                   3      15 (46.9)      12 (66.7)       3 (21.4)       
      15                   4      12 (37.5)       2 (11.1)      10 (71.4)       
      16                   5       5 (15.6)       4 (22.2)        1 (7.1)       
      17 carb (median [IQR]) 2.0 [2.0, 4.0] 4.0 [2.2, 4.0] 1.5 [1.0, 2.0] <0.001
            test
      1         
      2         
      3    exact
      4         
      5         
      6         
      7         
      8         
      9  nonnorm
      10        
      11        
      12        
      13   exact
      14        
      15        
      16        
      17 nonnorm

---

    Code
      read.csv("baseline_missing.csv", check.names = FALSE)
    Output
                         Overall   vs: 0   vs: 1   p test
      1                n      32      18      14  NA   NA
      2   mpg = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      3   cyl = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      4  disp = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      5    hp = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      6  drat = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      7    wt = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      8  qsec = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      9    am = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      10 gear = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA
      11 carb = TRUE (%) 0 (0.0) 0 (0.0) 0 (0.0) NaN   NA

# baseline_table generates correct output files

    Code
      read.csv("test_output.csv", check.names = FALSE)
    Output
                                                Overall         ph.ecog_cat: 0
      1                         n                   228                     63
      2       inst (median [IQR])      11.0 [3.0, 16.0]        7.0 [3.0, 13.0]
      3       time (median [IQR])  255.5 [166.8, 396.5]   303.0 [224.5, 437.5]
      4            status = 2 (%)            165 (72.4)              37 (58.7)
      5        age (median [IQR])     63.0 [56.0, 69.0]      61.0 [56.5, 68.0]
      6               sex = 2 (%)             90 (39.5)              27 (42.9)
      7               ph.ecog (%)                                             
      8                         0             63 (27.8)             63 (100.0)
      9                         1            113 (49.8)                0 (0.0)
      10                        2             50 (22.0)                0 (0.0)
      11                        3               1 (0.4)                0 (0.0)
      12  ph.karno (median [IQR])     80.0 [75.0, 90.0]     90.0 [90.0, 100.0]
      13 pat.karno (median [IQR])     80.0 [70.0, 90.0]      90.0 [80.0, 90.0]
      14  meal.cal (median [IQR]) 975.0 [635.0, 1150.0] 1000.0 [653.8, 1175.0]
      15   wt.loss (median [IQR])       7.0 [0.0, 15.8]        4.0 [0.0, 10.0]
                 ph.ecog_cat: 1      ph.ecog_cat: >=2      p    test
      1                     113                    51               
      2        11.0 [5.0, 15.0]      11.5 [3.2, 16.0]  0.254 nonnorm
      3    243.0 [177.0, 426.0]  180.0 [100.0, 301.0]  0.001 nonnorm
      4               82 (72.6)             45 (88.2)  0.002        
      5       63.0 [55.0, 68.0]     68.0 [60.5, 73.0]  0.002 nonnorm
      6               42 (37.2)             21 (41.2)  0.737        
      7                                               <0.001   exact
      8                 0 (0.0)               0 (0.0)               
      9             113 (100.0)               0 (0.0)               
      10                0 (0.0)             50 (98.0)               
      11                0 (0.0)               1 (2.0)               
      12      80.0 [80.0, 90.0]     70.0 [60.0, 70.0] <0.001 nonnorm
      13      80.0 [70.0, 90.0]     60.0 [60.0, 70.0] <0.001 nonnorm
      14 1025.0 [825.0, 1150.0] 796.5 [472.0, 1075.0]  0.037 nonnorm
      15        6.0 [0.0, 15.0]      10.5 [3.5, 22.8]  0.009 nonnorm

---

    Code
      read.csv("test_output_missing.csv", check.names = FALSE)
    Output
                                Overall ph.ecog_cat: 0 ph.ecog_cat: 1
      1                     n       228             63            113
      2       inst = TRUE (%)   1 (0.4)        0 (0.0)        0 (0.0)
      3       time = TRUE (%)   0 (0.0)        0 (0.0)        0 (0.0)
      4     status = TRUE (%)   0 (0.0)        0 (0.0)        0 (0.0)
      5        age = TRUE (%)   0 (0.0)        0 (0.0)        0 (0.0)
      6        sex = TRUE (%)   0 (0.0)        0 (0.0)        0 (0.0)
      7    ph.ecog = TRUE (%)   1 (0.4)        0 (0.0)        0 (0.0)
      8   ph.karno = TRUE (%)   1 (0.4)        0 (0.0)        0 (0.0)
      9  pat.karno = TRUE (%)   3 (1.3)        1 (1.6)        0 (0.0)
      10  meal.cal = TRUE (%) 47 (20.6)      13 (20.6)      27 (23.9)
      11   wt.loss = TRUE (%)  14 (6.1)        2 (3.2)        7 (6.2)
         ph.ecog_cat: >=2     p test
      1                51    NA   NA
      2           1 (2.0) 0.177   NA
      3           0 (0.0)   NaN   NA
      4           0 (0.0)   NaN   NA
      5           0 (0.0)   NaN   NA
      6           0 (0.0)   NaN   NA
      7           0 (0.0)   NaN   NA
      8           1 (2.0) 0.177   NA
      9           2 (3.9) 0.123   NA
      10         7 (13.7) 0.331   NA
      11          5 (9.8) 0.343   NA

---

    Code
      read.csv("test_output_pairwise.csv", check.names = FALSE)
    Output
                   ph.ecog_cat: 0_ph.ecog_cat: 1 ph.ecog_cat: 0_ph.ecog_cat: >=2
      1       inst                  2.225074e-01                    2.225074e-01
      2       time                  1.457843e-01                    8.157666e-04
      3     status                  8.680617e-02                    3.154529e-03
      4        age                  8.454807e-01                    2.960642e-03
      5        sex                  1.000000e+00                    1.000000e+00
      6    ph.ecog                  9.999000e-05                    9.999000e-05
      7   ph.karno                  8.136144e-12                    2.425136e-34
      8  pat.karno                  1.364685e-02                    1.158220e-13
      9   meal.cal                  5.094785e-01                    1.265793e-01
      10   wt.loss                  9.078558e-02                    6.261166e-03
         ph.ecog_cat: 1_ph.ecog_cat: >=2
      1                     7.774247e-01
      2                     1.011014e-02
      3                     6.501237e-02
      4                     2.960642e-03
      5                     1.000000e+00
      6                     9.999000e-05
      7                     2.078767e-13
      8                     9.885573e-10
      9                     3.162490e-02
      10                    9.078558e-02

# alpha_by_n calculates appropriate thresholds

    Code
      alpha_by_n(500)
    Output
      [1] 5.431508e-06

