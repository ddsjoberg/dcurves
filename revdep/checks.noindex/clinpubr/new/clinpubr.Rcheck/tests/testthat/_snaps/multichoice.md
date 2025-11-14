# split_multichoice handles basic splitting

    Code
      result
    Output
        q1_a  q1_b  q1_c  q1_d  q2_a  q2_b  q2_c  q2_d
      1 TRUE  TRUE FALSE FALSE  TRUE  TRUE FALSE FALSE
      2 TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE
      3 TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
      4   NA    NA    NA    NA  TRUE  TRUE FALSE FALSE

# split_multichoice with remove_space=FALSE

    Code
      result
    Output
         q1_a q1_   q1_b  q1_c  q1_d
      1  TRUE TRUE  TRUE FALSE FALSE
      2 FALSE TRUE FALSE  TRUE  TRUE
      3  TRUE TRUE  TRUE FALSE FALSE

# split_multichoice with custom link character

    Code
      result
    Output
         q1-a  q1-b  q1-c  q1-d
      1  TRUE  TRUE FALSE FALSE
      2 FALSE FALSE  TRUE  TRUE

# split_multichoice retains original columns when remove_cols=FALSE

    Code
      colnames(result)
    Output
      [1] "q1"   "q1_a" "q1_b" "q1_c" "q1_d"

# split_multichoice handles empty strings and NAs

    Code
      result
    Output
         q1_a  q1_b
      1    NA    NA
      2 FALSE FALSE
      3  TRUE  TRUE

