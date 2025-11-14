library(withr)

with_tempdir({
  test_that("No files returns NULL", {
    expect_null(combine_files())
  })
})

with_tempdir({
  test_that("Combine multiple files", {
    write.csv(data.frame(a = 1:3, b = c("a", "b", "c")), "file1.csv", row.names = FALSE)
    write.csv(data.frame(a = 4:6, b = c("a", "b", "c")), "file2.csv", row.names = FALSE)

    res <- combine_files(pattern = ".csv$")
    expect_equal(res$a, 1:6)
    expect_equal(res$b, c("a", "b", "c", "a", "b", "c"))
  })
})

with_tempdir({
  test_that("Unique only works", {
    write.csv(data.frame(a = rep(1:2, 3)), "dup.csv", row.names = FALSE)

    res <- combine_files(unique_only = TRUE)
    expect_equal(nrow(res), 2)
  })
})

with_tempdir({
  test_that("Add filename column", {
    write.csv(data.frame(a = 1), "test.csv", row.names = FALSE)

    res <- combine_files(add_file_name = TRUE)
    expect_true(grepl("^origin_file", names(res)[1]))
    expect_equal(res[[1]], "test.csv")
  })
})

with_tempdir({
  test_that("Column name conflict resolution", {
    write.csv(data.frame(origin_file = 1), "conflict.csv", row.names = FALSE)

    res <- combine_files(add_file_name = TRUE)
    expect_equal(names(res)[1], "origin_file_")
  })
})

with_tempdir({
  test_that("Different reader functions", {
    write.csv(data.frame(a = 1), "test.csv", row.names = FALSE)

    res <- combine_files(reader_fun = read.csv)
    expect_equal(class(res$a), "integer")
  })
})
