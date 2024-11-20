
mm1 <- data(mm1)

test_that("mm1 is a data frame", {
    expect_s3_class(mm1, "data.frame")
})
