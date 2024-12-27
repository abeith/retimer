
data(mm1)

test_that("mm1 is a tuneR WAVE object", {
    expect_s4_class(mm1, "Wave")
})

test_that("length of mm1 is correct", {
    expect_length(mm1, 42320)
})
