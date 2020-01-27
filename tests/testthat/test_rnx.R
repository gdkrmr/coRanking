context("R_NX")

df1 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(1., 2., 3., 4.))
df2 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(3., 2., 4., 2.))

q12 <- coranking(df1, df2)
q21 <- coranking(df2, df1)
q11 <- coranking(df1, df1)
q22 <- coranking(df2, df2)

rnx12 <- coRanking::R_NX(q12)
rnx21 <- coRanking::R_NX(q21)
rnx11 <- coRanking::R_NX(q11)
rnx22 <- coRanking::R_NX(q22)

test_that("R_NX values", {
  expect_equal(rnx11, rnx22)
  expect_equal(rnx11, c(`1` = 1, `2` = 1))
  expect_equal(unname(rnx12), unname(rev(rnx21)))
})
