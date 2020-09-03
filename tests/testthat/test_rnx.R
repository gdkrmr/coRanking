context("R_NX")

df1 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(1., 2., 3., 4.))
df2 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(3., 2., 4., 2.))

q12 <- coranking(df1, df2)
q21 <- coranking(df2, df1)
q11 <- coranking(df1, df1)
q22 <- coranking(df2, df2)

qnx12 <- coRanking:::Q_NX(q12)
qnx21 <- coRanking:::Q_NX(q21)
qnx11 <- coRanking:::Q_NX(q11)
qnx22 <- coRanking:::Q_NX(q22)

qnx12
qnx21
qnx11
qnx22

test_that("Q_NX values", {
  expect_equal(qnx12, qnx21)
  expect_equal(qnx11, qnx22)
  expect_equal(qnx11, c(`1` = 1, `2` = 1, `3` = 1))
})

rnx12 <- R_NX(q12)
rnx21 <- R_NX(q21)
rnx11 <- R_NX(q11)
rnx22 <- R_NX(q22)

test_that("R_NX values", {
  expect_equal(rnx11, rnx22)
  expect_equal(rnx11, c(`1` = 1, `2` = 1))
  expect_equal(unname(rnx12), unname(rev(rnx21)))
})
