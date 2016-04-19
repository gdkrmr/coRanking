
df1 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(1., 2., 3., 4.))
df2 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(3., 2., 4., 2.))

m1 <- matrix(c(2., 3., 4., 5.,
               1., 2., 3., 4.), ncol = 2)
m2 <- matrix(c(2., 3., 4., 5.,
               3., 2., 4., 2.), ncol = 2)


context('rankmatrix')

test_that('output dimensions', {
    expect_equal(nrow(rankmatrix(df1, use = 'C')),
                 nrow(df1))
    expect_equal(nrow(rankmatrix(df1, use = 'R')),
                 nrow(df1))
    expect_equal(nrow(rankmatrix(m1, use = 'C')),
                 nrow(m1))
    expect_equal(nrow(rankmatrix(m1, use = 'R')),
                 nrow(m1))
    expect_equal(nrow(rankmatrix(m2, use = 'R')),
                 nrow(m2))
})

test_that('data.frame and matrix', {
    expect_equal(rankmatrix(df1, use = 'C'),
                 rankmatrix(m1,  use = 'C'))
    expect_equal(rankmatrix(df1, use = 'R'),
                 rankmatrix(m1,  use = 'R'))
})

test_that('C and R backend equal', {
    expect_equal(rankmatrix(df1, use = 'C'),
                 rankmatrix(df1, use = 'R'))
    expect_equal(rankmatrix(df2, use = 'C'),
                 rankmatrix(df2, use = 'R'))
    expect_equal(rankmatrix(m1, use = 'C'),
                 rankmatrix(m1, use = 'R'))
    expect_equal(rankmatrix(m2, use = 'C'),
                 rankmatrix(m2, use = 'R'))
})

df3 <- matrix(letters[1:8], ncol = 2)
test_that('errors', {
    expect_error(rankmatrix(df3, use = 'C'),
                 rankmatrix(df3, use = 'R'))
})
