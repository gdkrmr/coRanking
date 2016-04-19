
df1 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(1., 2., 3., 4.))
df2 <- data.frame(x = c(2., 3., 4., 5.),
                  y = c(3., 2., 4., 2.))

m1 <- matrix(c(2., 3., 4., 5.,
               1., 2., 3., 4.), ncol = 2)
m2 <- matrix(c(2., 3., 4., 5.,
               3., 2., 4., 2.), ncol = 2)


context('coranking matrices')

test_that('output dimensions', {
    expect_equal(nrow(coranking(df1, df2, use = 'C')),
                 nrow(df1) - 1)
    expect_equal(nrow(coranking(df1, df2, use = 'R')),
                 nrow(df1) - 1)
    expect_equal(nrow(coranking(m1, m2, use = 'C')),
                 nrow(df2) - 1)
    expect_equal(nrow(coranking(m1, m2, use = 'R')),
                 nrow(df2) - 1)
    expect_equal(nrow(coranking(df1, m2, use = 'R')),
                 nrow(df2) - 1)
})

test_that('data.frame and matrix', {
    expect_equal(coranking(df1, df2, use = 'C'),
                 coranking(m1, m2,   use = 'C'))
    expect_equal(coranking(df1, df2, use = 'R'),
                 coranking(m1, m2,   use = 'R'))
})

test_that('equal data results in eye', {
    expect_equal(coranking(df2, df2, use = 'C'),
                 diag(nrow(df2) - 1))
    expect_equal(coranking(df2, df2,  use = 'R'),
                 diag(nrow(df2) - 1))
    expect_equal(coranking(df1, df1, use = 'C'),
                 diag(nrow(df1) - 1))
    expect_equal(coranking(df1, df1,  use = 'R'),
                 diag(nrow(df1 - 1)))
    expect_equal(coranking(m2, m2, use = 'C'),
                 diag(nrow(m2) - 1))
    expect_equal(coranking(m2, m2,  use = 'R'),
                 diag(nrow(m2) - 1))
    expect_equal(coranking(m1, m1, use = 'C'),
                 diag(nrow(m1) - 1))
    expect_equal(coranking(m1, m1,  use = 'R'),
                 diag(nrow(m2) - 1))
})

test_that('changing arguments transposes results', {
    expect_equal(coranking(df1, df2, use = 'C'),
                 coranking(df2, df1, use = 'C'))
    expect_equal(coranking(m2, m1, use = 'R'),
                 coranking(m1, m2, use = 'R'))
})


test_that('C and R backend equal', {
    expect_equal(coranking(df1, df2, use = 'C'),
                 coranking(df1, df2, use = 'R'))
    expect_equal(coranking(df2, m1, use = 'C'),
                 coranking(df2, m1, use = 'R'))
    expect_equal(coranking(m1, m2, use = 'C'),
                 coranking(m1, m2, use = 'R'))
    expect_equal(coranking(m2, m1, use = 'C'),
                 coranking(m2, m1, use = 'R'))
})

df3 <- matrix(letters[1:8], ncol = 2)
test_that('errors', {
    expect_error(coranking(df1, df3, use = 'C'),
                 coranking(df3, df1, use = 'C'))
    expect_error(coranking(df1, df3, use = 'R'),
                 coranking(df3, df1, use = 'R'))
    expect_error(coranking(df1, df3, use = 'C'),
                 coranking(df3, df1, use = 'R'))
})
