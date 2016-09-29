
context("combine-functions")

test_that("chain works with valid inputs", {
    expect_identical(chain(), NULL)
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(chain(x), x)
    expect_identical(chain(d), x)
    expect_identical(chain(as.matrix(d)), x)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c2 = c("x", "z", "y"), c3 = c("h", "i", "j"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c"), c3 = c("h", "j", "i"))
    x3 <- Concordance(d3)
    expect_identical(chain(x1, x2), x3)
    expect_identical(chain(d1, d2), x3)
    expect_identical(chain(d1, x2), x3)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c2 = c("x", "y", "z"), c3 = c("h", "i", "i"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c3 = c("i", "h"), c4 = c("q", "r"))
    x3 <- Concordance(d3)
    d4 <- data.frame(c1 = c("a", "b", "c"), c4 = c("r", "q", "q"))
    x4 <- Concordance(d4)
    expect_identical(chain(x1, x2, x3), x4)
    expect_identical(chain(x1, d2, x3), x4)
    expect_identical(chain(x1, as.matrix(d2), x3), x4)
})

test_that("chain throws appropriate errors", {
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    x2 <- "wrong"
    expect_error(chain(x1, x2),
                 "'x2' has class \"character\"")
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    x2 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"), c3 = 1:3)
    expect_error(chain(x1, x2),
                 "could not make concordance from 'x2' : does not have two columns")
})

test_that("splice works with valid inputs", {
    expect_identical(splice(), NULL)
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(splice(x), x)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = c("d", "e"), c2 = c("v", "w"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c", "d", "e"),
                     c2 = c("x", "y", "z", "v", "w"))
    x3 <- Concordance(d3)
    expect_identical(splice(x1, x2), x3)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = c("d", "e"), c2 = c("v", "w"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c2 = c("w", "u"), c1 = c("e", "f"))
    x3 <- Concordance(d3)
    d4 <- data.frame(c1 = c("a", "b", "c", "d", "e", "f"),
                     c2 = c("x", "y", "z", "v", "w", "u"))
    x4 <- Concordance(d4)
    expect_identical(splice(x1, x2, x3), x4)
    expect_identical(splice(x1, x2, x3, x3, x2), x4)
})

test_that("splice throws appropriate errors", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_error(splice(x, "wrong"),
                 "object with class \"character\"")
})
