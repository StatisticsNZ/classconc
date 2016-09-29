
context("helper-functions")

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

test_that("chain2 works with valid inputs", {
    chain2 <- classconc:::chain2
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c2 = c("x", "z", "y"), c3 = c("h", "i", "j"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c"), c3 = c("h", "j", "i"))
    x3 <- Concordance(d3)
    expect_identical(chain2(x1, x2), x3)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c2 = c("x", "y"), c3 = c("h", "i"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c"), c3 = c("h", "i", "h"))
    x3 <- Concordance(d3)
    expect_identical(chain2(x1, x2), x3)
    d1 <- data.frame(c1 = character(), c2 = character())
    x1 <- Concordance(d1)
    d2 <- data.frame(c2 = character(), c3 = character())
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = character(), c3 = character())
    x3 <- Concordance(d3)
    expect_identical(chain2(x1, x2), x3)
})

test_that("chain2 throws appropriate errors", {
    chain2 <- classconc:::chain2
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- d1[2:1]
    x2 <- Concordance(d2)
    expect_error(chain2(x1, x2),
                 "both concordances use classifications \"c1\" and \"c2\"")
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c3 = c("a", "b", "c"), c4 = c("x", "y", "z"))
    x2 <- Concordance(d2)
    expect_error(chain2(x1, x2),
                 paste("concordances have no classifications in common :",
                       "\"c1\" and \"c2\" vs \"c3\" and \"c4\""))
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = c("a", "b", "c"), c3 = c("i", "j", "i"))
    x2 <- Concordance(d2)
    expect_error(chain2(x1, x2),
                 "attempt to merge using 'from' classification \\[\"c1\"\\] of first concordance")
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c3 = c("i", "j", "k"), c2 = c("x", "y", "y"))
    x2 <- Concordance(d2)
    expect_error(chain2(x1, x2),
                 "attempt to merge using 'to' classification \\[\"c2\"\\] of second concordance")
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c3 = c("i", "j"), c2 = c("x", "z"))
    x2 <- Concordance(d2)
    expect_error(chain2(x1, x2),
                 "two versions of classification \"c2\" contain different values")
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

test_that("splice2 works with valid inputs", {
    splice2 <- classconc:::splice2
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = c("d", "e"), c2 = c("v", "w"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c", "d", "e"),
                     c2 = c("x", "y", "z", "v", "w"))
    x3 <- Concordance(d3)
    expect_identical(splice2(x1, x2), x3)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c2 = c("v", "w"), c1 = c("d", "e"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c", "d", "e"),
                     c2 = c("x", "y", "z", "v", "w"))
    x3 <- Concordance(d3)
    expect_identical(splice2(x1, x2), x3)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "y"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = c("c", "d"), c2 = c("y", "w"))
    x2 <- Concordance(d2)
    d3 <- data.frame(c1 = c("a", "b", "c", "d"),
                     c2 = c("x", "y", "y", "w"))
    x3 <- Concordance(d3)
    expect_identical(splice2(x1, x2), x3)
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = character(), c2 = character())
    x2 <- Concordance(d2)
    expect_identical(splice2(x1, x2), x1)
})

test_that("splice2 throws appropriate errors", {
    splice2 <- classconc:::splice2
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = c("d", "e"), wrong = c("v", "w"))
    x2 <- Concordance(d2)
    expect_error(splice2(x1, x2),
                 "classifications do not match : \"c1\" and \"c2\" vs \"c1\" and \"wrong\"")
    d1 <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
    x1 <- Concordance(d1)
    d2 <- data.frame(c1 = "a", c2 = "z")
    x2 <- Concordance(d2)
    expect_error(splice2(x1, x2),
                 "relationship neither one-to-one nor many-to-one")
})

test_that("tidyObjectToTranslate works", {
    tidyObjectToTranslate <- classconc:::tidyObjectToTranslate
    x <- 1:3
    expect_identical(tidyObjectToTranslate(x),
                     c("1", "2", "3"))
    x <- matrix(1:4, nrow = 2)
    expect_identical(tidyObjectToTranslate(x),
                     c("1", "2", "3", "4"))
    x <- c(1:3, NA)
    expect_error(tidyObjectToTranslate(x), "'object' has missing values")
    ## can't think of any way to create an NA using as.character
    ## so can't test other validity check
})
