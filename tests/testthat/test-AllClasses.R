
context("AllClasses")


test_that("can create valid object of class ManyToOne", {
    x <- new("ManyToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "x")),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
    x <- new("ManyToOne",
             values = matrix(character(), nrow = 0, ncol = 2),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
})

test_that("validity tests inherited from Concordance work", {
    x <- new("ManyToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "x")),
             classifications = c("c1", "c2"))
    ## 'values' has two columns
    x.wrong <- x
    x.wrong@values <- cbind(x.wrong@values, c("e", "f", "g"))
    expect_error(validObject(x.wrong),
                 "'values' does not have 2 columns")
    ## 'values' is character
    x.wrong <- x
    x.wrong@values <- matrix(1:6, nrow = 3)
    expect_error(validObject(x.wrong),
                 "'values' does not have type \"character\"")
    ## 'values' does not have missing values
    x.wrong <- x
    x.wrong@values[1] <- NA
    expect_error(validObject(x.wrong),
                 "'values' has missing values")
    ## 'values' does not have dimnames
    x.wrong <- x
    colnames(x.wrong@values) <- c("a", "b")
    expect_error(validObject(x.wrong),
                 "'values' has dimnames")
    ## every row of 'values' is unique
    x.wrong <- x
    x.wrong@values[3,] <- c("a", "x")
    expect_error(validObject(x.wrong),
                 "'values' has duplicate rows")
    ## 'classifications' has length 2
    x.wrong <- x
    x.wrong@classifications <- c("c1", "c2", "wrong")
    expect_error(validObject(x.wrong),
                 "'classifications' does not have length 2")
    ## 'classifications' does not have missing values
    x.wrong <- x
    x.wrong@classifications[1] <- NA
    expect_error(validObject(x.wrong),
                 "'classifications' has missing values")
    ## 'classifications' does not have blanks
    x.wrong <- x
    x.wrong@classifications[1] <- ""
    expect_error(validObject(x.wrong),
                 "'classifications' has blanks")
    ## 'classifications' does not have duplicates
    x.wrong <- x
    x.wrong@classifications[2] <- "c1"
    expect_error(validObject(x.wrong),
                 "'classifications' has duplicates \\[\"c1\"\\]")
})

test_that("validity tests inherited from ManyToOne work", {
    x <- new("ManyToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "x")),
             classifications = c("c1", "c2"))
    ## labels.from has no duplicates
    x.wrong <- x
    x.wrong@values[1] <- "b"
    expect_error(validObject(x.wrong),
                 "'from' classification \\[\"c1\"\\] has duplicates")
    ## labels.from has no duplicates
    x.wrong <- x
    x.wrong@values[3,2] <- "z"
    expect_error(validObject(x.wrong),
                 "'to' classification \\[\"c2\"\\] has no duplicates")
})

test_that("can create valid object of class OneToOne", {
    x <- new("OneToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "z")),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
    x <- new("OneToOne",
             values = matrix(character(), nrow = 0, ncol = 2),
             classifications = c("c1", "c2"))
    expect_true(validObject(x))
})

test_that("validity tests inherited from OneToOne work", {
    x <- new("OneToOne",
             values = cbind(c("a", "b", "c"), c("x", "y", "z")),
             classifications = c("c1", "c2"))
    ## neither column has duplicates
    x.wrong <- x
    x.wrong@values[1,1] <- "b"
    expect_error(validObject(x.wrong),
                 "classification \"c1\" has duplicates")
    x.wrong <- x
    x.wrong@values[1,2] <- "y"
    expect_error(validObject(x.wrong),
                 "classification \"c2\" has duplicates")
})

