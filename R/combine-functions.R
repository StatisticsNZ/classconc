
## HAS_TESTS
#' Chain concordances.
#' 
#' Combine two or more concordances to form a new concordance via a
#' \code{\link[base]{merge}}-like operation.  For instance, if \code{x} is a
#' concordance between classification \code{c1} and classification \code{c2},
#' and \code{y} is a concordance between \code{c2} and \code{c3}, then
#' \code{chain(x, y)} creates a concordance between \code{c1} and \code{c3}.
#' 
#' Two concordances that are to be chained together must have a classification
#' in common.  In addition, the two versions of the common classification must
#' have the same set of values, though not necessarily in the same order.
#' Concordances are merged from left to right.
#' 
#' @param \dots Objects of class \code{\linkS4class{Concordance}}.
#' 
#' @return An object of class \code{\linkS4class{Concordance}}.
#'
#' @seealso \code{splice} combines concordances via a
#' \code{\link[base]{rbind}}-like operation.
#' 
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("e", "f", "f"))
#' x <- Concordance(x)
#' y <- cbind(c2 = c("e", "f"), c3 = c("i", "j"))
#' y <- Concordance(y)
#' z <- cbind(c3 = c("i", "j"), c4 = c("m", "l"))
#' z <- Concordance(z)
#' x
#' y
#' z
#' chain(x, y)
#' chain(x, y, z)
#' 
#' x <- cbind(c1 = c("a", "b"), c2 = c("e", "f"))
#' x <- Concordance(x)
#' y <- cbind(c3 = c("i", "j"), c2 = c("e", "f"))
#' y <- Concordance(y)
#' z <- cbind(c3 = c("i", "j", "k"), c2 = c("e", "e", "f"))
#' z <- Concordance(z)
#' x
#' y
#' z
#' ## The order of the columns is irrelevant in a "OneToOne" object
#' chain(x, y)
#' ## ...but not a "ManyToOne" object
#' \dontrun{chain(x, z)}
#' @export
chain <- function(...) {
    names.objects <- as.character(substitute(list(...)))[-1L]
    objects <- list(...)
    n <- length(objects)
    if (n == 0L)
        return(NULL)
    for (i in seq_len(n)) {
        object <- objects[[i]]
        if (!methods::is(object, "Concordance")) {
            if (identical(length(dim(object)), 2L)) {
                object <- tryCatch(Concordance(object),
                                   error = function(e) e)
                if (methods::is(object, "error"))
                    stop(gettextf("could not make concordance from '%s' : %s",
                                  names.objects[i], object$message))
                objects[[i]] <- object
            }
            else
                stop(gettextf("'%s' has class \"%s\"",
                              names.objects[i], class(object)))
        }
    }
    ans <- objects[[1L]]
    if (n >= 2L) {
        for (i in 2:n)
            ans <- chain2(e1 = ans, e2 = objects[[i]])
    }
    ans
}


## HAS_TESTS
#' Splice concordances.
#' 
#' Combine two or more concordances to form a new concordance, via
#' a \code{\link[base]{rbind}}-like operation.
#' 
#' Concordances that are to be spliced together must have classifications with
#' the same names.  The result of the splicing must also be a many-to-one or
#' one-to-one relationship between classifications.  Duplicate rows are
#' deleted.
#' 
#' @param \dots Objects of class \code{\linkS4class{Concordance}}.
#'
#' @return An object of class \code{\linkS4class{Concordance}}.
#' 
#' @seealso \code{\link{chain}} combines concordances via a
#' \code{\link[base]{merge}}-like operation.
#'
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("q", "r", "r"))
#' x <- Concordance(x)
#' y <- cbind(c1 = c("e", "f"), c2 = c("s", "t"))
#' y <- Concordance(y)
#' x
#' y
#' splice(x, y)
#' 
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("q", "r", "r"))
#' x <- Concordance(x)
#' y <- cbind(c2 = c("r", "s"), c1 = c("c", "d"))
#' y <- Concordance(y)
#' x
#' y  ## columns in different order, and rows overlap with 'x'
#' splice(x, y)
#' 
#' @export
splice <- function(...) {
    objects <- list(...)
    not.concordance <- !sapply(objects, methods::is,"Concordance")
    if (any(not.concordance)) {
        i.first.wrong <- which(not.concordance)[1L]
        class.first.wrong <- class(objects[[i.first.wrong]])
        stop(gettextf("object with class \"%s\"",
                      class.first.wrong))
    }
    n <- length(objects)
    if (n == 0L)
        return(NULL)
    ans <- objects[[1L]]
    if (n >= 2L) {
        for (i in 2:n)
            ans <- splice2(e1 = ans,
                           e2 = objects[[i]])
    }
    ans
}
