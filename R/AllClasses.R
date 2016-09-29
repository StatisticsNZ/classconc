## CONCORDANCE ########################################################################

## HAS_TESTS
#' S4 class for holding concordances.
#'
#' An object of class \code{Concordance} describes how codes from one
#' classification map on to codes from another classification.
#' The superclass \code{Concordance} has two subclasses:
#' \code{ManyToOne} and \code{OneToOne}.
#'
#' @param object An object of class \code{Concordance}
#' 
#' @slot values A character matrix.
#' @slot classifications A character vector of length 2.
#'
#' @seealso Objects are created using function \code{\link{Concordance}}.
#'
#' @export
setClass("Concordance",
         slots = c(values = "matrix",
             classifications = "character"),
         contains = "VIRTUAL",
         validity = function(object) {
             values <- getValues(object)
             classifications <- classifications(object)
             ## 'values' has two columns
             if (!identical(ncol(values), 2L))
                 return(gettextf("'%s' does not have 2 columns", "values"))
             ## 'values' is character
             if (!is.character(values))
                 return(gettextf("'%s' does not have type \"%s\"", "values", "character"))
             ## 'values' does not have missing values
             if (any(is.na(values)))
                 return(gettextf("'%s' has missing values", "values"))
             ## 'values' does not have dimnames
             if (!identical(dimnames(values), NULL))
                 return(gettextf("'%s' has dimnames", "values"))
             ## every row of 'values' is unique
             if (anyDuplicated(values) > 0L)
                 return(gettextf("'%s' has duplicate rows", "values"))
             ## 'classifications' has length 2
             if (!identical(length(classifications), 2L))
                 return(gettextf("'%s' does not have length %d",
                                 "classifications", 2L))
             ## 'classifications' does not have missing values
             if (any(is.na(classifications)))
                 return(gettextf("'%s' has missing values", "classifications"))
             ## 'classifications' does not have blanks
             if (!all(nzchar(classifications)))
                 return(gettextf("'%s' has blanks", "classifications"))
             ## 'classifications' does not have duplicates
             if (anyDuplicated(classifications) > 0L)
                 return(gettextf("'%s' has duplicates [\"%s\"]",
                                 "classifications", classifications[1L]))
             TRUE
         })

## HAS_TESTS
#' @rdname Concordance-class
#' @export
setClass("ManyToOne",
         contains = "Concordance",
         validity = function(object) {
             classif.from <- classificationFrom(object)
             classif.to <- classificationTo(object)
             codes.from <- codes(object, classification = classif.from)
             codes.to <- codes(object, classification = classif.to)
             ## codes.from has no duplicates
             if (anyDuplicated(codes.from) > 0L)
                 return(gettextf("'from' classification [\"%s\"] has duplicates",
                                 classif.from))
             ## codes.to has at least one duplicate
             if (length(codes.to) > 0L && anyDuplicated(codes.to) == 0L)
                 return(gettextf("'to' classification [\"%s\"] has no duplicates",
                                 classif.to))
             TRUE
         })

## HAS_TESTS
#' @rdname Concordance-class
#' @export
setClass("OneToOne",
         contains = "Concordance",
         validity = function(object) {
             values <- getValues(object)
             classifications <- classifications(object)
             ## neither column has duplicates
             for (i in 1:2) {
                 if (anyDuplicated(values[, i]) > 0L)
                     return(gettextf("classification \"%s\" has duplicates",
                                     classifications[i]))
             }
             TRUE
         })
