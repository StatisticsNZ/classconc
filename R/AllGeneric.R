
#' Coerce concordances to other formats.
#'
#' @param x A \code{\linkS4class{Concordance}}
#' @param row.names \code{NULL} or a character vector giving the row names for
#' the data frame.
#' @param optional Logical.  See \code{\link[base]{as.data.frame}}.
#' @param stringsAsFactors  Whether codes should be represented as factors.
#' @param \dots additional arguments to be passed to or from methods.
#'
#' @return An matrix, data.frame, or list.
#'
#' @name coercion
NULL

setGeneric("as.data.frame")

setGeneric("as.list")

setGeneric("as.matrix")

#' Names of classifications used by concordances.
#'
#' Get and set names of classifications used by objects of class
#' \code{\link{Concordance}}.
#'
#' \code{classificationFrom} and \code{classificationTo} can only be used with
#' concordances of class \code{\linkS4class{ManyToOne}}, since the idea of a
#' 'from' classification or 'to' classification is only defined for a
#' many-to-one concordance. \code{classifications} can be used with concordances
#' of class \code{\linkS4class{ManyToOne}} or \code{\linkS4class{OneToOne}}.
#'
#' \code{value} has length 2 with function \code{classifications} and length 1
#' with functions \code{classificationFrom} or \code{classificationTo}.
#'
#' @param object Object of class \code{\linkS4class{Concordance}}.
#' 
#' @param value A character vector.
#'
#' @return A character vector, or in the case of the replacement function,
#' a concordance with new classification names.
#'
#' @seealso The codes from a concordance can be extracted using
#' \code{\link{codes}}.
#'
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
#' x <- Concordance(x)
#' x
#' classifications(x)
#' classifications(x)[1] <- "classif1"
#' classifications(x)
#' classificationFrom(x)
#' classificationTo(x)
#' @name classifications
NULL


#' @rdname classifications
#' @export
setGeneric("classificationFrom",
           function(object)
           standardGeneric("classificationFrom"))

#' @rdname classifications
#' @export
setGeneric("classificationFrom<-",
           function(object, value)
           standardGeneric("classificationFrom<-"))

#' @rdname classifications
#' @export
setGeneric("classifications",
           function(object)
           standardGeneric("classifications"))

#' @rdname classifications
#' @export
setGeneric("classifications<-",
           function(object, value)
           standardGeneric("classifications<-"))

#' @rdname classifications
#' @export
setGeneric("classificationTo",
           function(object)
           standardGeneric("classificationTo"))

#' @rdname classifications
#' @export
setGeneric("classificationTo<-",
           function(object, value)
           standardGeneric("classificationTo<-"))


#' Extract the codes used by a concordance.
#'
#' Extract the codes for one of the classifications used by an object of
#' class \code{\linkS4class{Concordance}}.
#'
#' @param object Object of class \code{\linkS4class{Concordance}}.
#'
#' @param classification Name of a classification.
#'
#' @return A character vector.
#'
#' @seealso Names of classifications can be extracted or changed using
#' \code{\link{classifications}}.
#'
#' @examples
#' x <- data.frame(code = 1:3,
#'                 descriptor = c("North Island", "South Island",
#'                                "Stewart Island"))
#' x <- Concordance(x)
#' x
#' codes(x, classification = "code")
#' codes(x, classification = "descriptor")
#' 
#' x <- data.frame(sex1 = c("girl", "boy", "woman", "man"),
#'                 sex2 = c("female", "male"))
#' x <- Concordance(x)
#' x
#' codes(x, classification = "sex1")
#' codes(x, classification = "sex2")
#' @export
setGeneric("codes",
           function(object, classification)
           standardGeneric("codes"))


#' Test whether codes in a concordance have fixed meanings.
#'
#' Test whether any codes in an object of class
#' \code{\linkS4class{Concordance}} are ambiguous.  A code is ambiguous if it
#' appears in more than one classification, unless it only ever maps on to
#' itself.  See below for examples.
#'
#' @param object Object of class \code{\linkS4class{Concordance}}.
#'
#' @return Logical.
#'
#' @seealso The codes used by a concordances can be extracted using
#' \code{\link{codes}}.
#'
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "y", "x"))
#' x <- Concordance(x)
#' ## no codes found in both classifications
#' codesAmbiguous(x)
#' 
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("a", "y", "x"))
#' x <- Concordance(x)
#' ## "a" found in both classifications, but maps on to itself.
#' codesAmbiguous(x)
#' 
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "a", "x"))
#' x <- Concordance(x)
#' ## "a" found in both classifications, mapping on to different values.
#' codesAmbiguous(x)
#' @export
setGeneric("codesAmbiguous",
           function(object)
           standardGeneric("codesAmbiguous"))

setGeneric("getValues",
           function(object)
           standardGeneric("getValues"))

#' Translate between classifications.
#' 
#' Translate between classifications, based on a concordance.
#' 
#' If \code{concordance} is \code{\linkS4class{ManyToOne}}, then
#' \code{object} must be the 'many' part, and argument \code{to} is not
#' required.  If \code{concordance} is \code{\linkS4class{OneToOne}}, and
#' \code{to} is not supplied, \code{translate} guess, based on the values
#' in \code{object}.
#' 
#' @param object A character vector.
#' @param concordance An object of class \code{\linkS4class{Concordance}}.
#' @param to Name of classification within \code{concordance} that codes from
#' \code{object} should be translated to.
#' @param \dots Not currently used.
#'
#' @return A character vector, the same length as \code{object}.
#'
#' @examples
#' conc <- data.frame(code = 1:3,
#'                    descriptor = c("North Island", "South Island",
#'                                    "Stewart Island"))
#' conc <- Concordance(conc)
#' conc
#' translate(c("1", "3", "1", "2"),
#'           concordance = conc)
#' translate(c("Stewart Island", "North Island"),
#'           concordance = conc)
#' 
#' 
#' codes(conc, classification = "code")
#' codes(conc, classification = "descriptor")
#' 
#' conc <- data.frame(sex1 = c("girl", "boy", "woman", "man"),
#'                    sex2 = c("female", "male"))
#' conc <- Concordance(conc)
#' conc
#' translate(c("man", "girl"), concordance = conc)
#' \dontrun{
#' translate(c("female", "male"), concordance = conc)
#' }
#' @export
setGeneric("translate",
           function(object, concordance, to = NULL, ...)
           standardGeneric("translate"))
