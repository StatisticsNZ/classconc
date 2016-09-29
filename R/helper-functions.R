


## HAS_TESTS
chain2 <- function(e1, e2) {
    classif1 <- classifications(e1)
    classif2 <- classifications(e2)
    values1 <- getValues(e1)
    values2 <- getValues(e2)
    i.classif <- match(classif1, classif2, nomatch = 0L)
    match.on.both <- identical(i.classif, 1:2) || identical(i.classif, 2:1)
    if (match.on.both)
        stop(gettextf("both concordances use classifications \"%s\" and \"%s\"",
                      classif1[1L], classif1[2L]))
    match.on.neither <- identical(i.classif, c(0L, 0L))
    if (match.on.neither)
        stop(gettextf("concordances have no classifications in common : \"%s\" and \"%s\" vs \"%s\" and \"%s\"",
                      classif1[1L], classif1[2L], classif2[1L], classif2[2L]))
    match.uses.first.col.e1 <- i.classif[1L] > 0L
    if (match.uses.first.col.e1 && methods::is(e1, "ManyToOne"))
        stop(gettextf("attempt to merge using '%s' classification [\"%s\"] of first concordance",
                      "from", classif1[1L]))
    match.uses.first.col.e2 <- identical(max(i.classif), 1L)
    if (!match.uses.first.col.e2 && methods::is(e2, "ManyToOne"))
        stop(gettextf("attempt to merge using '%s' classification [\"%s\"] of second concordance",
                      "to", classif2[2L]))
    i.match.1 <- 2L - match.uses.first.col.e1
    i.match.2 <- 2L - match.uses.first.col.e2
    values.match.1 <- values1[ , i.match.1]
    values.match.2 <- values2[ , i.match.2]
    if (length(setdiff(values.match.1, values.match.2)) > 0L) {
        classif.match <- classif1[2L - match.uses.first.col.e1]
        stop(gettextf("two versions of classification \"%s\" contain different values",
                      classif.match))
    }
    i.keep.1 <- 1L + match.uses.first.col.e1
    i.keep.2 <- 1L + match.uses.first.col.e2
    values.keep.1 <- values1[ , i.keep.1]
    values.keep.2 <- values2[ , i.keep.2]
    classif.keep.1 <- classif1[i.keep.1]
    classif.keep.2 <- classif2[i.keep.2]
    i <- match(values.match.1, values.match.2)
    values <- cbind(values.keep.1, values.keep.2[i])
    colnames(values) <- c(classif.keep.1, classif.keep.2)
    Concordance(values)
}


## HAS_TESTS
splice2 <- function(e1, e2) {
    classif1 <- classifications(e1)
    classif2 <- classifications(e2)
    values1 <- getValues(e1)
    values2 <- getValues(e2)
    i.classif <- match(classif1, classif2, nomatch = 0L)
    if (identical(i.classif, 1:2))
        object <- rbind(values1, values2)
    else if (identical(i.classif, 2:1))
        object <- rbind(values1, values2[, 2:1])
    else
        stop(gettextf("classifications do not match : \"%s\" and \"%s\" vs \"%s\" and \"%s\"",
                      classif1[1L], classif1[2L], classif2[1L], classif2[2L]))
    colnames(object) <- classif1
    Concordance(object)
}

## HAS_TESTS
tidyObjectToTranslate <- function(object) {
    if (any(is.na(object)))
        stop(gettextf("'%s' has missing values", "object"))
    ans <- as.character(object)
    if (any(is.na(ans)))
        stop(gettextf("NAs created during coercion to type \"%s\"",
                      "character"))
    ans
}

