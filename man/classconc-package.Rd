% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classconc-package.R
\docType{package}
\name{classconc-package}
\alias{classconc-package}
\title{Classes and methods for concordances between classifications.}
\description{
Social and economic statistics is full of classifications, such as
classifications of occupations, geographic areas, or educational
qualifications.  A concordance is a mapping between two classifications.
Package \code{classconc} contains classes and methods for dealing with
classifications and concordances.
}
\details{
Concordances are held in objects of class \code{\linkS4class{OneToOne}}
or \code{\linkS4class{ManyToOne}}, and are created using function
\code{Concordance}.

The names of the classifications used by a concordances are extracted
or changed using functions \code{\link{classifications}},
\code{\link{classificationFrom}} and \code{\link{classificationTo}}.
The codes within a concordance are extracted using
function\code{\link{codes}}.

Concordances are combined using functions \code{\link{chain}} and
\code{\link{splice}}.

A set of codes from one classification can be converted to another
classification using function \code{\link{translate}}.
}

