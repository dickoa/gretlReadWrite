#' Exchange data between R and Gretl
#'
#' gretlReadWrite provides tools that make it easier to exchange
#' data between R and gretl (and vice-versa)
#'
#' From gretl to R
#'
#' The main function to read gretl gdt file is \code{\link{read.gdt}}
#'
#'
#' From R to gretl
#'
#' To export R object into gdt file, users should use \code{\link{write.gdt}}
#'
#'
#' Gretl data metadata into R
#'
#' Sometime is useful to have information about the data
#' before importing the data in R. To know about variable
#' name and label correspondance, use \code{\link{getColumnLabel}}.
#' Users can also get the description of the gretldata using
#' \code{link{describeData}}.
#'
#'
#' @references gretl data document type Definition
#' \url{http://gretl.sourceforge.net/gretldata.dtd.html} .
#' @import XML
#' @docType package
#' @name gretlReadWrite
#' @aliases gretlReadWrite gretlReadWrite-package
NULL
