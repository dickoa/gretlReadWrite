##' Export R object data to gretl gdt
##'
##' @export  write.gdt write.gdt.matrix write.gdt.data.frame
##' @aliases write.gdt write.gdt.matrix write.gdt.data.frame
##' @param data
##' @param filename
##' @param ...
##' @return a gdt file
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
write.gdt <- function(data, filename, ...) UseMethod("write.gdt")



##' Export R object data to gretl gdt
##'
##' @export  write.gdt write.gdt.matrix write.gdt.data.frame
##' @aliases write.gdt write.gdt.matrix write.gdt.data.frame
##' @param data
##' @param filename
##' @param dataname
##' @param description
##' @param varlabel
##' @param obslabels
##' @param encoding
##' @param startobs
##' @param endobs
##' @param frequency
##' @param typeofdata
##' @return a gdt file
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
write.gdt.matrix <- function(data, filename, dataname = "Rdata", description = "", varlabel = NULL, obslabels = "false", encoding = "UTF-8", startobs = 1, endobs = nrow(data), frequency = 1, typeofdata = "cross-section") {
      out <- xmlTree("gretldata", attrs = c(name = dataname, frequency = frequency, startobs = startobs, endobs = endobs, type = typeofdata), dtd = 'gretldata SYSTEM "gretldata.dtd"')
      out$addNode("description", description)
      out$addNode("variables", attrs = c(count = as.character(ncol(data))), close = FALSE)
      colnames <- colnames(data)
      if (is.null(colnames)) colnames <- paste0("V", seq_len(ncol(data)))
      variableAttr <- list(name = colnames, label = varlabel)
      variableAttr <- do.call("cbind", variableAttr)
      variableAttr <- apply(variableAttr, 1, function(x) out$addNode("variable", attrs = x))
      out$closeTag()
      out$addNode("observations", attrs = c(count = as.character(nrow(data)), labels = obslabels), close = FALSE)
      obs <- apply(data, 1, paste, collapse = " ")
      obs <- lapply(obs, function(x) out$addNode("obs", x))
      out$closeTag()
      out$closeTag()
      saveXML(out$value(), file = filename, encoding = encoding)


}

##' Export R object data to gretl gdt
##'
##' @export  write.gdt write.gdt.matrix write.gdt.data.frame
##' @aliases write.gdt write.gdt.matrix write.gdt.data.frame
##' @param data
##' @param filename
##' @param ...
##' @return a gdt file
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
write.gdt.ts <- function(data, filename, ...) {
    startobs <- paste(sprintf("%02d", start(data)), collapse = ":")
    endobs <- paste(sprintf("%02d", end(data)), collapse = ":")
    frequency <- frequency(data)
    typeofdata <- "time-series"
    write.gdt.matrix(as.matrix(data), filename, startobs = startobs, endobs = endobs, frequency = frequency, typeofdata = typeofdata, ...)
}


##' Export R object data to gretl gdt
##'
##' @export  write.gdt write.gdt.matrix write.gdt.data.frame
##' @aliases write.gdt write.gdt.matrix write.gdt.data.frame
##' @param data
##' @param filename
##' @param ...
##' @return a gdt file
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
write.gdt.data.frame <- function(data, filename, ...) {
    data <- as.matrix(data)
    write.gdt.matrix(data, filename, ...)
}


## ##' Export R object data to gretl gdt
## ##'
## ##' @export  write.gdt write.gdt.matrix write.gdt.data.frame write.gdt.gretldata
## ##' @aliases write.gdt write.gdt.matrix write.gdt.data.frame write.gdt.gretldata
## ##' @param data
## ##' @param filename
## ##' @param encoding
## ##' @return a gdt file
## ##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
## write.gdt.gretldata <- function(data, filename, encoding = "UTF-8") {
##         ## export data into xml
##       meta <- getAllMetaData(attr(data, "filename"))
##       out <- xmlTree("gretldata", attrs = unlist(meta$gretldata), dtd = 'gretldata SYSTEM "gretldata.dtd"')
##       out$addNode("description", xmlTextNode(describeData(data, to.character = TRUE)))
##       out$addNode("variables", attrs = unlist(meta$variablesmeta), close = FALSE)
##       variableAttr <- do.call("cbind", meta$variable)
##       variableAttr <- apply(variableAttr, 1, function(x) out$addNode("variable", attrs = x))
##       out$closeTag()
##       out$addNode("observations", attrs = unlist(meta$observations), close = FALSE)
##       obs <- apply(data, 1, paste, collapse = " ")
##       obs <- lapply(obs, function(x) out$addNode("obs", x))
##       out$closeTag()
##       out$closeTag()
##       saveXML(out$value(), file = filename, encoding = encoding)

##     }


### to do write panel data
