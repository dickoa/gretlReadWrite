##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param file
##' @param keep.label
##' @param keep.description
##' @param forceDF
##' @return
##' @author ahmadou
read.gdt <-
    function(file, panelindexes = NULL, forceDF = FALSE) {

        doc <- xmlRoot(xmlInternalTreeParse(file))

        ## metadata
        metadata <- getNodeSet(doc, "//gretldata")
        typeofdata <- sapply(metadata, xmlGetAttr, "type")
        startobs <- sapply(metadata, xmlGetAttr, "startobs")
        endobs <- sapply(metadata, xmlGetAttr, "endobs")

        ## get variable name
        variablelabel <- getNodeSet(doc, "/gretldata//variables/variable")
        variablename <- sapply(variablelabel, xmlGetAttr, "name")
        ## value
        value <- xpathApply(doc, "/gretldata//observations/obs", xmlValue)
        ## warning because of NA coercion when the data
        ## have missing value, e.g as.numeric("NA")
        value <- suppressWarnings(lapply(value, function(x) as.numeric(strsplit(x, " ")[[1]])))
        value <- do.call("rbind", value)
        value <- as.data.frame(value)
        names(value) <- variablename
        class(value) <- c("gretldata.frame", "data.frame")

        if (typeofdata == "time-series") {
            startobs <- as.numeric(unlist(strsplit(startobs, ":")[[1]]))
            endobs <- as.numeric(unlist(strsplit(endobs, ":")[[1]]))
            frequency <- as.numeric(sapply(metadata, xmlGetAttr, "frequency"))
            value <- ts(value, start = startobs, end = endobs, frequency = frequency)
            class(value) <- c("mts", "gretlts", "ts")
        }

        if (typeofdata == "stacked-time-series" & !is.null(panelindexes) ) {
           require(plm, quietly = TRUE)
           value <- plm.data(value, indexes = panelindexes)
           class(value) <- c("gretlplm", "plm.dim", "data.frame")

        }

        if (forceDF | typeofdata == "cross-section") {
            attr(value, "filename") <- file
            return(value)
        }

        attr(value, "filename") <- file
        value
    }

#### todo
## add support for other  metadata according to dtd specification (gretldatadtd)
## write a write_gdt.R
