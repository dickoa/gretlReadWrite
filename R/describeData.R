##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param data
##' @return
##' @author ahmadou
describeData <-
    function(data = NULL, file = NULL) {
        if (is.null(file) & !is.null(data)) {
            if (! any(grepl("gretl", class(data))) ) stop("Your data is not of class 'gretldata' !!!")

            if (is.null(attr(data, "filename")))
                stop("No filename attribute in the dataframe !!!")

            doc <- xmlRoot(xmlInternalTreeParse(attr(data, "filename")))
        }
        if (!is.null(file) & is.null(data)) {
            doc <- xmlRoot(xmlInternalTreeParse(file))
        }

        if (is.null(file) & is.null(data)) stop("You have to enter gretlobject or the filepath of the gdt file")

### metadata description
        description <- xpathSApply(doc, "/gretldata//description", xmlValue)
        cat(description, "\n")
    }

