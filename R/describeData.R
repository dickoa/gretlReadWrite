##' Describe metadata
##'
##'
##' @export describeData
##' @param data
##' @param file
##' @return return metadata about gretldata object
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
describeData <-
    function(data = NULL, file = NULL, to.character = FALSE) {
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
        if (to.character) {
            description
        } else {
            cat(description, "\n")
        }

    }

