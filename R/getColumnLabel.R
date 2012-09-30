##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param data
##' @param colnames
##' @param to.data.frame
##' @return
##' @author ahmadou
getColumnLabel <-
    function(data = NULL, file = NULL, colnames = NULL, to.data.frame = FALSE) {
                if (is.null(file) & !is.null(data)) {
            if (! any(grepl("gretl", class(data))) ) stop("Your data is not of class 'gretldata' !!!")

            if (is.null(attr(data, "filename")))
                stop("No filename attribute in the dataframe !!!")

            doc <- xmlRoot(xmlInternalTreeParse(attr(data, "filename")))
        }
        if (!is.null(file) & is.null(data)) {
            doc <- xmlRoot(xmlInternalTreeParse(file))
        }

        variablelabel <- getNodeSet(doc, "/gretldata//variables/variable")
        variablename <- sapply(variablelabel, xmlGetAttr, "name")
        label <- sapply(variablelabel, xmlGetAttr, "label")
        if (to.data.frame) {
            data.frame(variable = variablename,
                       label = label)
        } else {
            for (i in seq_along(label))
                cat(variablename[i], " : ", label[i], "\n")
        }
    }

