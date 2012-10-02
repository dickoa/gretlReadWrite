##' Get label for variable name
##'
##' @export getColumnLabel
##' @param data
##' @param file
##' @param colnamesma
##' @param to.data.frame
##' @return a data.frame or a simple message displaying label and var (no object)
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
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
            Df <- data.frame(variable = variablename,
                             label = label)
            if (is.null(colnames)) {
                Df
            } else {
                subset(Df, variable %in% colnames )
            }
        } else {
            if (is.null(colnames)) {
                for (i in seq_along(label))
                    cat(variablename[i], " : ", label[i], "\n")
            } else {
                  index <- match(colnames, variablename)
                   for (i in index)
                    cat(variablename[i], " : ", label[i], "\n")
            }
        }
    }


