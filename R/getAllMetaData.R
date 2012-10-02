##' Return some metadata from a gdt file
##'
##' @param file
##' @return a list of list which contains all relevant metadata
##' @author Ahmadou Dicko <dicko.ahmadou at gmail.com>
getAllMetaData <-
    function(file) {
        doc <- xmlRoot(xmlInternalTreeParse(file))
        ## metadata : gretldata node
        metadata <- getNodeSet(doc, "//gretldata")
        version <- sapply(metadata, xmlGetAttr, "version")[[1]]
        name <- sapply(metadata, xmlGetAttr, "name")
        frequency <- sapply(metadata, xmlGetAttr, "frequency")
        startobs <- sapply(metadata, xmlGetAttr, "startobs")
        endobs <- sapply(metadata, xmlGetAttr, "endobs")
        type <- sapply(metadata, xmlGetAttr, "type")

        ## variables node
        variables <- getNodeSet(doc, "/gretldata//variables")
        variablecount <- sapply(variables, xmlGetAttr, "count")

        variablelabel <- getNodeSet(doc, "/gretldata//variables/variable")
        variablename <- sapply(variablelabel, xmlGetAttr, "name")
        variablelabel <-  sapply(variablelabel, xmlGetAttr, "label")

        ## observations metadata
        observations <- getNodeSet(doc, "/gretldata//observations")
        observationcount <- sapply(observations, xmlGetAttr, "count")
        observationlabel <- sapply(observations, xmlGetAttr, "labels")

        meta <- list(gretldata = list(version = version,
                     name = name,
                     frequency = frequency,
                     startobs = startobs,
                     endobs = endobs,
                     type = type),
                     variablesmeta = list(count = variablecount),
                     variable = list(name = variablename,
                     label = variablelabel),
                     observations = list(count = observationcount,
                     labels = observationlabel
                     ))
        index <- sapply(meta, function(y) sapply(y, function(x) !is.null(x)))
        for (i in seq_len(length(meta))) meta[[i]] <- meta[[i]][index[[i]]]
        meta
    }
