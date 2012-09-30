write.gdt <-
    function(data, filename) {
        ## export data into xml

    }


require(XML)
### un petit exemple
out <- xmlTree("gretldata", attrs = c(version = "1.2", name = "coucou",
                  frequency = "2", startobs = "1", endobs = "20", type = "cross-section"))
out$addNode("description", "c est la descriptio\nde la base")
out$addNode("variables", attrs = c(count = 10), close = FALSE)
out$addNode("variable", attrs = c(name = "ha", label = "he"))
out$addNode("variable", attrs = c(name = "hou", label = "hee"))
out$addNode("variable", attrs = c(name = "hii", label = "hi"))
out$closeTag()
out$addNode("observations", attrs = c(count = "10", labels = "false"), close = FALSE)
lapply(1:3, function(x) out$addNode("obs", x))
out$closeTag()
out$closeTag()
out$addTag()
class(out)
methods("XMLInternalDOM")
cat(saveXML(out$value()))

