# Functions

# Função getLinkMatrix
getLinkMatrix <- function(gse){
    l <- list("null")
    for (i in seq_along(gse)){
        url <- paste0( "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gse[i])
    	parseURL <- XML::htmlParse(url)
    	links <- XML::xpathSApply(doc, "//a/@href")
    	link <- links[grep("matrix/", links)]
    	mat <- RCurl::getURL(link)
    	mat <- unlist(strsplit(mat, " "))[length(mat)]
    	mat <- unlist(strsplit(x, "\n"))
    	link <- paste0(link,mat)
    	aux <- c(link,mat)
    	l[[i]] <- aux
    }
	return(l)
}

# Função 
