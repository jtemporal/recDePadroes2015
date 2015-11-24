# Função doMeta
# essa função fará as matrizes de  metadados de cada estudo
# recebe gse
# gse eh um vetor de strings onde cada string eh o geo ID do estudo
# retorna l
# uma lista com as matrizes de metadados de cada estudo
doMeta <- function(gse){
    l <- list("null")
    for (j in seq_along(gse)){
	# le as linhas da pagina referente a cada estudo
        x <- readLines(con = paste0("http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", gse[j]))
        # anota as linhas que possuem o termo GSM da pagina 
        gsmIndex <- grep("GSM", x)
        gsm <- strsplit(x=x[gsmIndex], split='">')

        patientIndex <- gsmIndex + 1
        patient <- strsplit(x=x[patientIndex], split='">')

        
	    for (k in seq_along(gsmIndex)){
                patient[[k]] <- patient[[k]][2]
                patient[[k]] <- substr(patient[[k]], start=1, stop=nchar(patient[[k]])-5)
                
                gsm[[k]] <- substr(gsm[[k]][length(gsm[[k]])], start=1, stop=10)
            }
        
    gsm <- unlist(gsm)
    patient <- unlist(patient)
    l[[j]] <- cbind(gsm,patient)
    }
    names(l) <- gse 
    return(l)
}
