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
        # faz as manipulacoes de string para obter os gsm de cada amostra
        gsm <- strsplit(x=x[gsmIndex], split='">')
        # anota o tipo de paciente de quem amostra foi colhida
        patient <- strsplit(x=x[gsmIndex + 1], split='">')
        # manipulacao de listas e strings para manter apenas as informações necessarias
        for (k in seq_along(gsmIndex)){
            patient[[k]] <- patient[[k]][2]
            patient[[k]] <- substr(patient[[k]], start=1, stop=nchar(patient[[k]])-5)
            gsm[[k]] <- substr(gsm[[k]][length(gsm[[k]])], start=1, stop=10)
        }
        # tornado gsm e patient em vetores p/ serem adicionaddos na lista final
        gsm <- unlist(gsm)
        patient <- unlist(patient)
        l[[j]] <- cbind(gsm,patient)
    }
    # renomeando os niveis da lista para corresponder ao estudo
    names(l) <- gse 
    return(l)
}
