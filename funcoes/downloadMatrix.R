# funcao downloadMatrix()
# função para fazer o download das matrizes de um estudo
# recebe gseList como parametro
# gseList eh uma lista contendo o link(s) para download e nome(s) do(s) arquivo(s)
# retorna uma string confirmando a conclusao do(s) download(s)
downloadMatrix <- function(gseList){ 
    # laço vai de 1 até o nivel final da lista recebida
    for (i in 1:length(gseList)){
        # executa o download do arquivo de acordo com o link (gseList[[i]][1])
        # passado e o salva com o nome escrito em gseList[[i]][2] 
        downloader::download(gseList[[i]][1],gseList[[i]][2])    
    }
    # retorna uma mensagem de sucesso quando termidado o download
    return("Todos os downloads foram concluídos")
}
