# Função findMatrixBegin
# esta função encontrará no arquivo de dados baixado do tipo series_matrix
# onde começa a matriz de dados para analise e retornar a linha anterior
# recebe mat como parametro
# mat eh o arquivo baixado pela funçao downloaMatrix
# retorna um inteiro correspondente a linha anterior à linha com os nomes
# das colunas da matriz
findMatrixBegin <- function(mat){
    # le todas linhas dentro do arq armazenando-as cada uma em um indice de x
    x <- readLines(con = mat)
    # procura pela linha que contem a string de começo da matriz
    lineNum <- grep("series_matrix_table_begin",x)
    # retorna o numero da linha encontrada
    return(lineNum)
}
