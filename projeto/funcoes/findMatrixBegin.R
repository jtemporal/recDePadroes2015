findMatrixBegin <- function(mat){
    x = readLines(con = mat)
    lineNum = grep("series_matrix_table_begin",x)
    return(lineNum)
}
