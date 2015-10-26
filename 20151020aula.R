# aula do dia 20/10/2015
# supondo uma urna com bolas coloridas em quantidades infinitas (mesma coisa q
# reitirar com reposição, a probabilidade de retirar aquela cor se mantem 
# nao importando a quantidade) as cores indetificam coisas diferentes na minha
# urna.

# dados que caíram do ceu

k = 38
n = 100
p = seq(0, 1, by = 0.01)

# sem o 1/D, calcula o valor dos pontos baseado nos valores de k e n caídos do ceu
calculaProb <- function(p, n, k){
    res  = (p^k) * ((1 - p)^(n-k))
    return(res)
}

# cria o vetor com os valores de prob 
prob = calculaProb(p, n, k)

# primeiro grafico
# pdf("plot1.pdf")
plot(prob, type = 'b', col = "red", pch = 19)
#dev.off()

# das aulas anteriores, calcula a integral
integral = function(x,y){
    sum((y[-1]+y[-length(y)])/2 * diff(x))
}
# como a gente "jogou pra fora" o 1/D precisamos normalizar os valores entao,
# armazenamos o valor da integral para isso. 
d = integral(p, prob)
# [1] 1.746191e-30

normalizaUsandoD <- function(p, d){
    for( i in seq_along(p)){
        p[i] = p[i]/d} 
    return(p)
}

# normalizando e atualizando os valores usando D
prob = normalizaUsandoD(prob, d)

# recalcula a integral para checar se deu certo, se der 1 ok senão f*deu
integral(p, prob)
#[1] 1
# deu certo uhuuulll

# plotemos:
# pdf("20151020aulaSegundoPlot.pdf")
plot(prob, type = 'b', col = "red", pch = 19, ylim=c(0, 30)) #repare na mudanca da escala

# agora fazendo pra mesma proporcao porem numa amostra maior:
K = 380
N = 1000
# p nao muda
PROB = calculaProb(p, N, K)
D = integral(p, PROB)
PROB = normalizaUsandoD(PROB,D)
integral(p, PROB)
#[1] 1 uhuulll

# plotando os pontos da nova dist no mesmo gráfico anterior
points(PROB, col="blue", pch=19, type='b')
# dev.off()
