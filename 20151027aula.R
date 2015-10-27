# aula do dia 27/10/2015
# continuando a ideia vista na aula do dia 20/10/2015 
# queremos definir o intervalo de p que tras o valor dado

# reaproveitando da outra aula
k = 38
n = 100
p = seq(0, 1, by = 0.001)

# sem o 1/D, calcula o valor dos pontos baseado nos valores de k e n caídos do ceu
calculaProb <- function(p, n, k){
    res  = (p^k) * ((1 - p)^(n-k))
    return(res)
}

# cria o vetor com os valores de prob 
prob = calculaProb(p, n, k)

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

# conferindo a integral deve dar 1
integral(p, prob)
#[1] 1

# visualizando
plot(y = prob, x = p,  col="red", type='b')

# Para hoje
# 1) intervalo de credibilidade: intervalo que mostra aquele valor de p
# 2) calcular a probabilidade das coisas

# 2)
integraPraMimNoIntervalo <- function(a,b, k=38, n=100){

	p = seq(0, 1, by = 0.01)
	# fazendo prob
	prob = calculaProb(p, n, k)
	# normalizando
	d = integral(p, prob)
	prob = normalizaUsandoD(prob, d)

	for(i in seq_along(p)){
		if (p[i] == a){
			primeiro = i
		}
		if (p[i] == b){
			segundo = i
		}
	}
	novoP = p[primeiro:segundo]
	novoProb = prob[primeiro:segundo]
	res = integral(novoP, novoProb)	
	return(res)
}

integraPraMimNoIntervalo(0.44, 0.59)
# check
integraPraMimNoIntervalo(0, 1)
#[1] 1

# 1)
descobreOintervaloAih <- function(P){

	p = seq(0, 1, by = 0.01)
	# fazendo prob
	prob = calculaProb(p, n, k)
	# normalizando
	d = integral(p, prob)
	prob = normalizaUsandoD(prob, d)

	index = match(max(prob), prob)
	
	m = max(prob)/2
	
	b = which.min(abs(m-prob))
	

	intervalo = c(,)
	return(intervalo)
}

# linha horizontal
abline(v = p[439], col='black')

# linha vertical
abline(h=prob[45], col='blue')
