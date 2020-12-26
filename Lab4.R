# Задано
a = 0
b = 1.2
p = 3
To = 1.1

fi = function(x, t){
  return(log(1 + 1/(x+2))*exp(-t))
}

ksi = function(x){
  return(cos(x))
}

Ua = function(t){
  return(1/(1+t))
}

Ub = function(t){
  return(cos(1.2 + t))
}


# Поиск U
# Задаем h и tau
findU = function(q, M){
h = (b-a)/M
x = seq(from = a, to = b, length = M)


tau = h*h*q/p
N = trunc(To/tau) #Граница n
# Зададим xm = mh + a и tn = n*tau
t = seq(from = 0, to = To, by = tau)

# Находим решение

#Прогонка

an = -q
bn = 1 + 2*q
cn = -q


U = matrix(nrow = N, ncol = M) ## M - x, N - t
for(m in 1: M)
  U[1, m] = ksi(x[m])

for(n in 1: N)
  U[n, 1] = Ua(t[n])

for(n in 1: N)
  U[n, M] = Ub(t[n])

alpha = matrix(nrow = N,  ncol = M - 1)
beta = matrix(nrow = N, ncol = M - 1)
for(i in 1: (N - 1)){
  alpha[i, 1] = 0;
  beta[i, 1] = Ua(t[i])
}

for(i in 1:(N -1)){
  for(j in 2: (M - 1)){
    alpha[i, j] = -cn/(an*alpha[i, j - 1] + bn)
    beta[i, j] = (fi(x[j],t[i]) * tau - an * beta[i, j-1] + U[i, j])/(an*alpha[i, j -1] + bn)
  }
  for(k in (M-1) : 2){
    U[i+1,k] = alpha[i,k]*U[i+1, k+1] + beta[i,k]
  }
}
return(U)
}

M1 = 25
h1 = (b-a)/M1
x1 = seq(from = a, to = b, length = M1)
U1 = findU(1.8, M1)
tlen1 = length(U1[,1])
n1 = tlen1/5
n2 = 3*tlen1/5
plot1 <- plot(x1, U1[1,], pch = 15, col = "red", ylab = "t", ylim = c(-2, 3), cex = 1.5)
plot1 <-points(x1, U1[n1,], pch = 15, col = "blue", cex = 1.5)
plot1 <-points(x1, U1[n2,], pch = 15, col = "forestgreen", cex = 1.5)
plot1 <-points(x1, U1[tlen1,], pch = 15, col = "violet", cex = 1.5)


M2 = 100
h2 = (b-a)/M2
x2 = seq(from = a, to = b, length = M2)
U2 = findU(1.8, M2)
tlen2 = length(U2[,1])
n3 = tlen2/5
n4 = 3*tlen2/5
plot1 <-points(x2, U2[1,], pch = 1, col = adjustcolor("black", 0.5), cex = 2)
plot1 <-points(x2, U2[n3,], pch = 1, col = adjustcolor("black", 0.5), cex = 2)
plot1 <-points(x2, U2[n4,], pch = 1, col = adjustcolor("black", 0.5), cex = 2)
plot1 <-points(x2, U2[tlen2,],pch = 1, col = adjustcolor("black", 0.5), cex = 2)

