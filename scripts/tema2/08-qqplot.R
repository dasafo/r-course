##-----Gráficos Quantil Quantil (qqnorm y qqplot)-----

s <- seq(0.01, 0.99, 0.01)
s
qn <- qnorm(s) #qnorm() distribución normal de los quantiles(si no le indicamos lo contrairo usa la gaussiana)
qn
df <- data.frame(p = s, q = qn) #p de percentiles

#Generamos 200 muestras sobre la normal con rnorm(), r de random
sample <- rnorm(200)
sample
quantile(sample, probs = s) #nos muestra los cuantiles con probabilidades dada por la secuencia s


#qqnorm
trees #data set que viene ya en R
par(mfrow=c(1,1))
qqnorm(trees$Height)

qqnorm(randu$x)

#qqplot
randu #dataset de R
n <- length(randu$x) #también se podría con la y o la z
n
y <- qunif(ppoints(n)) #creamos una distribución uniforme de n puntos
y
qqplot(y, randu$x)



chi3 <- qchisq(ppoints(30), df = 3)#quantiles Chi square(qchsq())
n30  <- qnorm(ppoints(30))
qqplot(n30, chi3)

qqplot(chi3, chi3)

cauchy <- qcauchy(ppoints(30))#quantiles de cauchy
qqplot(n30, cauchy)


par(mfrow=c(1,2))
x <- seq(-3, 3, 0.01)
plot(x, dnorm(x))
plot(x, pnorm(x))

plot(x, dchisq(x, df=3))
plot(x, pchisq(x, df=3))

plot(x, dcauchy(x))
plot(x, pcauchy(x))
