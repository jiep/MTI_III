#------------------------------------------------------------------------------
# Tarea III
#------------------------------------------------------------------------------

# Definimos algunas variables de interés
n <- 29 # Número de niños estudiados
x <- 22 # Número de niños que terminaron la Educación Secundaria

# Parámetros alfa y beta de la distribuación beta a priori: Be(1,1)
alpha <- 1
beta <- 1

# Parámetros alfa y beta de la distribución beta a posteriori: Be(23,8)

alpha_post <- x + alpha
beta_post <- n - x + beta 

# Generamos una muestra de tamaño 1000 para la distribución empírica
samples <- rbeta(1000, alpha_post, beta_post)

# Representamos la densidad empírica, la densidad a priori y la densidad a 
# posteriori
plot(density(samples), xaxt='n', ann=FALSE, yaxt='n', xlab=, ylab=, 
  main = ,xlim = c(0,1), ylim = c(0,6), col=2)
par(new = TRUE)
curve(dbeta(x,alpha, beta), ann=FALSE, yaxt='n', xlab=, ylab=, 
  xlim = c(0,1), ylim = c(0,6), col=4)
par(new = TRUE)
curve(dbeta(x,alpha_post, beta_post), xlim = c(0,1), ylim = c(0,6), col=6, 
  xlab = expression(pi), ylab = "Densidad", 
  main="Función de densidad de la distribución empírica, a priori 
    y a posteriori")
par(new = FALSE)
legend(x= "topleft", y=0.2,legend = c("Empírica", "A priori", "A posteriori"), 
  col = c(2,4,6),  lty=1, cex=0.8, box.lty=0)

# Calculamos la media y desviación típica a posteriori
post.mean <- (alpha + x)/(alpha + beta + n)
post.var <- ((x + alpha)*(n - x + beta))
  /((alpha + beta  + n)^2*(alpha + beta + n + 1))
post.sd <- sqrt(post.var)

# Calculamos un intervalo de probabilidad al 90% para p

percentage <- 0.9
alpha_level <- (1- percentage)/2

lcb <- qbeta(alpha_level, alpha_post, beta_post)
ucb <- qbeta(1 - alpha_level, alpha_post, beta_post)

# Representamos el intervalo de probabilidad junto con la distribución 
# a posteriori
curve(dbeta(x,alpha_post, beta_post), xlim = c(0,1), ylim = c(0,6), 
  col=6, xlab = expression(pi), ylab = "Densidad", 
  main="Intervalo de probabilidad al 90 % sobre la distribución 
    a posteriori")

cords2.x <- c(lcb, seq(lcb, ucb, 0.01), ucb)
cords2.y <- c(0, dbeta(seq(lcb, ucb, 0.01), alpha_post, beta_post), 0)
polygon(cords2.x,cords2.y,col='skyblue', border=NA)
abline(h=0)

# Hacemos el contraste de hipótesis 
# H_0 : p <= 0.4
# H_1 : p > 0.4

x0 <- pbeta(0.4, alpha_post, beta_post) 
x1 <- 1 - pbeta(0.4, alpha_post, beta_post) 

# Calculamos la probabilidad predictiva de que al menos 9 de los 10 niños 
# terminen la Educación Secundaria

k <- 9 # 9 niños acaban la Educación Secundaria
m <-10 # de 10
sum <- 0
x <- seq(0,m)
y <- c(rep(0, m+1))
for(k in 0:m){
  probability <- choose(m, k) * (gamma(alpha + beta + n)/
    (gamma(alpha + x)*gamma(beta + n - x)))*(gamma(alpha + x + k)*
    gamma(beta + n - x + m - k)/gamma(alpha + beta + m + n))  
  y[k] = probability
}

# Representamos la función de densidad de la distribución predictiva
plot(x,y, main = "Función de densidad de la distribución predictiva", 
  xlab = "Número de éxitos de 10 posibles", ylab = "Densidad", col = 2, 
  type = "o")

#------------------------------------------------------------------------------
# Tarea IV
#------------------------------------------------------------------------------

# Importamos la librería LearnBayes
library("LearnBayes")

# Definimos una serie de variables que nos serán de utilidad
mu <- seq(20, 70,10)
g_mu <- c(0.1, 0.15, 0.25, 0.25, 0.15, 0.1)
y <- c(38.6, 42.4, 57.5, 40.5, 51.7, 67.1, 33.4, 60.9, 64.1, 40.1, 40.7, 6.4)

sigma <- 10

# Representamos la función de densidad de la distribución a priori
plot(mu, g_mu, col = 2, main = "Función de densidad de la distribución a priori", 
  xlab = "Total anual de precipitaciones en forma de nieve (en cm)", 
  ylab = "Densidad", type = "h")

# Calculamos la verosimiltud para cada uno de los valores de mu

# Definimos una función para la verosimilitud
likelihood <- function(mu){
  n <- length(y)
  temp <- 1/(sqrt(2*pi)*sigma)^n
  temp2 <- prod(exp((-1/2) * ((y-mu)/sigma)^2))
  return(temp*temp2)
}

likelihood2 <- function(mu, y, sigma){
  n <- length(y)
  temp <- 1/(sqrt(2*pi)*sigma)^n
  temp2 <- exp(-1/(2*sigma^2)*(n*mu^2 + sum(y^2) - 2*mu*sum(y)))
  return(temp*temp2)
}


likelihood <- likelihood2(mu,y,sigma)

# Calculamos la distribución a posteriori

post <- likelihood*g_mu # Sin normalizar

post <- post/sum(post) # Normalizada

# Representamos la distribución a posteriori
plot(mu, post, main="Distribución a posteriori", xlab=expression(mu), 
  ylab="Densidad", type = "p", col = 2)
par(new = TRUE)
plot(mu, post, main="Distribución a posteriori", xlab=expression(mu), 
  ylab="Densidad", type = "h", col = 2)

# Calculamos el intervalo de probabilidad al 80%

percentage <- 0.8
dist <- cbind(mu, post)
discint(dist,percentage)

# Ahora tenemos como distribución a priori, una distribución N(60, 5).

mu0 <- 60
sigma0 <- 5
n <- length(y)

# La distribución a posteriori tiene como parámetros 

mu_post <- (mu0/sigma0^2 + n*mean(y)/sigma^2)/(1/sigma0^2 + n/sigma^2)
var_post <- (1/sigma0^2 + n/sigma^2)^-1
sd_post <- sqrt(var_post)

# Representamos las distribuciones a priori y posteriori
curve(xlim=c(30,90),dnorm(x,mu0, sigma0), ann=FALSE, yaxt='n', xlab=, 
  ylab=, ylim = c(0,0.18), col=4)
par(new = TRUE)
curve(xlim=c(30,90),dnorm(x,mu_post, sd_post), ylim = c(0,0.18), col=6, 
  xlab = expression(mu), ylab = "Densidad", main="Función de densidad de 
    la distribución a priori y a posteriori")
par(new = FALSE)
legend(x= "topleft", y=0.2,legend = c("A priori", "A posteriori"), 
  col = c(4,6),  lty=1, cex=0.8, box.lty=0)


# Calculamos un intervalo de probabilidad al 80% para mu

percentage <- 0.8
alpha_level <- (1 - percentage)/2

lcb <- qnorm(alpha_level, mu_post, sd_post)
ucb <- qnorm(1 - alpha_level, mu_post, sd_post)

# Representamos el intervalo de probabilidad junto con la 
# distribución a posteriori
curve(dnorm(x,mu_post, sd_post), xlim = c(30,90), ylim = c(0,0.18), 
  col=6, xlab = expression(mu), ylab = "Densidad", 
  main="Intervalo de probabilidad al 80 % sobre la distribución a posteriori")

cords2.x <- c(lcb, seq(lcb, ucb, 0.01), ucb)
cords2.y <- c(0, dnorm(seq(lcb, ucb, 0.01), mu_post, sd_post), 0)
polygon(cords2.x,cords2.y,col='skyblue', border=NA)
abline(h=0)
