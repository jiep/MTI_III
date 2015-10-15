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
plot(density(samples), xaxt='n', ann=FALSE, yaxt='n', xlab=, ylab=, main = ,xlim = c(0,1), ylim = c(0,6), col=2)
par(new = TRUE)
curve(dbeta(x,alpha, beta), ann=FALSE, yaxt='n', xlab=, ylab=, xlim = c(0,1), ylim = c(0,6), col=4)
par(new = TRUE)
curve(dbeta(x,alpha_post, beta_post), xlim = c(0,1), ylim = c(0,6), col=6)
par(new = FALSE)
legend(x= "topleft", y=0.2,legend = c("Empírica", "A priori", "A posteriori"), col = c(2,4,6),  lty=1:2, cex=0.8,
  box.lty=0)

# Calculamos la media y desviación típica a posteriori
post.mean <- (alpha + x)/(alpha + beta + n)
post.var <- ((x + alpha)*(n - x + beta))/((alpha + beta  + n)^2*(alpha + beta + n + 1))
post.sd <- sqrt(post.var)

# Calculamos un intervalo de probabilidad al 90% para p

percentage <- 0.9
alpha_level <- (1- percentage)/2

lcb <- qbeta(alpha_level, alpha_post, beta_post)
ucb <- qbeta(1 - alpha_level, alpha_post, beta_post)

# Hacemos el contraste de hipótesis 
# H_0 : p <= 0.4
# H_1 : p > 0.4

x0 <- pbeta(0.4, alpha_post, beta_post) 
x1 <- 1 - pbeta(0.4, alpha_post, beta_post) 

# Calculamos la probabilidad predictiva de que al menos 9 de los 10 niños terminen 
# la Educación Secundaria

k <- 9 # 9 niños acaban la Educación Secundaria
m <-10 # de 10  
probability <- 1 - choose(m, k) * (gamma(alpha + beta + n)/(gamma(alpha + x)*gamma(beta + n - x)))*(gamma(alpha + x + k)*gamma(beta + n - x + m - k)/gamma(alpha + beta + m + n))  


#------------------------------------------------------------------------------
# Tarea IV
#------------------------------------------------------------------------------