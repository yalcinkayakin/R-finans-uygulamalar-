
V0 <- 100 ; nu <- 0.1 ; sigma <-0.2 
dt <- 1 /252 ; Time <-1
M <- Time /dt
n <-10000
set.seed(117)
val <-rnorm(n*M, mean = (nu -sigma^2 / 2)* dt, sd =sigma * dt^0.5)
dlnV <-matrix(val,M,n)
V <- V0 * exp(apply(dlnV, 2, cumsum))
matplot(x =seq(0+dt,Time,dt),y=V[,1:5],type = 's',lty = 1, 
        xlab ="Time",ylab = "Firm Value", main="in the Merton model")

r <- 0.05 ; K <- 80
D <- exp(-r * Time) * mean((pmin(V[M, ], K)))
D
