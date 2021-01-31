library(pso)
library(scatterplot3d)

set.seed(1)
psoptim(c(NA),function(x) x^2-3,lower=-5,upper=5,control=list(abstol=1e-8))

set.seed(1)
## Rastrigin function
psoptim(rep(NA,2),function(x) 20+sum(x^2-10*cos(2*pi*x)),
        lower=-5,upper=5,control=list(abstol=1e-8))
set.seed(1)
## Rastrigin function - local refinement with L-BFGS-B on improvements
psoptim(rep(NA,2),function(x) 20+sum(x^2-10*cos(2*pi*x)),
        lower=-5,upper=5,control=list(abstol=1e-8,hybrid="improved"))
## Griewank function
psoptim(rep(NA,2),function(x) sum(x*x)/4000-prod(cos(x/sqrt(1:2)))+1,
        lower=-100,upper=100,control=list(abstol=1e-2))
set.seed(1)
## Rastrigin function with reporting
o <- psoptim(rep(NA,2),function(x) 20+sum(x^2-10*cos(2*pi*x)),
             lower=-5,upper=5,control=list(abstol=1e-8,trace=1,REPORT=1,
                                           trace.stats=TRUE))
## Not run:
plot(o$stats$it,o$stats$error,log="y",xlab="It",ylab="Error")
points(o$stats$it,sapply(o$stats$f,min),col="blue",pch=2)



z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3d(x, y, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "Helix", pch = 20)




x1 <- seq(-5, 5, 0.01)
x2 <- seq(-5, 5, 0.01)
z = c()
for (i in 1:length(x1))
{
  zz = c(x1[i],x2[i])
  z[i] = 20+sum(zz^2-10*cos(2*pi*zz))
}
scatterplot3d(x1, x2, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue",  pch = 20)





x1 <- seq(-100, 100, 0.1)
x2 <- seq(-100, 100, 0.1)
z = c()
for (i in 1:length(x1))
{
  zz = c(x1[i],x2[i])
  z[i] = sum(zz*zz)/4000-prod(cos(zz/sqrt(1:2)))+1
}
scatterplot3d(x1, x2, z, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue",  pch = 20)

