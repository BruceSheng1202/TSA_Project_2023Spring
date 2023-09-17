set.seed(123)
x <- c(rnorm(2e4, 1), rnorm(3e4,2),rnorm(2e4, 4), rnorm(2e4,2))#确定分布均值
databuffer <- as.vector(x)



library(FOCuS)

f <- function() {
  out <- databuffer[i]    
  i <<- i + 1
  out
}

ts=c()
changepoints = c()
runtime = c()
k=1
rest <- 1e5
del = 0
cp <- c(0,2e4,5e4)
for (del in cp) {
  rest = 1e5 - del
  databuffer <- tail(databuffer,rest)
  i <- 1
  ptm = c(0,0,0,0,0)
  ptm <- as.numeric(system.time(res <- FOCuS(f, 28)))
  t <- res$t; changepoint <- res$changepoint
  changepoint=del+changepoint
  t=del+t
  changepoints <- append(changepoints,changepoint)
  ts <- append(ts,t)
  runtime <- append(runtime,ptm[3])
}

sum(runtime)
changepoints
ts




source("C:/Users/ASUS/Desktop/Test for TSA/related/wcm.gsa-main/main.R")
ptm = c(0,0,0,0,0)
ptm <- as.numeric(system.time(res <- wcm.gsa(x)))
ptm[3]
res$cp



library(breakfast)
library(mosum)
source("C:/Users/ASUS/Desktop/Test for TSA/related/TAVC.seg-main/main.R")
ptm = c(0,0,0,0,0)
ptm <- proc.time()
x.m.c = mosum.tavc(x,G = c(30,60,90,150), alpha = 0.05)
ptm <- proc.time() - ptm
ptm[3]
x.m.c$cpts


library(breakfast)
#ptm = c(0,0,0,0,0)
#ptm <- proc.time()
x.w.c = wbs2.tavc(x, min.int.len = 60)
#ptm <- proc.time() - ptm
#ptm[3]
x.w.c$cpts


ptm <- proc.time()
library(mosum)
x.m.c = mosum(x,G = 0.1, alpha = 0.05)
x.m.c$cpts
#plot(x, type = 'l')
#points(x.m.c$cpts, x[x.m.c$cpts], col = 'red', pch = 19)
#text(x.m.c$cpts, x[x.m.c$cpts], x.m.c$cpts, col = 'red', pos = 4)
#title('mMoSum')

ptm <- proc.time() - ptm
ptm
print(round(ptm[3],4))
x.m.c$cpts

