# TSA_Project_2022June
This is the project detail in the course Time Series Analysis taught by Yingxing Li in Xiamen University.

---
title: "Comparison of Different Methods of Changepoint Detection"
author: "Bruce Sheng"
date: "`r Sys.Date()`"
output: html_document
---
# 多种方法检测变点结果比较
###  数据来源自(E. T. McGonigle and H. Cho, 2022)

## 创建数据
```{r}
x <- c(rep(0,200),rep(2,300),rep(4,200),rep(2,300))#确定分布均值
set.seed(123)
x = x + arima.sim(model = list(ar = 0.5), #增加扰动项
                  sd = sqrt(1-0.5^2), 
                  n = 1000)
databuffer <- as.vector(x)
```

## 方法一：FOCuS(Romano et al, 2023)
```{r}
library(FOCuS)

f <- function() {
  out <- databuffer[i]    
  i <<- i + 1
  out
}

ts_data <- x
plot(ts_data, type = 'l')
title('FOCuS(Romano et al, 2023)')

rest <- 1000
del = 0
cp <- c(0,200,500)
ptm = c(0,0,0,0,0)
for (del in cp) {
  rest = 1000 - del
  databuffer <- tail(databuffer,rest)
  i <- 1
  ptm <- as.numeric(system.time(res <- FOCuS(f, 18))) + ptm
  t <- res$t; changepoint <- res$changepoint
  changepoint = del+changepoint
  points(changepoint, ts_data[changepoint], col = 'red', pch = 19)
  text(changepoint, ts_data[changepoint], changepoint, pos = 4,col = 'red')
  points(del+t, ts_data[del+t], col = 'blue', pch = 19)
  text(del+t, ts_data[del+t], del+t, pos = 4, col = 'blue')
}
  
text(x = 0.5, y = 6, ptm[3], pos = 4)
```

## 方法二：WCM.GSA(H. Cho and Fryzlewicz,2021)
```{r}
source("E:/terms/2nd in 3rd/TSA/project/related/wcm.gsa-main/main.R")
ptm = c(0,0,0,0,0)
ptm <- as.numeric(system.time(res <-wcm.gsa(x)))

plot(x, type = 'l')
points(res$cp, x[res$cp], col = 'red', pch = 19)
text(res$cp, x[res$cp], res$cp, col = 'red', pos = 4)
title('WCM.GSA(H. Cho and Fryzlewicz,2021)')

text(x = 0.5, y = 6, ptm[3], pos = 4)
```

## 方法三：MOSUM.TAVC(E. T. McGonigle and H. Cho,2022)
```{r}

library(mosum)
source("E:/terms/2nd in 3rd/TSA/project/related/TAVC.seg-main/main.R")
ptm = c(0,0,0,0,0)
ptm <- as.numeric(system.time(x.m.c <- mosum.tavc(x,G = c(30,60,90,150), alpha = 0.05)))
x.m.c$cpts
plot(x, type = 'l')
points(x.m.c$cpts, x[x.m.c$cpts], col = 'red', pch = 19)
text(x.m.c$cpts, x[x.m.c$cpts], x.m.c$cpts, col = 'red', pos = 4)
title('MoSum.TAVC(E. T. McGonigle and H. Cho,2022)')

text(x = 0.5, y = 6, ptm[3], pos = 4)
```

##  方法四：WBS2(Fryzlewicz,2020)
```{r}
library(breakfast)
ptm = c(0,0,0,0,0)
ptm <- as.numeric(system.time(x.w.c <- wbs2.tavc(x, min.int.len = 60)))
x.w.c$cpts
plot(x, type = 'l')
points(x.w.c$cpts, x[x.w.c$cpts], col = 'red', pch = 19)
text(x.w.c$cpts, x[x.w.c$cpts], x.w.c$cpts, col = 'red', pos = 4)
title('WBS2(Fryzlewicz,2020)')
text(x = 0.5, y = 6, ptm[3], pos = 4)
```

## 方法五：CUSUM (PAGE,1954)
```{r}
library(cusumcharter)
library(ggplot2)
databuffer <- as.vector(x)

#检测第一个变点
test_vec <- databuffer
controls <- cusum_control(test_vec, target = 0)
cusum_control_plot(controls, 
                   xvar = obs, 
                   title_text = "CUSUM 1 (PAGE,1954)")+
  geom_vline(xintercept = 200, color = "gray", linetype = "dashed")+
  geom_text(aes(x = 200, y = 1000, label = "200"), size = 4, color = "gray")


#检测第二个变点
rest = 800
test_vec <- tail(databuffer,rest)
controls <- cusum_control(test_vec, target = 2)
cusum_control_plot(controls, 
                   xvar = obs, 
                   title_text = "CUSUM 2 (PAGE,1954)")+
  geom_vline(xintercept = 300, color = "gray", linetype = "dashed")+
  geom_text(aes(x = 300, y = 200, label = "300"), size = 4, color = "gray")

#检测第三个变点
rest = 500
test_vec <- tail(databuffer,rest)
controls <- cusum_control(test_vec, target = 4)

cusum_control_plot(controls, 
                   xvar = obs, 
                   title_text = "CUSUM 3 (PAGE,1954)")+
  geom_vline(xintercept = 200, color = "gray", linetype = "dashed")+
  geom_text(aes(x = 200, y = -400, label = "200"), size = 4, color = "gray")

```

## 方法六：MOSUM(Horv´ath et al., 2008 & Aue et al., 2008)
```{r}
library(mosum)
ptm = c(0,0,0,0,0)
ptm <- as.numeric(system.time(m <- mosum(x, G = 40)))
plot(x, type = 'l')
points(m$cpts, x[m$cpts], col = 'red', pch = 19)
text(m$cpts, x[m$cpts], m$cpts, col = 'red', pos = 4)
title('MOSUM(Horv´ath et al., 2008 & Aue et al., 2008)')
text(x = 0.5, y = 6, ptm[3], pos = 4)

plot(m)
```

#  方法七：PAGE-CUSUM(Fremdt, 2014)
```{r}
library(CPAT)
library(ggplot2)

time = 0
cpts = c()
databuffer <- as.vector(x)

for (rest in c(1000,800,500)) {
  test_vec <- tail(databuffer,rest)
  ptm <- proc.time()
  m <- CUSUM.test(test_vec,stat_plot = F)
  ptm <- proc.time() - ptm
  cpts = rbind(cpts,m$estimate+1000-rest)
  time = ptm[3] + time
}


plot(x, type = 'l')
points(cpts, x[cpts], col = 'red', pch = 19)
text(cpts, x[cpts], cpts, col = 'red', pos = 4)
title('PAGE-CUSUM(Fremdt, 2014)')
text(x = 0.5, y = 6, time, pos = 4)
```

#  方法八：mMOSUM(Chen and Tian, 2010)
```{r}
library(mosum)
ptm = c(0,0,0,0,0)
ptm <- as.numeric(system.time(m <- mosum(x,G = 0.1, alpha = 0.05)))
plot(x, type = 'l')
points(m$cpts, x[m$cpts], col = 'red', pch = 19)
text(m$cpts, x[m$cpts], m$cpts, col = 'red', pos = 4)
title('mMOSUM(Chen and Tian, 2010)')
text(x = 0.5, y = 6, ptm[3], pos = 4)

plot(m)
```



