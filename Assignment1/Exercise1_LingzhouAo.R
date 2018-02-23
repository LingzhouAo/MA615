#Exercise 1
#1(a)
a <- c(1:20)
#1(b)
b <- c(20:1)
#1(c)
c <- c(1:20,19:1)
#1(d)
temp <- c(4,6,3)
#1(e)
temp_e <- rep(temp,times=10)
#1(f)
temp_f <- c(temp_e,4)
#1(g)
temp_g <- c(rep(temp_e[1],times=10), rep(temp_e[2],times=20), rep(temp_e[3],times=30))
#2
x <- seq(3, 6, by=0.1)
y <- exp(x)*cos(x)
#3(a)
a3 <-(0.1^seq(3,36, by=3))*(0.2^seq(1,34, by=3))
#3(b)
b3 <-(2^seq(1,25)/seq(1,25))
#4(a)
i <- seq(10,100)
a4 <- i^3+4*i^2
#4(b)
i <- seq(1,25)
b4 <- (2^i)/i+(3^i)/(i^2)
#5(a)
labels <- paste("label",1:30,sep=" ")
#5(b)
fns <- paste("fn",1:30,sep="")
#6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
#6a
a6 <- yVec[-1]- xVec[-length(xVec)]
#6b
b6 <- sin(yVec[-length(yVec)])/cos(xVec[-1])
#6c
c6 <- xVec[-c(249,250)] + 2*xVec[-c(1,250)] - xVec[-c(1,2)]
#6d
d6 <- sum(exp(-xVec[-1])/(xVec[-length(xVec)] + 10))
#7a
a7 <- yVec[yVec>600]
#7b
b7 <- (1:length(yVec))[yVec > 600]
#7c
c7 <- xVec[yVec>600]
#7d
d7 <- sqrt(abs(xVec - mean(xVec)))
#7e
e7 <- sum(yVec > max(yVec) - 200)
#7f
f7 <- sum(xVec%%2 == 0)
#7g
g7 <-  xVec[order(yVec)]
#7h
h7 <- yVec[c(T,F,F)]
#8
ans <- 1 + sum(cumprod(seq(2, 38, by = 2)/seq(3, 39, by = 2)))
