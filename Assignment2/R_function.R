tmpFn1 <- function(xVec){
  return(xVec^(1:length(xVec)))
}

tmpFn2 <- function(xVec){
  return(xVec^(1:length(xVec))/(1:length(xVec)))
}

tmpFn3 <- function(x, n){
  return(1+sum(x^(1:n)/(1:n)))
}

tmpFn <- function(xVec){
  return((xVec[-c(length(xVec)-1,length(xVec))] + xVec[-c(1,length(xVec))] + xVec[-c(1,2)])/3)
}

tmpFn(c(1:5,6:1))

tmpFn <- function(x){
  if (x < 0){
    return (x^2+2*x+3)
  }
  else if (x<2){
    return (x+3)
  }
  else{
    return (x^2+4*x-7)
  }
}
x <- seq(-3,3)
plot(x, tmpFn(x), type="l")

tmpFn <- function(mat){
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  return(mat)
}

tmpFn <- function(n,k){
  tmp <- diag(k, nr = n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  return(tmp)
}

quadrant <- function(alpha){
  return(1 + (alpha%%360)%/%90)
}

weekday <- function(day,month,year){
  if ((month - 2) > 0){
    month <- month - 2
  }
  else{
    month <- month + 10
    year <- year - 1
  }
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  return(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7])
}

testLoop <- function(n){
  xVec <- rep(NA,n-1)
  for (i in 1:(n-1)){
    if (i == 1){
      xVec[i] = 1
    }
    else if (i == 2){
      xVec[i] == 2
    }
    else{
      xVec[i] = xVec[i-1] + (2 / xVec[i])
    }
  }
  return(xVec)
}

testLoop2 <- function(yVec){
  return(sum(exp(seq(along=yVec))))
}
testLoop2(1:3)

quadmap <- function(start, rho, niter){
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for (i in 1:(niter-1)){
    xVec[i+1] <- rho*xVec[i]*(1 - xVec[i])
  }
  return(xVec)
}

tmp <- quadmap(start=0.95, rho=2.99, niter=500)
plot(tmp, type="l")
plot(tmp[300:500], type="l")


quadmap2 <- function(start, rho){
  n <- 0
  pre <- start
  while(TRUE){
    now <- rho*pre*(1 - pre)
    n = n + 1
    if (abs(now - pre) < 0.02){
      break
    }
    pre <- now
  }
  return(n)
}
tmp2 <- quadmap2(start=0.95, rho=2.99)
tmp2

tmpFn <- function(xVec){
  new_xVec <- xVec - mean(xVec)
  d <- sum(new_xVec^2)
  n <- length(xVec)
  r1 <- sum(new_xVec[2:n] * new_xVec[1:(n-1)])/d
  r2 <- sum(new_xVec[3:n] * new_xVec[1:(n-2)])/d
  return(c(r1,r2))
}
tmpFn(seq(2,56,by=3))

tmpFn2 <- function(xVec, k){
  new_xVec <- xVec - mean(xVec)
  d <- sum(new_xVec^2)
  n <- length(xVec)
  temp <- function(j){
    sum(new_xVec[(j+1):n] * new_xVec[1:(n-j)] )/d
  }
  return(c(1, sapply(1:k, temp)))
}
