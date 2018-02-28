A <- matrix(c(1,1,3,5,2,6,-2,-1,-3), nrow = 3, byrow = TRUE)
A
A^3
A[,3] <- A[,2] + A[,3]
A
B <- matrix(c(10,-10,10),nrow = 15, ncol = 3 ,byrow=TRUE)
B
crossprod(B,B)
matE <- matrix(rep(0,36), nrow = 6, byrow = TRUE)
row(matE)
col(matE)
row(matE)-col(matE)
matE[abs(row(matE)-col(matE))==1] <- 1
matE
a <- 0:4
A <- outer(a,a,"+")
A
B <- outer(a,a, "*")
B
b <- 5:10
C <- outer(a,b,"+")
C
D <- outer(b,a, "%%")
D

a <- 0:4
A <- outer(a,a,"+")%%5
A
b <- 0:9
B <- outer(b,b,"+")%%10
B
c <- 0:8
C <- outer(c, c, "-")%%9
C

A <- matrix(c(1,2,3,4,5,2,1,2,3,4,3,2,1,2,3,4,3,2,1,2,5,4,3,2,1),nrow = 5, ncol = 5,byrow=TRUE)
A
y <- matrix(c(7,-1,-3,5,17),nrow = 5, ncol = 1, byrow=TRUE)
y
x <- solve(A) %*% y
x

set.seed(75)
aMat <- matrix(sample(10, size=60, replace=TRUE), nr=6)
aMat
apply(aMat, 1, function(x){sum(x > 4)})

apply(aMat, 1, function(x){sum(x>6 & x <8)} == 2)

cSums <- colSums(aMat)
which( outer(cSums, cSums, "+") > 75, arr.ind = TRUE)

sum((1:20)^4) * sum(1/(3+(1:5)))
