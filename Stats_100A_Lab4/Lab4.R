library(prob)
S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = max, invars = c("X1", "X2"), name = "U")
S <- addrv(S, FUN = sum, invars = c("X1", "X2"), name = "V")
head(S)

UV <- marginal(S, vars = c("U", "V"))
head(UV)
xtabs(round(probs, 3) ~ U + V, data = UV)

marginal(UV, vars = "U")
head(marginal(UV, vars = "V"))
temp <- xtabs(probs ~ U + V, data = UV)
rowSums(temp)
colSums(temp)

Eu <- sum(S$U * S$probs)
Ev <- sum(S$V * S$probs)
Euv <- sum(S$U * S$V * S$probs)
Euv - Eu * Ev

library(mvtnorm)
x <- y <- seq(from = -3, to = 3, length.out = 30)
f <- function(x, y) dmvnorm(cbind(x, y), mean = c(0, 0), sigma = diag(2))
z <- outer(x, y, FUN = f)
persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")

library(combinat)
tmp <- t(xsimplex(3, 6))
p <- apply(tmp, MARGIN = 1, FUN = dmultinom, prob = c(36, 27, 37))
library(prob)
S <- probspace(tmp, probs = p)
ProbTable <- xtabs(probs ~ X1 + X2, data = S)
round(ProbTable, 3)

library(scatterplot3d)
X <- t(as.matrix(expand.grid(0:6, 0:6)))
X <- X[, colSums(X) <= 6]
X <- rbind(X, 6 - colSums(X))
Z <- round(apply(X, 2, function(x) dmultinom(x, prob = 1:3)), 3)
A <- data.frame(x = X[1, ], y = X[2, ], probability = Z)
scatterplot3d(A, type = "h", lwd = 3, box = FALSE)

# TRY THIS: Joint probability distribution of a rat-drug experiment
pXY = c(0.388, 0.485, 0.09, 0.009, 0.01, 0.008, 0.003, 0.005, 0.002)
# The vector of joint probabilities from a rat-drug example
pXYm = matrix(pXY, 3, 3) # the matrix of joint probabilities
px = rowSums(pXYm)
py = colSums(pXYm) # the marginal PMFs
diag(1/px) %*% pXYm # the matrix pYjX=x (y)x;y
pXYm[1, ]/px[1] # the first row of pYjX=x (y)x;y
pXYm %*% diag(1/py) # the matrix pXjY=y (x)x;y
pXYm[, 1]/py[1] # the first column of pXjY=y (x)x;y

#Binomial Joint Mass Function:
#For X Bin(2; 0:5), Y Bin(3; 0:5) independent, do:
pXYm = dbinom(0:2, 2, 0.5) %*% t(dbinom(0:3, 3, 0.5))

P = expand.grid(px = dbinom(0:2, 2, 0.5), py = dbinom(0:3, 3, 0.5))
P$pxy = P$px * P$py
pXYm = matrix(P$pxy, 3, 4)

P$pxy
pXY=as.vector(pXYm)
P$pxy #same as pXY=as.vector(pXYm)
print("P$pxy same as pXY=as.vector(pXYm)")

px = rowSums(pXYm)
py = colSums(pXYm) # same as dbinom(0:2,2,0.5) and dbinom(0:3,3,0.5), respectively.
px
dbinom(0:2,2,0.5)
py
dbinom(0:3,3,0.5)

print("px is same as dbinom(0:2,2,0.5), py is same as dbinom(0:3,3,0.5)")

R = expand.grid(px = dbinom(0:3, 3, 0.3), py = dbinom(0:4, 4, 0.6))
R$pxy = R$px * P$py
G = expand.grid(X = 0:3, Y = 0:4)
R$X = G$X
R$Y = G$Y
attach(R)
sum(R$pxy[which(R$X + R$Y == 4)]) # pmf of X + Y at x = 4
sum(R$pxy[which(R$X + R$Y <= 4)]) # cdf of R$X + R$Y at x = 4
sum(R$pxy[which(3 < R$X + R$Y & R$X + R$Y <= 5)]) # P(3 < R$X + R$Y <= 5)

v = seq(0, 20, 2)
v[5]
which(v == 8)
which(2 <= v & v <= 8)
sum(v[2:5])
sum(v[which(2 <= v & v <= 8)])

# With X and Y as in the previous part, find the conditional PMF of X given that X + Y = 6. Solution:
# Given X + Y = 6, the possible values of X (i.e. the conditional sample space) are 3 and 2.
R$X[which(R$X + R$Y == 6)] # gives the conditional sample space
R$px[which(R$X + R$Y == 6)] * R$py[which(R$X + R$Y == 6)]/sum(R$pxy[which(R$X + R$Y == 6)]) # gives the conditional pmf

pXYm[1, ]/rowSums(pXYm)[1] # gives P(Y = yjX = 0), y = 0 : 3
dbinom(0:3, 3, 0.5) # why?
print("Earlier, we assigned pXYm to be pXYm = dbinom(0:2, 2, 0.5) %*% t(dbinom(0:3, 3, 0.5)), which is the joint probability distribution of random vars X and Y. Now we divided pXYm by the sum of the rows (PYX=x), and we get dbinom(0:3, 3,0.5) back. This makes sense because dbinom gives you the values of probability from 0:3, and the values are the same as the corresponding marginal probability density function pY of pXYm -> P(Y = yjX = 0), y = 0 : 3")

pXYm[2, ]/rowSums(pXYm)[2] # gives P(Y = yjX = 1), y = 0 : 3
pXYm[3, ]/rowSums(pXYm)[3] # gives P(Y = yjX = 2), y = 0 : 3
print("These are the same")
pxy = c(0.1, 0.08, 0.06, 0.04, 0.2, 0.14, 0.02, 0.06, 0.3)
pxym = matrix(pxy, 3, 3)
px = rowSums(pxym)
py = colSums(pxym)
P = expand.grid(px = px, py = py)
P$pxy = pxy
G = expand.grid(x = 0:2, y = 0:2)
P$x = G$x
P$y = G$y
sum((P$x + P$y) * P$pxy)
sum(P$x * P$pxy)
sum(P$y * P$pxy)
sum((P$x + P$y)^2 * P$pxy)
sum(pmin(P$x, P$y) * P$pxy)
sum(pmin(P$x, P$y)^2 * P$pxy)

# E(e^(X+Y)) =
sum(exp(P$x + P$y) * P$pxy)

# PROOF By Simulation
m = matrix(rnorm(50000), ncol = 10000)
mean(apply(m, 2, var))

integrate(dnorm, -Inf, Inf)
integrate(dnorm, -Inf, Inf)$value
integrate(dnorm, -Inf, 1.1) # same as pnorm(1.1)
pnorm(1.1)

# Define your own function and integrate it:
f = function(x) {
  1/((x + 1) * sqrt(x))
}
integrate(f, lower = 0, upper = 5)

# Double Integral:
g1 = function(y) {
  integrate(function(x) tan(x + y), -0.5, y)$value
} # the inside integral
g2 = function(y) {
  sapply(y, g1)
} # note g2(y)=g1(y) for all y
integrate(g2, 0, 0.5)
