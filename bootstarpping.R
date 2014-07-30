library(car)
library(MASS)
data(Duncan)
dim(Duncan)
attributes(Duncan)

mod.duncan.hub <- rlm(prestige ~ income + education, data = Duncan)
summary(mod.duncan.hub)

boot.huber <- function(data, indices, maxit = 20){
  data <- data[indices, ] #select obs. in bootstrap sample
  mod <- rlm(prestige ~ income + education, data = data, maxit = maxit)
  coefficients(mod) #return coefficient vector
}

library(boot)
system.time(duncan.boot <- boot(Duncan, boot.huber, 1999, maxit = 100))
duncan.boot

duncan.array <- boot.array(duncan.boot)
duncan.array[1:2,]
dim(duncan.array)

#BOOTSTRAP DISTRIBUTIONS####
plot(duncan.boot, index = 2) #income coef.
plot(duncan.boot, index = 3) #education coef.

#JOIN DISTRIBUTION OF THE BOOTSTRAPPED####
dataEllipse(duncan.boot$t[, 2], duncan.boot$t[, 3], xlab = "income coefficient", ylab = "education coefficient", cex=.3, levels = c (0.5, 0.95, 0.99), robust = T)

#BOOTSTRAP CONFIDENCE INTERVALS####
boot.ci(duncan.boot, index = 2, type = c("norm", "perc", "bca"))
boot.ci(duncan.boot, index = 3, type = c("norm", "perc", "bca"))

#SENSITIVITY OF THE STATISTIC
par(mfcol = c(2, 1))
jack.after.boot(duncan.boot, index = 2, main = "(a) income coefficient")
jack.after.boot(duncan.boot, index = 3, main = "(b) education coefficient")


##FIXED-x RESAMPLING
fit <- fitted(mod.duncan.hub)
e <- residuals(mod.duncan.hub)
X <- model.matrix(mod.duncan.hub)
boot.huber.fixed <- function(data, indices, maxit = 20){
  y <- fit + e[indices]
  mod <- rlm (y ~ X - 1, maxit = maxit)
  coefficients(mod)
}

duncan.fix.boot <- boot(Duncan, boot.huber.fixed, 1999, maxit = 100)
duncan.fix.boot

#BOOTSTRAP DISTRIBUTIONS####
plot(duncan.fix.boot, index = 2) #income coef.
plot(duncan.fix.boot, index = 3) #education coef.

#JOIN DISTRIBUTION OF THE BOOTSTRAPPED####
dataEllipse(duncan.fix.boot$t[, 2], duncan.fix.boot$t[, 3], xlab = "income coefficient", ylab = "education coefficient", cex=.3, levels = c (0.5, 0.95, 0.99), robust = T)

#BOOTSTRAP CONFIDENCE INTERVALS####
boot.ci(duncan.fix.boot, index = 2, type = c("norm", "perc", "bca"))
boot.ci(duncan.fix.boot, index = 3, type = c("norm", "perc", "bca"))

#SENSITIVITY OF THE STATISTIC
par(mfcol = c(2, 1))
jack.after.boot(duncan.fix.boot, index = 2, main = "(a) income coefficient")
jack.after.boot(duncan.fix.boot, index = 3, main = "(b) education coefficient")

rm()