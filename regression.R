#For diferent biotopes####
#m - meadow
mf <- c(56) #m - meadow - dominated landscape
mm <- c(68, 75, 77, 84) #shrub-dominated (m - mixed)
ma <- c(1, 9, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25, 27) #meadow-dominated (a - agriculture)
mea <- c(mf, mm, ma)
#s - short shrub
sf <- c(36, 39, 42, 45, 46) 
sm <- c(60, 64, 65, 66, 67, 70, 78, 79, 82)
sa <- c(2, 8, 10, 11, 12, 13, 20)
shr <- c(sf, sm, sa)
#t - tall shrub
tf <- c(34, 49) #t - shrub-dominated landscape
tm <- c(57, 58, 59, 61, 62, 63, 69, 71, 72, 74, 76)
ta <- c(3, 4, 5, 6, 7, 28)
tal <- c(tf, tm, ta)
#w - woodland
wf <- c(29, 30, 31, 32, 33, 35, 37, 38, 40, 41, 43, 44, 47, 48, 50, 51, 52, 53, 54, 55) 
wm <- c(73, 80, 81, 83)
wa <- c(22, 26)
woo <- c(wf, wm, wa)

#within biotope
##ALL YEARS
meadata <- landsc_within(b.div, mea, mea)
shrdata <- landsc_within(b.div, shr, shr)
taldata <- landsc_within(b.div, tal, tal)
woodata <- landsc_within(b.div, woo, woo)
##FIRST YEAR
meadata1 <- landsc_within(b.div1, mea, mea)
shrdata1 <- landsc_within(b.div1, shr, shr)
taldata1 <- landsc_within(b.div1, tal, tal)
woodata1 <- landsc_within(b.div1, woo, woo)
##SECOND YEAR
meadata2 <- landsc_within(b.div2, mea, mea)
shrdata2 <- landsc_within(b.div2, shr, shr)
taldata2 <- landsc_within(b.div2, tal, tal)
woodata2 <- landsc_within(b.div2, woo, woo)

par(mfrow = c(4, 3))
##JACCARD ESTIMATOR (regressionJAC.tiff)
modelmea <- lm(1 - meadata[, 19] ~ meadata[, 21])
modelmea1 <- lm(1 - meadata1[, 19] ~ meadata1[, 21])
modelmea2 <- lm(1 - meadata2[, 19] ~ meadata2[, 21])

plot(meadata[, 21], 1 - meadata[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture")
abline(a = coef(modelmea)[[1]], b = coef(modelmea)[[2]], col = 2, lwd = 4.5)
plot(meadata1[, 21], 1 - meadata1[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 1")
abline(a = coef(modelmea1)[[1]], b = coef(modelmea1)[[2]], col = 2, lwd = 4.5)
plot(meadata2[, 21], 1 - meadata2[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 2")
abline(a = coef(modelmea2)[[1]], b = coef(modelmea2)[[2]], col = 2, lwd = 4.5)

modelshr <- lm(1 - shrdata[, 19] ~ shrdata[, 21])
modelshr1 <- lm(1 - shrdata1[, 19] ~ shrdata1[, 21])
modelshr2 <- lm(1 - shrdata2[, 19] ~ shrdata2[, 21])

plot(shrdata[, 21], 1 - shrdata[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Short Shrub")
abline(a = coef(modelshr)[[1]], b = coef(modelshr)[[2]], col = 2, lwd = 4.5)
plot(shrdata1[, 21], 1 - shrdata1[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Short Shrub - Year 1")
abline(a = coef(modelshr1)[[1]], b = coef(modelshr1)[[2]], col = 2, lwd = 4.5)
plot(shrdata2[, 21], 1 - shrdata2[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Short Shrub - Year 2")
abline(a = coef(modelshr2)[[1]], b = coef(modelshr2)[[2]], col = 2, lwd = 4.5)

modeltal <- lm(1 - taldata[, 19] ~ taldata[, 21])
modeltal1 <- lm(1 - taldata1[, 19] ~ taldata1[, 21])
modeltal2 <- lm(1 - taldata2[, 19] ~ taldata2[, 21])

plot(taldata[, 21], 1 - taldata[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Tall Shrub")
abline(a = coef(modeltal)[[1]], b = coef(modeltal)[[2]], col = 2, lwd = 4.5)
plot(taldata1[, 21], 1 - taldata1[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Tall Shrub - Year 1")
abline(a = coef(modeltal1)[[1]], b = coef(modeltal1)[[2]], col = 2, lwd = 4.5)
plot(taldata2[, 21], 1 - taldata2[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Tall Shrub - Year 2")
abline(a = coef(modeltal2)[[1]], b = coef(modeltal2)[[2]], col = 2, lwd = 4.5)

modelwoo <- lm(1 - woodata[, 19] ~ woodata[, 21])
modelwoo1 <- lm(1 - woodata1[, 19] ~ woodata1[, 21])
modelwoo2 <- lm(1 - woodata2[, 19] ~ woodata2[, 21])

plot(woodata[, 21], 1 - woodata[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Woodland")
abline(a = coef(modelwoo)[[1]], b = coef(modelwoo)[[2]], col = 2, lwd = 4.5)
plot(woodata1[, 21], 1 - woodata1[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Woodland - Year 1")
abline(a = coef(modelwoo1)[[1]], b = coef(modelwoo1)[[2]], col = 2, lwd = 4.5)
plot(woodata2[, 21], 1 - woodata2[, 19], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Woodland - Year 2")
abline(a = coef(modelwoo2)[[1]], b = coef(modelwoo2)[[2]], col = 2, lwd = 4.5)

##SORENSEN ESTIMATOR (regressionSOR.tiff)
par(mfrow = c(4, 3))
modelmea <- lm(1 - meadata[, 20] ~ meadata[, 21])
modelmea1 <- lm(1 - meadata1[, 20] ~ meadata1[, 21])
modelmea2 <- lm(1 - meadata2[, 20] ~ meadata2[, 21])

plot(meadata[, 21], 1 - meadata[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture")
abline(a = coef(modelmea)[[1]], b = coef(modelmea)[[2]], col = 2, lwd = 4.5)
plot(meadata1[, 21], 1 - meadata1[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 1")
abline(a = coef(modelmea1)[[1]], b = coef(modelmea1)[[2]], col = 2, lwd = 4.5)
plot(meadata2[, 21], 1 - meadata2[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 2")
abline(a = coef(modelmea2)[[1]], b = coef(modelmea2)[[2]], col = 2, lwd = 4.5)

modelshr <- lm(1 - shrdata[, 20] ~ shrdata[, 21])
modelshr1 <- lm(1 - shrdata1[, 20] ~ shrdata1[, 21])
modelshr2 <- lm(1 - shrdata2[, 20] ~ shrdata2[, 21])

plot(shrdata[, 21], 1 - shrdata[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Short Shrub")
abline(a = coef(modelshr)[[1]], b = coef(modelshr)[[2]], col = 2, lwd = 4.5)
plot(shrdata1[, 21], 1 - shrdata1[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Short Shrub - Year 1")
abline(a = coef(modelshr1)[[1]], b = coef(modelshr1)[[2]], col = 2, lwd = 4.5)
plot(shrdata2[, 21], 1 - shrdata2[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Short Shrub - Year 2")
abline(a = coef(modelshr2)[[1]], b = coef(modelshr2)[[2]], col = 2, lwd = 4.5)

modeltal <- lm(1 - taldata[, 20] ~ taldata[, 21])
modeltal1 <- lm(1 - taldata1[, 20] ~ taldata1[, 21])
modeltal2 <- lm(1 - taldata2[, 20] ~ taldata2[, 21])

plot(taldata[, 21], 1 - taldata[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Tall Shrub")
abline(a = coef(modeltal)[[1]], b = coef(modeltal)[[2]], col = 2, lwd = 4.5)
plot(taldata1[, 21], 1 - taldata1[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Tall Shrub - Year 1")
abline(a = coef(modeltal1)[[1]], b = coef(modeltal1)[[2]], col = 2, lwd = 4.5)
plot(taldata2[, 21], 1 - taldata2[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Tall Shrub - Year 2")
abline(a = coef(modeltal2)[[1]], b = coef(modeltal2)[[2]], col = 2, lwd = 4.5)

modelwoo <- lm(1 - woodata[, 20] ~ woodata[, 21])
modelwoo1 <- lm(1 - woodata1[, 20] ~ woodata1[, 21])
modelwoo2 <- lm(1 - woodata2[, 20] ~ woodata2[, 21])

plot(woodata[, 21], 1 - woodata[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Woodland")
abline(a = coef(modelwoo)[[1]], b = coef(modelwoo)[[2]], col = 2, lwd = 4.5)
plot(woodata1[, 21], 1 - woodata1[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Woodland - Year 1")
abline(a = coef(modelwoo1)[[1]], b = coef(modelwoo1)[[2]], col = 2, lwd = 4.5)
plot(woodata2[, 21], 1 - woodata2[, 20], xlim = c(0, 7400), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Woodland - Year 2")
abline(a = coef(modelwoo2)[[1]], b = coef(modelwoo2)[[2]], col = 2, lwd = 4.5)

# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs.jac <- function(formula, data, ind){
  d <- data[ind,] # allows boot to select sample
  fit <- lm(formula, data = d)
  return(c(coef(fit), summary(fit)$r.square, mean(d[, 19]))) 
} 

bs.sor <- function(formula, data, ind){
  d <- data[ind,] # allows boot to select sample
  fit <- lm(formula, data = d)
  return(c(coef(fit), summary(fit)$r.square, mean(d[, 20]))) 
} 

# bootstrapping with 2000 replications
#Jaccard Estimator
jwoores <- boot(data = as.data.frame(woodata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jwoores1 <- boot(data = as.data.frame(woodata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jwoores2 <- boot(data = as.data.frame(woodata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

jtalres <- boot(data = as.data.frame(taldata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jtalres1 <- boot(data = as.data.frame(taldata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jtalres2 <- boot(data = as.data.frame(taldata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

jshrres <- boot(data = as.data.frame(shrdata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jshrres1 <- boot(data = as.data.frame(shrdata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jshrres2 <- boot(data = as.data.frame(shrdata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

jmeares <- boot(data = as.data.frame(meadata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jmeares1 <- boot(data = as.data.frame(meadata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jmeares2 <- boot(data = as.data.frame(meadata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

#Sorensen Estimator
swoores <- boot(data = as.data.frame(woodata), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
swoores1 <- boot(data = as.data.frame(woodata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
swoores2 <- boot(data = as.data.frame(woodata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

stalres <- boot(data = as.data.frame(taldata), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
stalres1 <- boot(data = as.data.frame(taldata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
stalres2 <- boot(data = as.data.frame(taldata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

sshrres <- boot(data = as.data.frame(shrdata), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
sshrres1 <- boot(data = as.data.frame(shrdata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
sshrres2 <- boot(data = as.data.frame(shrdata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

smeares <- boot(data = as.data.frame(meadata), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares1 <- boot(data = as.data.frame(meadata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares2 <- boot(data = as.data.frame(meadata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

# view results
plot(jwoores, index = 1) # intercept 
plot(jwoores, index = 2) # slope 
plot(jwoores, index = 3) # r.square
plot(jwoores, index = 4) # mean

# get 95% confidence intervals
###Jaccard Estimators
##woodland
temp1 <- boot.ci(jwoores, type="bca", index=1) # intercept 
temp2 <- boot.ci(jwoores, type="bca", index=2) # slope
temp3 <- boot.ci(jwoores, type="bca", index=3) # r.square
temp4 <- boot.ci(jwoores, type="bca", index=4) # mean

woores.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jwoores1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jwoores1, type="bca", index=2) # slope
temp3 <- boot.ci(jwoores1, type="bca", index=3) # r.square
temp4 <- boot.ci(jwoores1, type="bca", index=4) # mean

woores1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jwoores2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jwoores2, type="bca", index=2) # slope
temp3 <- boot.ci(jwoores2, type="bca", index=3) # r.square
temp4 <- boot.ci(jwoores2, type="bca", index=4) # mean

woores2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Tall - shrub
temp1 <- boot.ci(jtalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jtalres, type="bca", index=2) # slope
temp3 <- boot.ci(jtalres, type="bca", index=3) # r.square
temp4 <- boot.ci(jtalres, type="bca", index=4) # mean

talres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jtalres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jtalres1, type="bca", index=2) # slope
temp3 <- boot.ci(jtalres1, type="bca", index=3) # r.square
temp4 <- boot.ci(jtalres1, type="bca", index=4) # mean

talres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jtalres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jtalres2, type="bca", index=2) # slope
temp3 <- boot.ci(jtalres2, type="bca", index=3) # r.square
temp4 <- boot.ci(jtalres2, type="bca", index=4) # mean

talres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Short - shrub
temp1 <- boot.ci(jshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jshrres, type="bca", index=2) # slope
temp3 <- boot.ci(jshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(jshrres, type="bca", index=4) # mean

shrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jshrres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jshrres1, type="bca", index=2) # slope
temp3 <- boot.ci(jshrres1, type="bca", index=3) # r.square
temp4 <- boot.ci(jshrres1, type="bca", index=4) # mean

shrres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jshrres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jshrres2, type="bca", index=2) # slope
temp3 <- boot.ci(jshrres2, type="bca", index=3) # r.square
temp4 <- boot.ci(jshrres2, type="bca", index=4) # mean

shrres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Meadow
temp1 <- boot.ci(jmeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares, type="bca", index=4) # mean

meares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares1, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares1, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares1, type="bca", index=4) # mean

meares1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares2, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares2, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares2, type="bca", index=4) # mean

meares2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
##ALL Years (jac.boot.tiff)
#Intercept
mean.ci <- c(meares.ci[1, 1], shrres.ci[1, 1], talres.ci[1, 1], woores.ci[1, 1])
l.ci <- c(meares.ci[1, 2], shrres.ci[1, 2], talres.ci[1, 2], woores.ci[1, 2])
s.ci <- c(meares.ci[1, 3], shrres.ci[1, 3], talres.ci[1, 3], woores.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(meares.ci[4, 1], shrres.ci[4, 1], talres.ci[4, 1], woores.ci[4, 1])
l.ci <- c(meares.ci[4, 2], shrres.ci[4, 2], talres.ci[4, 2], woores.ci[4, 2])
s.ci <- c(meares.ci[4, 3], shrres.ci[4, 3], talres.ci[4, 3], woores.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(meares.ci[2, 1], shrres.ci[2, 1], talres.ci[2, 1], woores.ci[2, 1])
l.ci <- c(meares.ci[2, 2], shrres.ci[2, 2], talres.ci[2, 2], woores.ci[2, 2])
s.ci <- c(meares.ci[2, 3], shrres.ci[2, 3], talres.ci[2, 3], woores.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(meares.ci[3, 1], shrres.ci[3, 1], talres.ci[3, 1], woores.ci[3, 1])
l.ci <- c(meares.ci[3, 2], shrres.ci[3, 2], talres.ci[3, 2], woores.ci[3, 2])
s.ci <- c(meares.ci[3, 3], shrres.ci[3, 3], talres.ci[3, 3], woores.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

##First Year (jac.boot1.tiff)
#Intercept
mean.ci <- c(meares1.ci[1, 1], shrres1.ci[1, 1], talres1.ci[1, 1], woores1.ci[1, 1])
l.ci <- c(meares1.ci[1, 2], shrres1.ci[1, 2], talres1.ci[1, 2], woores1.ci[1, 2])
s.ci <- c(meares1.ci[1, 3], shrres1.ci[1, 3], talres1.ci[1, 3], woores1.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("First Year - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(meares1.ci[4, 1], shrres1.ci[4, 1], talres1.ci[4, 1], woores1.ci[4, 1])
l.ci <- c(meares1.ci[4, 2], shrres1.ci[4, 2], talres1.ci[4, 2], woores1.ci[4, 2])
s.ci <- c(meares1.ci[4, 3], shrres1.ci[4, 3], talres1.ci[4, 3], woores1.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(meares1.ci[2, 1], shrres1.ci[2, 1], talres1.ci[2, 1], woores1.ci[2, 1])
l.ci <- c(meares1.ci[2, 2], shrres1.ci[2, 2], talres1.ci[2, 2], woores1.ci[2, 2])
s.ci <- c(meares1.ci[2, 3], shrres1.ci[2, 3], talres1.ci[2, 3], woores1.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(meares1.ci[3, 1], shrres1.ci[3, 1], talres1.ci[3, 1], woores1.ci[3, 1])
l.ci <- c(meares1.ci[3, 2], shrres1.ci[3, 2], talres1.ci[3, 2], woores1.ci[3, 2])
s.ci <- c(meares1.ci[3, 3], shrres1.ci[3, 3], talres1.ci[3, 3], woores1.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

##Second Year (jac.boot2.tiff)
#Intercept
mean.ci <- c(meares2.ci[1, 1], shrres2.ci[1, 1], talres2.ci[1, 1], woores2.ci[1, 1])
l.ci <- c(meares2.ci[1, 2], shrres2.ci[1, 2], talres2.ci[1, 2], woores2.ci[1, 2])
s.ci <- c(meares2.ci[1, 3], shrres2.ci[1, 3], talres2.ci[1, 3], woores2.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("Second Year - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(meares2.ci[4, 1], shrres2.ci[4, 1], talres2.ci[4, 1], woores2.ci[4, 1])
l.ci <- c(meares2.ci[4, 2], shrres2.ci[4, 2], talres2.ci[4, 2], woores2.ci[4, 2])
s.ci <- c(meares2.ci[4, 3], shrres2.ci[4, 3], talres2.ci[4, 3], woores2.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(meares2.ci[2, 1], shrres2.ci[2, 1], talres2.ci[2, 1], woores2.ci[2, 1])
l.ci <- c(meares2.ci[2, 2], shrres2.ci[2, 2], talres2.ci[2, 2], woores2.ci[2, 2])
s.ci <- c(meares2.ci[2, 3], shrres2.ci[2, 3], talres2.ci[2, 3], woores2.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(meares2.ci[3, 1], shrres2.ci[3, 1], talres2.ci[3, 1], woores2.ci[3, 1])
l.ci <- c(meares2.ci[3, 2], shrres2.ci[3, 2], talres2.ci[3, 2], woores2.ci[3, 2])
s.ci <- c(meares2.ci[3, 3], shrres2.ci[3, 3], talres2.ci[3, 3], woores2.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

###Sorensen Estimator
##woodland
temp1 <- boot.ci(swoores, type="bca", index=1) # intercept 
temp2 <- boot.ci(swoores, type="bca", index=2) # slope
temp3 <- boot.ci(swoores, type="bca", index=3) # r.square
temp4 <- boot.ci(swoores, type="bca", index=4) # mean

swoores.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(swoores1, type="bca", index=1) # intercept 
temp2 <- boot.ci(swoores1, type="bca", index=2) # slope
temp3 <- boot.ci(swoores1, type="bca", index=3) # r.square
temp4 <- boot.ci(swoores1, type="bca", index=4) # mean

swoores1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(swoores2, type="bca", index=1) # intercept 
temp2 <- boot.ci(swoores2, type="bca", index=2) # slope
temp3 <- boot.ci(swoores2, type="bca", index=3) # r.square
temp4 <- boot.ci(swoores2, type="bca", index=4) # mean

swoores2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Tall - shrub
temp1 <- boot.ci(stalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(stalres, type="bca", index=2) # slope
temp3 <- boot.ci(stalres, type="bca", index=3) # r.square
temp4 <- boot.ci(stalres, type="bca", index=4) # mean

stalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(stalres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(stalres1, type="bca", index=2) # slope
temp3 <- boot.ci(stalres1, type="bca", index=3) # r.square
temp4 <- boot.ci(stalres1, type="bca", index=4) # mean

stalres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(stalres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(stalres2, type="bca", index=2) # slope
temp3 <- boot.ci(stalres2, type="bca", index=3) # r.square
temp4 <- boot.ci(stalres2, type="bca", index=4) # mean

stalres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Short - shrub
temp1 <- boot.ci(sshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sshrres, type="bca", index=2) # slope
temp3 <- boot.ci(sshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(sshrres, type="bca", index=4) # mean

sshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sshrres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(sshrres1, type="bca", index=2) # slope
temp3 <- boot.ci(sshrres1, type="bca", index=3) # r.square
temp4 <- boot.ci(sshrres1, type="bca", index=4) # mean

sshrres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sshrres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(sshrres2, type="bca", index=2) # slope
temp3 <- boot.ci(sshrres2, type="bca", index=3) # r.square
temp4 <- boot.ci(sshrres2, type="bca", index=4) # mean

sshrres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Meadow
temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares1, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares1, type="bca", index=2) # slope
temp3 <- boot.ci(smeares1, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares1, type="bca", index=4) # mean

smeares1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares2, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares2, type="bca", index=2) # slope
temp3 <- boot.ci(smeares2, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares2, type="bca", index=4) # mean

smeares2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
##ALL Years (sor.boot.tiff)
#Intercept
mean.ci <- c(smeares.ci[1, 1], sshrres.ci[1, 1], stalres.ci[1, 1], swoores.ci[1, 1])
l.ci <- c(smeares.ci[1, 2], sshrres.ci[1, 2], stalres.ci[1, 2], swoores.ci[1, 2])
s.ci <- c(smeares.ci[1, 3], sshrres.ci[1, 3], stalres.ci[1, 3], swoores.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(smeares.ci[4, 1], sshrres.ci[4, 1], stalres.ci[4, 1], swoores.ci[4, 1])
l.ci <- c(smeares.ci[4, 2], sshrres.ci[4, 2], stalres.ci[4, 2], swoores.ci[4, 2])
s.ci <- c(smeares.ci[4, 3], sshrres.ci[4, 3], stalres.ci[4, 3], swoores.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(smeares.ci[2, 1], sshrres.ci[2, 1], stalres.ci[2, 1], swoores.ci[2, 1])
l.ci <- c(smeares.ci[2, 2], sshrres.ci[2, 2], stalres.ci[2, 2], swoores.ci[2, 2])
s.ci <- c(smeares.ci[2, 3], sshrres.ci[2, 3], stalres.ci[2, 3], swoores.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(smeares.ci[3, 1], sshrres.ci[3, 1], stalres.ci[3, 1], swoores.ci[3, 1])
l.ci <- c(smeares.ci[3, 2], sshrres.ci[3, 2], stalres.ci[3, 2], swoores.ci[3, 2])
s.ci <- c(smeares.ci[3, 3], sshrres.ci[3, 3], stalres.ci[3, 3], swoores.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

##First Year (sor.boot1.tiff)
#Intercept
mean.ci <- c(smeares1.ci[1, 1], sshrres1.ci[1, 1], stalres1.ci[1, 1], swoores1.ci[1, 1])
l.ci <- c(smeares1.ci[1, 2], sshrres1.ci[1, 2], stalres1.ci[1, 2], swoores1.ci[1, 2])
s.ci <- c(smeares1.ci[1, 3], sshrres1.ci[1, 3], stalres1.ci[1, 3], swoores1.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("First Year - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(smeares1.ci[4, 1], sshrres1.ci[4, 1], stalres1.ci[4, 1], swoores1.ci[4, 1])
l.ci <- c(smeares1.ci[4, 2], sshrres1.ci[4, 2], stalres1.ci[4, 2], swoores1.ci[4, 2])
s.ci <- c(smeares1.ci[4, 3], sshrres1.ci[4, 3], stalres1.ci[4, 3], swoores1.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(smeares1.ci[2, 1], sshrres1.ci[2, 1], stalres1.ci[2, 1], swoores1.ci[2, 1])
l.ci <- c(smeares1.ci[2, 2], sshrres1.ci[2, 2], stalres1.ci[2, 2], swoores1.ci[2, 2])
s.ci <- c(smeares1.ci[2, 3], sshrres1.ci[2, 3], stalres1.ci[2, 3], swoores1.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(smeares1.ci[3, 1], sshrres1.ci[3, 1], stalres1.ci[3, 1], swoores1.ci[3, 1])
l.ci <- c(smeares1.ci[3, 2], sshrres1.ci[3, 2], stalres1.ci[3, 2], swoores1.ci[3, 2])
s.ci <- c(smeares1.ci[3, 3], sshrres1.ci[3, 3], stalres1.ci[3, 3], swoores1.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

##Second Year (sor.boot2.tiff)
#Intercept
mean.ci <- c(smeares2.ci[1, 1], sshrres2.ci[1, 1], stalres2.ci[1, 1], swoores2.ci[1, 1])
l.ci <- c(smeares2.ci[1, 2], sshrres2.ci[1, 2], stalres2.ci[1, 2], swoores2.ci[1, 2])
s.ci <- c(smeares2.ci[1, 3], sshrres2.ci[1, 3], stalres2.ci[1, 3], swoores2.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("Second Year - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(smeares2.ci[4, 1], sshrres2.ci[4, 1], stalres2.ci[4, 1], swoores2.ci[4, 1])
l.ci <- c(smeares2.ci[4, 2], sshrres2.ci[4, 2], stalres2.ci[4, 2], swoores2.ci[4, 2])
s.ci <- c(smeares2.ci[4, 3], sshrres2.ci[4, 3], stalres2.ci[4, 3], swoores2.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(smeares2.ci[2, 1], sshrres2.ci[2, 1], stalres2.ci[2, 1], swoores2.ci[2, 1])
l.ci <- c(smeares2.ci[2, 2], sshrres2.ci[2, 2], stalres2.ci[2, 2], swoores2.ci[2, 2])
s.ci <- c(smeares2.ci[2, 3], sshrres2.ci[2, 3], stalres2.ci[2, 3], swoores2.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(smeares2.ci[3, 1], sshrres2.ci[3, 1], stalres2.ci[3, 1], swoores2.ci[3, 1])
l.ci <- c(smeares2.ci[3, 2], sshrres2.ci[3, 2], stalres2.ci[3, 2], swoores2.ci[3, 2])
s.ci <- c(smeares2.ci[3, 3], sshrres2.ci[3, 3], stalres2.ci[3, 3], swoores2.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Agri", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

##Within landscapes
idmea <- 1:28
idmix <- 57:84
idfor <- 29:56

###ALL YEARS####
meadata <- landsc_within(b.div, idmea, idmea); comment(meadata) <- "MEADOW"
mixdata <- landsc_within(b.div, idmix, idmix); comment(mixdata) <- "MIXED"
fordata <- landsc_within(b.div, idfor, idfor); comment(fordata) <- "FOREST"

###FIRST YEAR####
meadata1 <- landsc_within(b.div1, idmea, idmea); comment(meadata1) <- "MEADOW 1"
mixdata1 <- landsc_within(b.div1, idmix, idmix); comment(mixdata1) <- "MIXED 1"
fordata1 <- landsc_within(b.div1, idfor, idfor); comment(fordata1) <- "FOREST 1"

###SECOND YEAR####
meadata2 <- landsc_within(b.div2, idmea, idmea); comment(meadata2) <- "MEADOW 2"
mixdata2 <- landsc_within(b.div2, idmix, idmix); comment(mixdata2) <- "MIXED 2"
fordata2 <- landsc_within(b.div2, idfor, idfor); comment(fordata2) <- "FOREST 2"

par(mfrow = c(3, 3))
##JACCARD ESTIMATOR (regressionJAC2.tiff)
modelmea <- lm(1 - meadata[, 19] ~ meadata[, 21])
modelmea1 <- lm(1 - meadata1[, 19] ~ meadata1[, 21])
modelmea2 <- lm(1 - meadata2[, 19] ~ meadata2[, 21])

plot(meadata[, 21], 1 - meadata[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture")
abline(a = coef(modelmea)[[1]], b = coef(modelmea)[[2]], col = 2, lwd = 4.5)
plot(meadata1[, 21], 1 - meadata1[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 1")
abline(a = coef(modelmea1)[[1]], b = coef(modelmea1)[[2]], col = 2, lwd = 4.5)
plot(meadata2[, 21], 1 - meadata2[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 2")
abline(a = coef(modelmea2)[[1]], b = coef(modelmea2)[[2]], col = 2, lwd = 4.5)

modelmix <- lm(1 - mixdata[, 19] ~ mixdata[, 21])
modelmix1 <- lm(1 - mixdata1[, 19] ~ mixdata1[, 21])
modelmix2 <- lm(1 - mixdata2[, 19] ~ mixdata2[, 21])

plot(mixdata[, 21], 1 - mixdata[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Mixed")
abline(a = coef(modelmix)[[1]], b = coef(modelmix)[[2]], col = 2, lwd = 4.5)
plot(mixdata1[, 21], 1 - mixdata1[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Mixed - Year 1")
abline(a = coef(modelmix1)[[1]], b = coef(modelmix1)[[2]], col = 2, lwd = 4.5)
plot(mixdata2[, 21], 1 - mixdata2[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Mixed - Year 2")
abline(a = coef(modelmix2)[[1]], b = coef(modelmix2)[[2]], col = 2, lwd = 4.5)

modelfor <- lm(1 - fordata[, 19] ~ fordata[, 21])
modelfor1 <- lm(1 - fordata1[, 19] ~ fordata1[, 21])
modelfor2 <- lm(1 - fordata2[, 19] ~ fordata2[, 21])

plot(fordata[, 21], 1 - fordata[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(modelfor)[[1]], b = coef(modelfor)[[2]], col = 2, lwd = 4.5)
plot(fordata1[, 21], 1 - fordata1[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Forest - Year 1")
abline(a = coef(modelfor1)[[1]], b = coef(modelfor1)[[2]], col = 2, lwd = 4.5)
plot(fordata2[, 21], 1 - fordata2[, 19], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Forest - Year 2")
abline(a = coef(modelfor2)[[1]], b = coef(modelfor2)[[2]], col = 2, lwd = 4.5)

##SORENSEN ESTIMATOR (regressionSOR2.tiff)
modelmea <- lm(1 - meadata[, 20] ~ meadata[, 21])
modelmea1 <- lm(1 - meadata1[, 20] ~ meadata1[, 21])
modelmea2 <- lm(1 - meadata2[, 20] ~ meadata2[, 21])

plot(meadata[, 21], 1 - meadata[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture")
abline(a = coef(modelmea)[[1]], b = coef(modelmea)[[2]], col = 2, lwd = 4.5)
plot(meadata1[, 21], 1 - meadata1[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 1")
abline(a = coef(modelmea1)[[1]], b = coef(modelmea1)[[2]], col = 2, lwd = 4.5)
plot(meadata2[, 21], 1 - meadata2[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Agriculture - Year 2")
abline(a = coef(modelmea2)[[1]], b = coef(modelmea2)[[2]], col = 2, lwd = 4.5)

modelmix <- lm(1 - mixdata[, 20] ~ mixdata[, 21])
modelmix1 <- lm(1 - mixdata1[, 20] ~ mixdata1[, 21])
modelmix2 <- lm(1 - mixdata2[, 20] ~ mixdata2[, 21])

plot(mixdata[, 21], 1 - mixdata[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Mixed")
abline(a = coef(modelmix)[[1]], b = coef(modelmix)[[2]], col = 2, lwd = 4.5)
plot(mixdata1[, 21], 1 - mixdata1[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Mixed - Year 1")
abline(a = coef(modelmix1)[[1]], b = coef(modelmix1)[[2]], col = 2, lwd = 4.5)
plot(mixdata2[, 21], 1 - mixdata2[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Mixed - Year 2")
abline(a = coef(modelmix2)[[1]], b = coef(modelmix2)[[2]], col = 2, lwd = 4.5)

modelfor <- lm(1 - fordata[, 20] ~ fordata[, 21])
modelfor1 <- lm(1 - fordata1[, 20] ~ fordata1[, 21])
modelfor2 <- lm(1 - fordata2[, 20] ~ fordata2[, 21])

plot(fordata[, 21], 1 - fordata[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(modelfor)[[1]], b = coef(modelfor)[[2]], col = 2, lwd = 4.5)
plot(fordata1[, 21], 1 - fordata1[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Forest - Year 1")
abline(a = coef(modelfor1)[[1]], b = coef(modelfor1)[[2]], col = 2, lwd = 4.5)
plot(fordata2[, 21], 1 - fordata2[, 20], xlim = c(0, 2000), ylim = c(0, 1), xlab = "Geographic Distance (m)" , ylab = expression(~beta*"-diversity"), main = "Forest - Year 2")
abline(a = coef(modelfor2)[[1]], b = coef(modelfor2)[[2]], col = 2, lwd = 4.5)

# bootstrapping with 2000 replications
#Jaccard Estimator
jforres <- boot(data = as.data.frame(fordata), statistic = bs.jac, R = 3000, formula = (1 - Jabd) ~ ipcc)
jforres1 <- boot(data = as.data.frame(fordata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jforres2 <- boot(data = as.data.frame(fordata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

jmixres <- boot(data = as.data.frame(mixdata), statistic = bs.jac, R = 3000, formula = (1 - Jabd) ~ ipcc)
jmixres1 <- boot(data = as.data.frame(mixdata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jmixres2 <- boot(data = as.data.frame(mixdata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

jmeares <- boot(data = as.data.frame(meadata), statistic = bs.jac, R = 3000, formula = (1 - Jabd) ~ ipcc)
jmeares1 <- boot(data = as.data.frame(meadata1), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
jmeares2 <- boot(data = as.data.frame(meadata2), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)

#Sorensen Estimator
sforres <- boot(data = as.data.frame(fordata), statistic = bs.sor, R = 3000, formula = (1 - Jabd) ~ ipcc)
sforres1 <- boot(data = as.data.frame(fordata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
sforres2 <- boot(data = as.data.frame(fordata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

smixres <- boot(data = as.data.frame(mixdata), statistic = bs.sor, R = 3000, formula = (1 - Jabd) ~ ipcc)
smixres1 <- boot(data = as.data.frame(mixdata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
smixres2 <- boot(data = as.data.frame(mixdata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

smeares <- boot(data = as.data.frame(meadata), statistic = bs.sor, R = 3000, formula = (1 - Jabd) ~ ipcc)
smeares1 <- boot(data = as.data.frame(meadata1), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares2 <- boot(data = as.data.frame(meadata2), statistic = bs.sor, R = 2000, formula = (1 - Jabd) ~ ipcc)

# get 95% confidence intervals
###Jaccard Estimators
##Forest
temp1 <- boot.ci(jforres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jforres, type="bca", index=2) # slope
temp3 <- boot.ci(jforres, type="bca", index=3) # r.square
temp4 <- boot.ci(jforres, type="bca", index=4) # mean

forres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jforres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jforres1, type="bca", index=2) # slope
temp3 <- boot.ci(jforres1, type="bca", index=3) # r.square
temp4 <- boot.ci(jforres1, type="bca", index=4) # mean

forres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jforres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jforres2, type="bca", index=2) # slope
temp3 <- boot.ci(jforres2, type="bca", index=3) # r.square
temp4 <- boot.ci(jforres2, type="bca", index=4) # mean

forres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Mixed
temp1 <- boot.ci(jmixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmixres, type="bca", index=2) # slope
temp3 <- boot.ci(jmixres, type="bca", index=3) # r.square
temp4 <- boot.ci(jmixres, type="bca", index=4) # mean

mixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmixres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmixres1, type="bca", index=2) # slope
temp3 <- boot.ci(jmixres1, type="bca", index=3) # r.square
temp4 <- boot.ci(jmixres1, type="bca", index=4) # mean

mixres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmixres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmixres2, type="bca", index=2) # slope
temp3 <- boot.ci(jmixres2, type="bca", index=3) # r.square
temp4 <- boot.ci(jmixres2, type="bca", index=4) # mean

mixres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Meadow
temp1 <- boot.ci(jmeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares, type="bca", index=4) # mean

meares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares1, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares1, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares1, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares1, type="bca", index=4) # mean

meares1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares2, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares2, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares2, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares2, type="bca", index=4) # mean

meares2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
##ALL Years (jac.boot.land.tiff)
#Intercept
mean.ci <- c(meares.ci[1, 1], mixres.ci[1, 1], forres.ci[1, 1])
l.ci <- c(meares.ci[1, 2], mixres.ci[1, 2], forres.ci[1, 2])
s.ci <- c(meares.ci[1, 3], mixres.ci[1, 3], forres.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(meares.ci[4, 1], mixres.ci[4, 1], forres.ci[4, 1])
l.ci <- c(meares.ci[4, 2], mixres.ci[4, 2], forres.ci[4, 2])
s.ci <- c(meares.ci[4, 3], mixres.ci[4, 3], forres.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(meares.ci[2, 1], mixres.ci[2, 1], woores.ci[2, 1])
l.ci <- c(meares.ci[2, 2], mixres.ci[2, 2], woores.ci[2, 2])
s.ci <- c(meares.ci[2, 3], mixres.ci[2, 3], woores.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(meares.ci[3, 1], mixres.ci[3, 1], woores.ci[3, 1])
l.ci <- c(meares.ci[3, 2], mixres.ci[3, 2], woores.ci[3, 2])
s.ci <- c(meares.ci[3, 3], mixres.ci[3, 3], woores.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

##First Year (jac.boot.land1.tiff)
#Intercept
mean.ci <- c(meares1.ci[1, 1], mixres1.ci[1, 1], forres1.ci[1, 1])
l.ci <- c(meares1.ci[1, 2], mixres1.ci[1, 2], forres1.ci[1, 2])
s.ci <- c(meares1.ci[1, 3], mixres1.ci[1, 3], forres1.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("First Year - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(meares1.ci[4, 1], mixres1.ci[4, 1], forres1.ci[4, 1])
l.ci <- c(meares1.ci[4, 2], mixres1.ci[4, 2], forres1.ci[4, 2])
s.ci <- c(meares1.ci[4, 3], mixres1.ci[4, 3], forres1.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(meares1.ci[2, 1], mixres1.ci[2, 1], woores1.ci[2, 1])
l.ci <- c(meares1.ci[2, 2], mixres1.ci[2, 2], woores1.ci[2, 2])
s.ci <- c(meares1.ci[2, 3], mixres1.ci[2, 3], woores1.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(meares1.ci[3, 1], mixres1.ci[3, 1], woores1.ci[3, 1])
l.ci <- c(meares1.ci[3, 2], mixres1.ci[3, 2], woores1.ci[3, 2])
s.ci <- c(meares1.ci[3, 3], mixres1.ci[3, 3], woores1.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

##Second Year (jac.boot.land2.tiff)
#Intercept
mean.ci <- c(meares2.ci[1, 1], mixres2.ci[1, 1], forres2.ci[1, 1])
l.ci <- c(meares2.ci[1, 2], mixres2.ci[1, 2], forres2.ci[1, 2])
s.ci <- c(meares2.ci[1, 3], mixres2.ci[1, 3], forres2.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("Second Year - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(meares2.ci[4, 1], mixres2.ci[4, 1], forres2.ci[4, 1])
l.ci <- c(meares2.ci[4, 2], mixres2.ci[4, 2], forres2.ci[4, 2])
s.ci <- c(meares2.ci[4, 3], mixres2.ci[4, 3], forres2.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(meares2.ci[2, 1], mixres2.ci[2, 1], woores2.ci[2, 1])
l.ci <- c(meares2.ci[2, 2], mixres2.ci[2, 2], woores2.ci[2, 2])
s.ci <- c(meares2.ci[2, 3], mixres2.ci[2, 3], woores2.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(meares2.ci[3, 1], mixres2.ci[3, 1], woores2.ci[3, 1])
l.ci <- c(meares2.ci[3, 2], mixres2.ci[3, 2], woores2.ci[3, 2])
s.ci <- c(meares2.ci[3, 3], mixres2.ci[3, 3], woores2.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

###Sorensen Estimators
##Forest
temp1 <- boot.ci(sforres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sforres, type="bca", index=2) # slope
temp3 <- boot.ci(sforres, type="bca", index=3) # r.square
temp4 <- boot.ci(sforres, type="bca", index=4) # mean

sforres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sforres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(sforres1, type="bca", index=2) # slope
temp3 <- boot.ci(sforres1, type="bca", index=3) # r.square
temp4 <- boot.ci(sforres1, type="bca", index=4) # mean

sforres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sforres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(sforres2, type="bca", index=2) # slope
temp3 <- boot.ci(sforres2, type="bca", index=3) # r.square
temp4 <- boot.ci(sforres2, type="bca", index=4) # mean

sforres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Mixed
temp1 <- boot.ci(smixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(smixres, type="bca", index=2) # slope
temp3 <- boot.ci(smixres, type="bca", index=3) # r.square
temp4 <- boot.ci(smixres, type="bca", index=4) # mean

smixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smixres1, type="bca", index=1) # intercept 
temp2 <- boot.ci(smixres1, type="bca", index=2) # slope
temp3 <- boot.ci(smixres1, type="bca", index=3) # r.square
temp4 <- boot.ci(smixres1, type="bca", index=4) # mean

smixres1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smixres2, type="bca", index=1) # intercept 
temp2 <- boot.ci(smixres2, type="bca", index=2) # slope
temp3 <- boot.ci(smixres2, type="bca", index=3) # r.square
temp4 <- boot.ci(smixres2, type="bca", index=4) # mean

smixres2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Meadow
temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares1, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares1, type="bca", index=2) # slope
temp3 <- boot.ci(smeares1, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares1, type="bca", index=4) # mean

smeares1.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares2, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares2, type="bca", index=2) # slope
temp3 <- boot.ci(smeares2, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares2, type="bca", index=4) # mean

smeares2.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

##Sorensen
par(mfrow = c(2, 2))
##ALL Years (sor.boot.land.tiff)
#Intercept
mean.ci <- c(smeares.ci[1, 1], smixres.ci[1, 1], sforres.ci[1, 1])
l.ci <- c(smeares.ci[1, 2], smixres.ci[1, 2], sforres.ci[1, 2])
s.ci <- c(smeares.ci[1, 3], smixres.ci[1, 3], sforres.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(smeares.ci[4, 1], smixres.ci[4, 1], sforres.ci[4, 1])
l.ci <- c(smeares.ci[4, 2], smixres.ci[4, 2], sforres.ci[4, 2])
s.ci <- c(smeares.ci[4, 3], smixres.ci[4, 3], sforres.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(smeares.ci[2, 1], smixres.ci[2, 1], swoores.ci[2, 1])
l.ci <- c(smeares.ci[2, 2], smixres.ci[2, 2], swoores.ci[2, 2])
s.ci <- c(smeares.ci[2, 3], smixres.ci[2, 3], swoores.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(smeares.ci[3, 1], smixres.ci[3, 1], swoores.ci[3, 1])
l.ci <- c(smeares.ci[3, 2], smixres.ci[3, 2], swoores.ci[3, 2])
s.ci <- c(smeares.ci[3, 3], smixres.ci[3, 3], swoores.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

##First Year (sor.boot.land1.tiff)
#Intercept
mean.ci <- c(smeares1.ci[1, 1], smixres1.ci[1, 1], sforres1.ci[1, 1])
l.ci <- c(smeares1.ci[1, 2], smixres1.ci[1, 2], sforres1.ci[1, 2])
s.ci <- c(smeares1.ci[1, 3], smixres1.ci[1, 3], sforres1.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("First Year - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(smeares1.ci[4, 1], smixres1.ci[4, 1], sforres1.ci[4, 1])
l.ci <- c(smeares1.ci[4, 2], smixres1.ci[4, 2], sforres1.ci[4, 2])
s.ci <- c(smeares1.ci[4, 3], smixres1.ci[4, 3], sforres1.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(smeares1.ci[2, 1], smixres1.ci[2, 1], swoores1.ci[2, 1])
l.ci <- c(smeares1.ci[2, 2], smixres1.ci[2, 2], swoores1.ci[2, 2])
s.ci <- c(smeares1.ci[2, 3], smixres1.ci[2, 3], swoores1.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(smeares1.ci[3, 1], smixres1.ci[3, 1], swoores1.ci[3, 1])
l.ci <- c(smeares1.ci[3, 2], smixres1.ci[3, 2], swoores1.ci[3, 2])
s.ci <- c(smeares1.ci[3, 3], smixres1.ci[3, 3], swoores1.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

##Second Year (sor.boot.land2.tiff)
#Intercept
mean.ci <- c(smeares2.ci[1, 1], smixres2.ci[1, 1], sforres2.ci[1, 1])
l.ci <- c(smeares2.ci[1, 2], smixres2.ci[1, 2], sforres2.ci[1, 2])
s.ci <- c(smeares2.ci[1, 3], smixres2.ci[1, 3], sforres2.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("Second Year - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(smeares2.ci[4, 1], smixres2.ci[4, 1], sforres2.ci[4, 1])
l.ci <- c(smeares2.ci[4, 2], smixres2.ci[4, 2], sforres2.ci[4, 2])
s.ci <- c(smeares2.ci[4, 3], smixres2.ci[4, 3], sforres2.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim = c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(smeares2.ci[2, 1], smixres2.ci[2, 1], swoores2.ci[2, 1])
l.ci <- c(smeares2.ci[2, 2], smixres2.ci[2, 2], swoores2.ci[2, 2])
s.ci <- c(smeares2.ci[2, 3], smixres2.ci[2, 3], swoores2.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(smeares2.ci[3, 1], smixres2.ci[3, 1], swoores2.ci[3, 1])
l.ci <- c(smeares2.ci[3, 2], smixres2.ci[3, 2], swoores2.ci[3, 2])
s.ci <- c(smeares2.ci[3, 3], smixres2.ci[3, 3], swoores2.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Agri", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")
