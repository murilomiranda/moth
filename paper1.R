####DATA#######
distanc <- function(base){
  DIST <- matrix(0, nrow = 3486, ncol = 3)
  row <- 1
  for(i in 1:83){
    for(j in (i+1):84){
      DIST[row, 1] <- i
      DIST[row, 2] <- j
      DIST[row, 3] <- base[i, j]
      row <- row + 1
    }
  }
  DIST
}

#IPCC####
dist <- read.csv("dist_mat_ipcc.csv", h = T)
dist <- dist[-1]
DIST1 <- distanc(dist)
head(DIST1)

#IGEOE####
dist <- read.csv("dist_mat_igeoe.csv", h = T)
dist <- dist[-1]
DIST2 <- distanc(dist)
head(DIST2)

par(mfrow=c(2, 1))
hist(DIST1[, 3], main = "IPCC", xlab = "Geographic distance (m)", ylab = "Frequency")
hist(DIST2[, 3], main = "IGEOE", xlab = "Geographic distance (m)", ylab = "Frequency")

#Read dataset with id, site, period, species, abund, landscape, biotope####
data <- read.csv("data.csv", h = T)
head(data)

source('chao.R')
sample1 <- chao(1, 1, "sample1")
comment(sample1) <- "Sample1"
sample2 <- chao(2, 2, "sample2")
comment(sample2) <- "Sample2"
sample3 <- chao(3, 3, "sample3")
comment(sample3) <- "Sample3"
sample4 <- chao(4, 4, "sample4")
comment(sample4) <- "Sample4"
sample5 <- chao(5, 5, "sample5")
comment(sample5) <- "Sample5"
sample6 <- chao(6, 6, "sample6")
comment(sample6) <- "Sample6"

trisamples <- function(base1, base2, base3, vr){
  base <- matrix(0, nrow = dim(base1)[1])
  for(i in 1:dim(base1)[1]){
    base[i] <- (base1[i, vr] + base2[i, vr] + base3[i, vr])/3
  }
  base
}

bisamples <- function(base1, base2){
  base <- matrix(0, nrow = length(base1))
  for(i in 1:length(base1)){
    base[i] <- (base1[i] + base2[i])/2
  }
  base
}

## For first year
Jperiod1 <- trisamples(sample1, sample2, sample3, 19)
comment(Jperiod1) <- "Year 1 - Jaccard"
Lperiod1 <- trisamples(sample1, sample2, sample3, 20)
comment(Lperiod1) <- "Year 1 - Sorensen"

## For second year
Jperiod2 <- trisamples(sample4, sample5, sample6, 19)
comment(Jperiod2) <- "Year 2 - Jaccard"
Lperiod2 <- trisamples(sample4, sample5, sample6, 20)
comment(Lperiod2) <- "Year 2 - Sorensen"

## For all traps
Jperiod <- bisamples(Jperiod1, Jperiod2)
comment(Jperiod) <- "All - Jaccard"
Lperiod <- bisamples(Lperiod1, Lperiod2)
comment(Lperiod) <- "All - Sorensen"

#Figure - all_points.tiff
par(mfrow=c(3, 2))
plot(DIST1[, 3], (1 - Jperiod1), ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard\nFirst Year")
plot(DIST1[, 3], (1 - Lperiod1), ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen\nFirst Year")
plot(DIST1[, 3], (1 - Jperiod2), ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Second Year")
plot(DIST1[, 3], (1 - Lperiod2), ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Second Year")
plot(DIST1[, 3], (1 - Jperiod), ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = " all years")
plot(DIST1[, 3], (1 - Lperiod), ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "all years")

#BIG-DATASET####
b.div.mean <- cbind(sample1[, 1], sample1[, 2], sample1[, 3], Jperiod, Lperiod, DIST1[, 3], DIST2[, 3])
colnames(b.div.mean) <- c("p", "s1", "s2", "Jabd", "Labd", "ipcc", "igeoe")
head(b.div.mean)

b.div <- rbind(cbind(sample1[, c(1, 2, 3, 19, 20)], DIST1[, 3], DIST2[, 3]), cbind(sample2[, c(1, 2, 3, 19, 20)], DIST1[, 3], DIST2[, 3]), 
               cbind(sample3[, c(1, 2, 3, 19, 20)], DIST1[, 3], DIST2[, 3]), cbind(sample4[, c(1, 2, 3, 19, 20)], DIST1[, 3], DIST2[, 3]), 
               cbind(sample5[, c(1, 2, 3, 19, 20)], DIST1[, 3], DIST2[, 3]), cbind(sample6[, c(1, 2, 3, 19, 20)], DIST1[, 3], DIST2[, 3]))
colnames(b.div) <- c("p", "s1", "s2", "Jabd", "Labd", "ipcc", "igeoe")
head(b.div)

###WITHIN LANDSCAPE#####
landsc_within <- function(base, s1, s2){
  result <- NULL
  for(i in s1){
    for(j in s2){
      if(i != j){
        result <- rbind(result, base[base[, 2] == i & base[, 3] == j, ])
      }
    }
  }
  result
}

idmea <- 1:28
idmix <- 57:84
idfor <- 29:56

###ALL YEARS####
m.meadata <- landsc_within(b.div.mean, idmea, idmea); comment(m.meadata) <- "MEADOW - MEAN"
m.mixdata <- landsc_within(b.div.mean, idmix, idmix); comment(m.mixdata) <- "MIXED - MEAN"
m.fordata <- landsc_within(b.div.mean, idfor, idfor); comment(m.fordata) <- "FOREST - MEAN"

meadata <- landsc_within(b.div, idmea, idmea); comment(meadata) <- "MEADOW - ALL POINTS"
mixdata <- landsc_within(b.div, idmix, idmix); comment(mixdata) <- "MIXED - ALL POINTS"
fordata <- landsc_within(b.div, idfor, idfor); comment(fordata) <- "FOREST - ALL POINTS"

# Bootstrap 95% CI for regression coefficients 
library(boot)
# function to obtain regression weights 
bs.jac <- function(formula, data, ind){
  d <- data[ind,] # allows boot to select sample
  fit <- lm(formula, data = d)
  return(c(coef(fit), summary(fit)$r.square, mean(d[, 4]))) 
} 

bs.sor <- function(formula, data, ind){
  d <- data[ind,] # allows boot to select sample
  fit <- lm(formula, data = d)
  return(c(coef(fit), summary(fit)$r.square, mean(d[, 5]))) 
} 

###MEAN POINTS #####
#Jaccard Estimator
jm.modelfor <- lm((1- m.fordata[, 4]) ~ m.fordata[, 6])
jm.modelmix <- lm((1- m.mixdata[, 4]) ~ m.mixdata[, 6])
jm.modelmea <- lm((1- m.meadata[, 4]) ~ m.meadata[, 6])

#Sorensen Estimator
sm.modelfor <- lm((1- m.fordata[, 5]) ~ m.fordata[, 6])
sm.modelmix <- lm((1- m.mixdata[, 5]) ~ m.mixdata[, 6])
sm.modelmea <- lm((1- m.meadata[, 5]) ~ m.meadata[, 6])

par(mfrow=c(3, 2)) #Figure - eachlandscape_mean.tiff
plot(m.fordata[, 6], 1 - m.fordata[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jm.modelfor)[[1]], b = coef(jm.modelfor)[[2]], col = 2, lwd = 4.5)
plot(m.fordata[, 6], 1 - m.fordata[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(sm.modelfor)[[1]], b = coef(sm.modelfor)[[2]], col = 2, lwd = 4.5)

plot(m.mixdata[, 6], 1 - m.mixdata[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jm.modelmix)[[1]], b = coef(jm.modelmix)[[2]], col = 2, lwd = 4.5)
plot(m.mixdata[, 6], 1 - m.mixdata[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(sm.modelmix)[[1]], b = coef(sm.modelmix)[[2]], col = 2, lwd = 4.5)

plot(m.meadata[, 6], 1 - m.meadata[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jm.modelmea)[[1]], b = coef(jm.modelmea)[[2]], col = 2, lwd = 4.5)
plot(m.meadata[, 6], 1 - m.meadata[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(sm.modelmea)[[1]], b = coef(sm.modelmea)[[2]], col = 2, lwd = 4.5)

# bootstrapping with 2000 replications
jforres <- boot(data = as.data.frame(m.fordata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
sforres <- boot(data = as.data.frame(m.fordata), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmixres <- boot(data = as.data.frame(m.mixdata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smixres <- boot(data = as.data.frame(m.mixdata), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmeares <- boot(data = as.data.frame(m.meadata), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares <- boot(data = as.data.frame(m.meadata), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

#get 95% confidence intervals
#Jaccard Estimator
temp1 <- boot.ci(jforres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jforres, type="bca", index=2) # slope
temp3 <- boot.ci(jforres, type="bca", index=3) # r.square
temp4 <- boot.ci(jforres, type="bca", index=4) # mean

jforres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmixres, type="bca", index=2) # slope
temp3 <- boot.ci(jmixres, type="bca", index=3) # r.square
temp4 <- boot.ci(jmixres, type="bca", index=4) # mean

jmixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares, type="bca", index=4) # mean

jmeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

#Sorensen Estimator
temp1 <- boot.ci(sforres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sforres, type="bca", index=2) # slope
temp3 <- boot.ci(sforres, type="bca", index=3) # r.square
temp4 <- boot.ci(sforres, type="bca", index=4) # mean

sforres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(smixres, type="bca", index=2) # slope
temp3 <- boot.ci(smixres, type="bca", index=3) # r.square
temp4 <- boot.ci(smixres, type="bca", index=4) # mean

smixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
#Jaccard(pap_jac.boot.tiff)
#Intercept
mean.ci <- c(jforres.ci[1, 1], jmixres.ci[1, 1], jmeares.ci[1, 1])
l.ci <- c(jforres.ci[1, 2], jmixres.ci[1, 2], jmeares.ci[1, 2])
s.ci <- c(jforres.ci[1, 3], jmixres.ci[1, 3], jmeares.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(jforres.ci[4, 1], jmixres.ci[4, 1], jmeares.ci[4, 1])
l.ci <- c(jforres.ci[4, 2], jmixres.ci[4, 2], jmeares.ci[4, 2])
s.ci <- c(jforres.ci[4, 3], jmixres.ci[4, 3], jmeares.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(jforres.ci[2, 1], jmixres.ci[2, 1], jmeares.ci[2, 1])
l.ci <- c(jforres.ci[2, 2], jmixres.ci[2, 2], jmeares.ci[2, 2])
s.ci <- c(jforres.ci[2, 3], jmixres.ci[2, 3], jmeares.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(jforres.ci[3, 1], jmixres.ci[3, 1], jmeares.ci[3, 1])
l.ci <- c(jforres.ci[3, 2], jmixres.ci[3, 2], jmeares.ci[3, 2])
s.ci <- c(jforres.ci[3, 3], jmixres.ci[3, 3], jmeares.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

par(mfrow = c(2, 2))
#Sorensen(pap_sor.boot.tiff)
#Intercept
mean.ci <- c(sforres.ci[1, 1], smixres.ci[1, 1], smeares.ci[1, 1])
l.ci <- c(sforres.ci[1, 2], smixres.ci[1, 2], smeares.ci[1, 2])
s.ci <- c(sforres.ci[1, 3], smixres.ci[1, 3], smeares.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(sforres.ci[4, 1], smixres.ci[4, 1], smeares.ci[4, 1])
l.ci <- c(sforres.ci[4, 2], smixres.ci[4, 2], smeares.ci[4, 2])
s.ci <- c(sforres.ci[4, 3], smixres.ci[4, 3], smeares.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(sforres.ci[2, 1], smixres.ci[2, 1], smeares.ci[2, 1])
l.ci <- c(sforres.ci[2, 2], smixres.ci[2, 2], smeares.ci[2, 2])
s.ci <- c(sforres.ci[2, 3], smixres.ci[2, 3], smeares.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(sforres.ci[3, 1], smixres.ci[3, 1], smeares.ci[3, 1])
l.ci <- c(sforres.ci[3, 2], smixres.ci[3, 2], smeares.ci[3, 2])
s.ci <- c(sforres.ci[3, 3], smixres.ci[3, 3], smeares.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")


### ALL POINTS #####
#Jaccard Estimator
j.modelfor <- lm((1- fordata[, 4]) ~ fordata[, 6])
j.modelmix <- lm((1- mixdata[, 4]) ~ mixdata[, 6])
j.modelmea <- lm((1- meadata[, 4]) ~ meadata[, 6])

#Sorensen Estimator
s.modelfor <- lm((1- fordata[, 5]) ~ fordata[, 6])
s.modelmix <- lm((1- mixdata[, 5]) ~ mixdata[, 6])
s.modelmea <- lm((1- meadata[, 5]) ~ meadata[, 6])

par(mfrow=c(3, 2)) #Figure - eachlandscape_all.tiff
plot(fordata[, 6], 1 - fordata[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(j.modelfor)[[1]], b = coef(j.modelfor)[[2]], col = 2, lwd = 4.5)
plot(fordata[, 6], 1 - fordata[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(s.modelfor)[[1]], b = coef(s.modelfor)[[2]], col = 2, lwd = 4.5)

plot(mixdata[, 6], 1 - mixdata[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(j.modelmix)[[1]], b = coef(j.modelmix)[[2]], col = 2, lwd = 4.5)
plot(mixdata[, 6], 1 - mixdata[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(s.modelmix)[[1]], b = coef(s.modelmix)[[2]], col = 2, lwd = 4.5)

plot(meadata[, 6], 1 - meadata[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(j.modelmea)[[1]], b = coef(j.modelmea)[[2]], col = 2, lwd = 4.5)
plot(meadata[, 6], 1 - meadata[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(s.modelmea)[[1]], b = coef(s.modelmea)[[2]], col = 2, lwd = 4.5)

# bootstrapping with 3000 replications
jforres <- boot(data = as.data.frame(fordata), statistic = bs.jac, R = 3000, formula = (1 - Jabd) ~ ipcc)
sforres <- boot(data = as.data.frame(fordata), statistic = bs.sor, R = 3000, formula = (1 - Labd) ~ ipcc)

jmixres <- boot(data = as.data.frame(mixdata), statistic = bs.jac, R = 3000, formula = (1 - Jabd) ~ ipcc)
smixres <- boot(data = as.data.frame(mixdata), statistic = bs.sor, R = 3000, formula = (1 - Labd) ~ ipcc)

jmeares <- boot(data = as.data.frame(meadata), statistic = bs.jac, R = 3000, formula = (1 - Jabd) ~ ipcc)
smeares <- boot(data = as.data.frame(meadata), statistic = bs.sor, R = 3000, formula = (1 - Labd) ~ ipcc)

#get 95% confidence intervals
#Jaccard Estimator
temp1 <- boot.ci(jforres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jforres, type="bca", index=2) # slope
temp3 <- boot.ci(jforres, type="bca", index=3) # r.square
temp4 <- boot.ci(jforres, type="bca", index=4) # mean

jforres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmixres, type="bca", index=2) # slope
temp3 <- boot.ci(jmixres, type="bca", index=3) # r.square
temp4 <- boot.ci(jmixres, type="bca", index=4) # mean

jmixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares, type="bca", index=4) # mean

jmeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

#Sorensen Estimator
temp1 <- boot.ci(sforres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sforres, type="bca", index=2) # slope
temp3 <- boot.ci(sforres, type="bca", index=3) # r.square
temp4 <- boot.ci(sforres, type="bca", index=4) # mean

sforres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(smixres, type="bca", index=2) # slope
temp3 <- boot.ci(smixres, type="bca", index=3) # r.square
temp4 <- boot.ci(smixres, type="bca", index=4) # mean

smixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
#Jaccard(pap_jac.boot.all.tiff)
#Intercept
mean.ci <- c(jforres.ci[1, 1], jmixres.ci[1, 1], jmeares.ci[1, 1])
l.ci <- c(jforres.ci[1, 2], jmixres.ci[1, 2], jmeares.ci[1, 2])
s.ci <- c(jforres.ci[1, 3], jmixres.ci[1, 3], jmeares.ci[1, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(jforres.ci[4, 1], jmixres.ci[4, 1], jmeares.ci[4, 1])
l.ci <- c(jforres.ci[4, 2], jmixres.ci[4, 2], jmeares.ci[4, 2])
s.ci <- c(jforres.ci[4, 3], jmixres.ci[4, 3], jmeares.ci[4, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(jforres.ci[2, 1], jmixres.ci[2, 1], jmeares.ci[2, 1])
l.ci <- c(jforres.ci[2, 2], jmixres.ci[2, 2], jmeares.ci[2, 2])
s.ci <- c(jforres.ci[2, 3], jmixres.ci[2, 3], jmeares.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(jforres.ci[3, 1], jmixres.ci[3, 1], jmeares.ci[3, 1])
l.ci <- c(jforres.ci[3, 2], jmixres.ci[3, 2], jmeares.ci[3, 2])
s.ci <- c(jforres.ci[3, 3], jmixres.ci[3, 3], jmeares.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

par(mfrow = c(2, 2))
#Sorensen(pap_sor.boot.all.tiff)
#Intercept
mean.ci <- c(sforres.ci[1, 1], smixres.ci[1, 1], smeares.ci[1, 1])
l.ci <- c(sforres.ci[1, 2], smixres.ci[1, 2], smeares.ci[1, 2])
s.ci <- c(sforres.ci[1, 3], smixres.ci[1, 3], smeares.ci[1, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(sforres.ci[4, 1], smixres.ci[4, 1], smeares.ci[4, 1])
l.ci <- c(sforres.ci[4, 2], smixres.ci[4, 2], smeares.ci[4, 2])
s.ci <- c(sforres.ci[4, 3], smixres.ci[4, 3], smeares.ci[4, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(sforres.ci[2, 1], smixres.ci[2, 1], smeares.ci[2, 1])
l.ci <- c(sforres.ci[2, 2], smixres.ci[2, 2], smeares.ci[2, 2])
s.ci <- c(sforres.ci[2, 3], smixres.ci[2, 3], smeares.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(sforres.ci[3, 1], smixres.ci[3, 1], smeares.ci[3, 1])
l.ci <- c(sforres.ci[3, 2], smixres.ci[3, 2], smeares.ci[3, 2])
s.ci <- c(sforres.ci[3, 3], smixres.ci[3, 3], smeares.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

