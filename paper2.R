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

#For diferent landscapes and biotopes####
#forest-dominated
mf <- c(56) #m - meadow
sf <- c(36, 39, 42, 45, 46) #s - short shrub
tf <- c(34, 49) #t - tall shrub
wf <- c(29, 30, 31, 32, 33, 35, 37, 38, 40, 41, 43, 44, 47, 48, 50, 51, 52, 53, 54, 55) #w - woodland
#shrub-dominated (m - mixed)
mm <- c(68, 75, 77, 84)
sm <- c(60, 64, 65, 66, 67, 70, 78, 79, 82)
tm <- c(57, 58, 59, 61, 62, 63, 69, 71, 72, 74, 76)
wm <- c(73, 80, 81, 83)
#meadow-dominated (a - agriculture)
ma <- c(1, 9, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25, 27)
sa <- c(2, 8, 10, 11, 12, 13, 20)
ta <- c(3, 4, 5, 6, 7, 28)
wa <- c(22, 26)

#within landscapes####
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

#ALL YEARS
agr.mea <- landsc_within(b.div.mean, ma, ma)
shr.shr <- landsc_within(b.div.mean, sm, sm)
tal.shr <- landsc_within(b.div.mean, tm, tm)
for.woo <- landsc_within(b.div.mean, wf, wf)
mix.shr <- landsc_within(b.div.mean, c(sm, tm), c(sm, tm))

#DISSIMILARITY (dissimilaritywithin_all.tiff)
par(mfrow=c(2, 2))
m.data <- c(mean(for.woo[, 4]), mean(mix.shr[, 4]), mean(agr.mea[, 4]))
m.li <- c(mean(for.woo[, 4]) - sd(for.woo[, 4]), mean(mix.shr[, 4]) - sd(mix.shr[, 4]), mean(agr.mea[, 4]) - sd(agr.mea[, 4]))
m.lu <- c(mean(for.woo[, 4]) + sd(for.woo[, 4]), mean(mix.shr[, 4]) + sd(mix.shr[, 4]), mean(agr.mea[, 4]) + sd(agr.mea[, 4]))
plot(1:3, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Forest", "Mixed", "Agric"))
segments(x0 = 1:3, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c( mean(for.woo[, 5]), mean(mix.shr[, 5]), mean(agr.mea[, 5]))
m.li <- c(mean(for.woo[, 5]) - sd(for.woo[, 5]), mean(mix.shr[, 5]) - sd(mix.shr[, 5]), mean(agr.mea[, 5]) - sd(agr.mea[, 5]))
m.lu <- c(mean(for.woo[, 5]) + sd(for.woo[, 5]), mean(mix.shr[, 5]) + sd(mix.shr[, 5]), mean(agr.mea[, 5]) + sd(agr.mea[, 5]))
plot(1:3, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Forest", "Mixed", "Agric"))
segments(x0 = 1:3, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

##with all biotopes
m.data <- c(mean(for.woo[, 4]), mean(tal.shr[, 4]), mean(shr.shr[, 4]), mean(agr.mea[, 4]))
m.li <- c(mean(for.woo[, 4]) - sd(for.woo[, 4]), mean(tal.shr[, 4]) - sd(tal.shr[, 4]), mean(shr.shr[, 4]) - sd(shr.shr[, 4]), mean(agr.mea[, 4]) - sd(agr.mea[, 4]))
m.lu <- c(mean(for.woo[, 4]) + sd(for.woo[, 4]), mean(tal.shr[, 4]) + sd(tal.shr[, 4]), mean(shr.shr[, 4]) + sd(shr.shr[, 4]), mean(agr.mea[, 4]) + sd(agr.mea[, 4]))
plot(1:4, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c(mean(for.woo[, 5]), mean(tal.shr[, 5]), mean(shr.shr[, 5]), mean(agr.mea[, 5]))
m.li <- c(mean(for.woo[, 5]) - sd(for.woo[, 5]), mean(tal.shr[, 5]) - sd(tal.shr[, 5]), mean(shr.shr[, 5]) - sd(shr.shr[, 5]), mean(agr.mea[, 5]) - sd(agr.mea[, 5]))
m.lu <- c(mean(for.woo[, 5]) + sd(for.woo[, 5]), mean(tal.shr[, 5]) + sd(tal.shr[, 5]), mean(shr.shr[, 5]) + sd(shr.shr[, 5]), mean(agr.mea[, 5]) + sd(agr.mea[, 5]))
plot(1:4, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

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

#Jaccard Estimator
jmodelfor.woo <- lm((1 - for.woo[, 4]) ~ for.woo[, 6])
jmodelmix.shr <- lm((1 - mix.shr[, 4]) ~ mix.shr[, 6])
jmodelagr.mea <- lm((1 - agr.mea[, 4]) ~ agr.mea[, 6])
jmodelshr.shr <- lm((1 - shr.shr[, 4]) ~ shr.shr[, 6])
jmodeltal.shr <- lm((1 - tal.shr[, 4]) ~ tal.shr[, 6])

#Sorensen Estimator
smodelfor.woo <- lm((1 - for.woo[, 5]) ~ for.woo[, 6])
smodelmix.shr <- lm((1 - mix.shr[, 5]) ~ mix.shr[, 6])
smodelagr.mea <- lm((1 - agr.mea[, 5]) ~ agr.mea[, 6])
smodelshr.shr <- lm((1 - shr.shr[, 5]) ~ shr.shr[, 6])
smodeltal.shr <- lm((1 - tal.shr[, 5]) ~ tal.shr[, 6])

par(mfrow=c(3, 2)) #Figure - eachlandscape_mean3.tiff
plot(for.woo[, 6], 1 - for.woo[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jmodelfor.woo)[[1]], b = coef(jmodelfor.woo)[[2]], col = 2, lwd = 4.5)
plot(for.woo[, 6], 1 - for.woo[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(smodelfor.woo)[[1]], b = coef(smodelfor.woo)[[2]], col = 2, lwd = 4.5)

plot(mix.shr[, 6], 1 - mix.shr[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodelmix.shr)[[1]], b = coef(jmodelmix.shr)[[2]], col = 2, lwd = 4.5)
plot(mix.shr[, 6], 1 - mix.shr[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodelmix.shr)[[1]], b = coef(smodelmix.shr)[[2]], col = 2, lwd = 4.5)

plot(agr.mea[, 6], 1 - agr.mea[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jmodelagr.mea)[[1]], b = coef(jmodelagr.mea)[[2]], col = 2, lwd = 4.5)
plot(agr.mea[, 6], 1 - agr.mea[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(smodelagr.mea)[[1]], b = coef(smodelagr.mea)[[2]], col = 2, lwd = 4.5)

par(mfrow=c(4, 2)) #Figure - eachlandscape_mean4.tiff
plot(for.woo[, 6], 1 - for.woo[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jmodelfor.woo)[[1]], b = coef(jmodelfor.woo)[[2]], col = 2, lwd = 4.5)
plot(for.woo[, 6], 1 - for.woo[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(smodelfor.woo)[[1]], b = coef(smodelfor.woo)[[2]], col = 2, lwd = 4.5)

plot(tal.shr[, 6], 1 - tal.shr[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodeltal.shr)[[1]], b = coef(jmodeltal.shr)[[2]], col = 2, lwd = 4.5)
plot(tal.shr[, 6], 1 - tal.shr[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodeltal.shr)[[1]], b = coef(smodeltal.shr)[[2]], col = 2, lwd = 4.5)

plot(shr.shr[, 6], 1 - shr.shr[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodelshr.shr)[[1]], b = coef(jmodelshr.shr)[[2]], col = 2, lwd = 4.5)
plot(shr.shr[, 6], 1 - shr.shr[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodelshr.shr)[[1]], b = coef(smodelshr.shr)[[2]], col = 2, lwd = 4.5)

plot(agr.mea[, 6], 1 - agr.mea[, 4], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jmodelagr.mea)[[1]], b = coef(jmodelagr.mea)[[2]], col = 2, lwd = 4.5)
plot(agr.mea[, 6], 1 - agr.mea[, 5], ylim = c(0, 1), xlim = c(0, 2000), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(smodelagr.mea)[[1]], b = coef(smodelagr.mea)[[2]], col = 2, lwd = 4.5)

# bootstrapping with 2000 replications
jforres <- boot(data = as.data.frame(for.woo), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
sforres <- boot(data = as.data.frame(for.woo), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmixres <- boot(data = as.data.frame(mix.shr), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smixres <- boot(data = as.data.frame(mix.shr), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jtalres <- boot(data = as.data.frame(tal.shr), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
stalres <- boot(data = as.data.frame(tal.shr), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jshrres <- boot(data = as.data.frame(shr.shr), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
sshrres <- boot(data = as.data.frame(shr.shr), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmeares <- boot(data = as.data.frame(agr.mea), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares <- boot(data = as.data.frame(agr.mea), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

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

temp1 <- boot.ci(jtalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jtalres, type="bca", index=2) # slope
temp3 <- boot.ci(jtalres, type="bca", index=3) # r.square
temp4 <- boot.ci(jtalres, type="bca", index=4) # mean

jtalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jshrres, type="bca", index=2) # slope
temp3 <- boot.ci(jshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(jshrres, type="bca", index=4) # mean

jshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

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

temp1 <- boot.ci(stalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(stalres, type="bca", index=2) # slope
temp3 <- boot.ci(stalres, type="bca", index=3) # r.square
temp4 <- boot.ci(stalres, type="bca", index=4) # mean

stalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sshrres, type="bca", index=2) # slope
temp3 <- boot.ci(sshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(sshrres, type="bca", index=4) # mean

sshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
#Jaccard(jac.boot_mean3.tiff)
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
#Sorensen(sor.boot_mean3.tiff)
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

#Jaccard(jac.boot_mean4.tiff)
#Intercept
mean.ci <- c(jforres.ci[1, 1], jtalres.ci[1, 1], jshrres.ci[1, 1], jmeares.ci[1, 1])
l.ci <- c(jforres.ci[1, 2], jtalres.ci[1, 2], jshrres.ci[1, 2], jmeares.ci[1, 2])
s.ci <- c(jforres.ci[1, 3], jtalres.ci[1, 3], jshrres.ci[1, 3], jmeares.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(jforres.ci[4, 1], jtalres.ci[4, 1], jshrres.ci[4, 1], jmeares.ci[4, 1])
l.ci <- c(jforres.ci[4, 2], jtalres.ci[4, 2], jshrres.ci[4, 2], jmeares.ci[4, 2])
s.ci <- c(jforres.ci[4, 3], jtalres.ci[4, 3], jshrres.ci[4, 3], jmeares.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(jforres.ci[2, 1], jtalres.ci[2, 1], jshrres.ci[2, 1], jmeares.ci[2, 1])
l.ci <- c(jforres.ci[2, 2], jtalres.ci[2, 2], jshrres.ci[2, 2], jmeares.ci[2, 2])
s.ci <- c(jforres.ci[2, 3], jtalres.ci[2, 3], jshrres.ci[2, 3], jmeares.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(jforres.ci[3, 1], jtalres.ci[3, 1], jshrres.ci[3, 1], jmeares.ci[3, 1])
l.ci <- c(jforres.ci[3, 2], jtalres.ci[3, 2], jshrres.ci[3, 2], jmeares.ci[3, 2])
s.ci <- c(jforres.ci[3, 3], jtalres.ci[3, 3], jshrres.ci[3, 3], jmeares.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

par(mfrow = c(2, 2))
#Sorensen(sor.boot_mean4.tiff)
#Intercept
mean.ci <- c(sforres.ci[1, 1], stalres.ci[1, 1], sshrres.ci[1, 1], smeares.ci[1, 1])
l.ci <- c(sforres.ci[1, 2], stalres.ci[1, 2], sshrres.ci[1, 2], smeares.ci[1, 2])
s.ci <- c(sforres.ci[1, 3], stalres.ci[1, 3], sshrres.ci[1, 3], smeares.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(sforres.ci[4, 1], stalres.ci[4, 1], sshrres.ci[4, 1], smeares.ci[4, 1])
l.ci <- c(sforres.ci[4, 2], stalres.ci[4, 2], sshrres.ci[4, 2], smeares.ci[4, 2])
s.ci <- c(sforres.ci[4, 3], stalres.ci[4, 3], sshrres.ci[4, 3], smeares.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(sforres.ci[2, 1], stalres.ci[2, 1], sshrres.ci[2, 1], smeares.ci[2, 1])
l.ci <- c(sforres.ci[2, 2], stalres.ci[2, 2], sshrres.ci[2, 2], smeares.ci[2, 2])
s.ci <- c(sforres.ci[2, 3], stalres.ci[2, 3], sshrres.ci[2, 3], smeares.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(sforres.ci[3, 1], stalres.ci[3, 1], sshrres.ci[3, 1], smeares.ci[3, 1])
l.ci <- c(sforres.ci[3, 2], stalres.ci[3, 2], sshrres.ci[3, 2], smeares.ci[3, 2])
s.ci <- c(sforres.ci[3, 3], stalres.ci[3, 3], sshrres.ci[3, 3], smeares.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

##between landscapes#####
#between landscapes####
landsc_between <- function(base, s1, s2){
  result <- NULL
  for(i in s1){
    for(j in s2){
      result <- rbind(result, base[(base[, 2] == i & base[, 3] == j) | (base[, 2] == j & base[, 3] == i), ])
    }
  }
  result
}

#ALL YEARS
mea <- landsc_between(b.div.mean, ma, c(mm, mf))
shr <- landsc_between(b.div.mean, sm, c(sa, sf))
tal <- landsc_between(b.div.mean, tm, c(ta, tf))
woo <- landsc_between(b.div.mean, wf, c(wa, wm))
mix <- rbind(shr, tal)

#DISSIMILARITY (dissimilaritybetween_all.tiff)
par(mfrow=c(2, 2))
#ALL YEARS
m.data <- c(mean(woo[, 4]), mean(mix[, 4]), mean(mea[, 4]) )
m.li <- c(mean(woo[, 4]) - sd(woo[, 4]), mean(mix[, 4]) - sd(mix[, 4]), mean(mea[, 4]) - sd(mea[, 4]))
m.lu <- c(mean(woo[, 4]) + sd(woo[, 4]), mean(mix[, 4]) + sd(mix[, 4]), mean(mea[, 4]) + sd(mea[, 4]))
plot(1:3, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Forest", "Mixed", "Agric"))
segments(x0 = 1:3, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c(mean(woo[, 5]), mean(mix[, 5]), mean(mea[, 5]) )
m.li <- c(mean(woo[, 5]) - sd(woo[, 5]), mean(mix[, 5]) - sd(mix[, 5]), mean(mea[, 5]) - sd(mea[, 5]))
m.lu <- c(mean(woo[, 5]) + sd(woo[, 5]), mean(mix[, 5]) + sd(mix[, 5]), mean(mea[, 5]) + sd(mea[, 5]))
plot(1:3, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Forest", "Mixed", "Agric"))
segments(x0 = 1:3, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c(mean(woo[, 4]), mean(tal[, 4]), mean(shr[, 4]), mean(mea[, 4]) )
m.li <- c(mean(woo[, 4]) - sd(woo[, 4]), mean(tal[, 4]) - sd(tal[, 4]), mean(shr[, 4]) - sd(shr[, 4]), mean(mea[, 4]) - sd(mea[, 4]))
m.lu <- c(mean(woo[, 4]) + sd(woo[, 4]), mean(tal[, 4]) + sd(tal[, 4]), mean(shr[, 4]) + sd(shr[, 4]), mean(mea[, 4]) + sd(mea[, 4]))
plot(1:4, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c(mean(woo[, 5]), mean(tal[, 5]), mean(shr[, 5]), mean(mea[, 5]) )
m.li <- c(mean(woo[, 5]) - sd(woo[, 5]), mean(tal[, 5]) - sd(tal[, 5]), mean(shr[, 5]) - sd(shr[, 5]), mean(mea[, 5]) - sd(mea[, 5]))
m.lu <- c(mean(woo[, 5]) + sd(woo[, 5]), mean(tal[, 5]) + sd(tal[, 5]), mean(shr[, 5]) + sd(shr[, 5]), mean(mea[, 5]) + sd(mea[, 5]))
plot(1:4, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

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

#Jaccard Estimator
jmodelwoo <- lm((1 - woo[, 4]) ~ woo[, 6])
jmodelmix <- lm((1 - mix[, 4]) ~ mix[, 6])
jmodelmea <- lm((1 - mea[, 4]) ~ mea[, 6])
jmodelshr <- lm((1 - shr[, 4]) ~ shr[, 6])
jmodeltal <- lm((1 - tal[, 4]) ~ tal[, 6])

#Sorensen Estimator
smodelwoo <- lm((1 - woo[, 5]) ~ woo[, 6])
smodelmix <- lm((1 - mix[, 5]) ~ mix[, 6])
smodelmea <- lm((1 - mea[, 5]) ~ mea[, 6])
smodelshr <- lm((1 - shr[, 5]) ~ shr[, 6])
smodeltal <- lm((1 - tal[, 5]) ~ tal[, 6])

par(mfrow=c(3, 2)) #Figure - landscape_mean3.tiff
plot(woo[, 6], 1 - woo[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jmodelwoo)[[1]], b = coef(jmodelwoo)[[2]], col = 2, lwd = 4.5)
plot(woo[, 6], 1 - woo[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(smodelwoo)[[1]], b = coef(smodelwoo)[[2]], col = 2, lwd = 4.5)

plot(mix[, 6], 1 - mix[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodelmix)[[1]], b = coef(jmodelmix)[[2]], col = 2, lwd = 4.5)
plot(mix[, 6], 1 - mix[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodelmix)[[1]], b = coef(smodelmix)[[2]], col = 2, lwd = 4.5)

plot(mea[, 6], 1 - mea[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jmodelmea)[[1]], b = coef(jmodelmea)[[2]], col = 2, lwd = 4.5)
plot(mea[, 6], 1 - mea[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(smodelmea)[[1]], b = coef(smodelmea)[[2]], col = 2, lwd = 4.5)

par(mfrow=c(4, 2)) #Figure - landscape_mean4.tiff
plot(woo[, 6], 1 - woo[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jmodelwoo)[[1]], b = coef(jmodelwoo)[[2]], col = 2, lwd = 4.5)
plot(woo[, 6], 1 - woo[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(smodelwoo)[[1]], b = coef(smodelwoo)[[2]], col = 2, lwd = 4.5)

plot(tal[, 6], 1 - tal[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Tall")
abline(a = coef(jmodeltal)[[1]], b = coef(jmodeltal)[[2]], col = 2, lwd = 4.5)
plot(tal[, 6], 1 - tal[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Tall")
abline(a = coef(smodeltal)[[1]], b = coef(smodeltal)[[2]], col = 2, lwd = 4.5)

plot(shr[, 6], 1 - shr[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Short")
abline(a = coef(jmodelshr)[[1]], b = coef(jmodelshr)[[2]], col = 2, lwd = 4.5)
plot(shr[, 6], 1 - shr[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Short")
abline(a = coef(smodelshr)[[1]], b = coef(smodelshr)[[2]], col = 2, lwd = 4.5)

plot(mea[, 6], 1 - mea[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jmodelmea)[[1]], b = coef(jmodelmea)[[2]], col = 2, lwd = 4.5)
plot(mea[, 6], 1 - mea[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(smodelmea)[[1]], b = coef(smodelmea)[[2]], col = 2, lwd = 4.5)

# bootstrapping with 2000 replications
jwoores <- boot(data = as.data.frame(woo), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
swoores <- boot(data = as.data.frame(woo), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmixres <- boot(data = as.data.frame(mix), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smixres <- boot(data = as.data.frame(mix), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jtalres <- boot(data = as.data.frame(tal), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
stalres <- boot(data = as.data.frame(tal), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jshrres <- boot(data = as.data.frame(shr), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
sshrres <- boot(data = as.data.frame(shr), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmeares <- boot(data = as.data.frame(mea), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares <- boot(data = as.data.frame(mea), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

#get 95% confidence intervals
#Jaccard Estimator
temp1 <- boot.ci(jwoores, type="bca", index=1) # intercept 
temp2 <- boot.ci(jwoores, type="bca", index=2) # slope
temp3 <- boot.ci(jwoores, type="bca", index=3) # r.square
temp4 <- boot.ci(jwoores, type="bca", index=4) # mean

jwoores.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmixres, type="bca", index=2) # slope
temp3 <- boot.ci(jmixres, type="bca", index=3) # r.square
temp4 <- boot.ci(jmixres, type="bca", index=4) # mean

jmixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jtalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jtalres, type="bca", index=2) # slope
temp3 <- boot.ci(jtalres, type="bca", index=3) # r.square
temp4 <- boot.ci(jtalres, type="bca", index=4) # mean

jtalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jshrres, type="bca", index=2) # slope
temp3 <- boot.ci(jshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(jshrres, type="bca", index=4) # mean

jshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jmeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(jmeares, type="bca", index=2) # slope
temp3 <- boot.ci(jmeares, type="bca", index=3) # r.square
temp4 <- boot.ci(jmeares, type="bca", index=4) # mean

jmeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

#Sorensen Estimator
temp1 <- boot.ci(swoores, type="bca", index=1) # intercept 
temp2 <- boot.ci(swoores, type="bca", index=2) # slope
temp3 <- boot.ci(swoores, type="bca", index=3) # r.square
temp4 <- boot.ci(swoores, type="bca", index=4) # mean

swoores.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smixres, type="bca", index=1) # intercept 
temp2 <- boot.ci(smixres, type="bca", index=2) # slope
temp3 <- boot.ci(smixres, type="bca", index=3) # r.square
temp4 <- boot.ci(smixres, type="bca", index=4) # mean

smixres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(stalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(stalres, type="bca", index=2) # slope
temp3 <- boot.ci(stalres, type="bca", index=3) # r.square
temp4 <- boot.ci(stalres, type="bca", index=4) # mean

stalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sshrres, type="bca", index=2) # slope
temp3 <- boot.ci(sshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(sshrres, type="bca", index=4) # mean

sshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
#Jaccard(jac.boot.between_mean3.tiff)
#Intercept
mean.ci <- c(jwoores.ci[1, 1], jmixres.ci[1, 1], jmeares.ci[1, 1])
l.ci <- c(jwoores.ci[1, 2], jmixres.ci[1, 2], jmeares.ci[1, 2])
s.ci <- c(jwoores.ci[1, 3], jmixres.ci[1, 3], jmeares.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(jwoores.ci[4, 1], jmixres.ci[4, 1], jmeares.ci[4, 1])
l.ci <- c(jwoores.ci[4, 2], jmixres.ci[4, 2], jmeares.ci[4, 2])
s.ci <- c(jwoores.ci[4, 3], jmixres.ci[4, 3], jmeares.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(jwoores.ci[2, 1], jmixres.ci[2, 1], jmeares.ci[2, 1])
l.ci <- c(jwoores.ci[2, 2], jmixres.ci[2, 2], jmeares.ci[2, 2])
s.ci <- c(jwoores.ci[2, 3], jmixres.ci[2, 3], jmeares.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(jwoores.ci[3, 1], jmixres.ci[3, 1], jmeares.ci[3, 1])
l.ci <- c(jwoores.ci[3, 2], jmixres.ci[3, 2], jmeares.ci[3, 2])
s.ci <- c(jwoores.ci[3, 3], jmixres.ci[3, 3], jmeares.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

par(mfrow = c(2, 2))
#Sorensen(sor.boot.between_mean3.tiff)
#Intercept
mean.ci <- c(swoores.ci[1, 1], smixres.ci[1, 1], smeares.ci[1, 1])
l.ci <- c(swoores.ci[1, 2], smixres.ci[1, 2], smeares.ci[1, 2])
s.ci <- c(swoores.ci[1, 3], smixres.ci[1, 3], smeares.ci[1, 3])
plot(1:3, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(swoores.ci[4, 1], smixres.ci[4, 1], smeares.ci[4, 1])
l.ci <- c(swoores.ci[4, 2], smixres.ci[4, 2], smeares.ci[4, 2])
s.ci <- c(swoores.ci[4, 3], smixres.ci[4, 3], smeares.ci[4, 3])
plot(1:3, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:3, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:3, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(swoores.ci[2, 1], smixres.ci[2, 1], smeares.ci[2, 1])
l.ci <- c(swoores.ci[2, 2], smixres.ci[2, 2], smeares.ci[2, 2])
s.ci <- c(swoores.ci[2, 3], smixres.ci[2, 3], smeares.ci[2, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(swoores.ci[3, 1], smixres.ci[3, 1], smeares.ci[3, 1])
l.ci <- c(swoores.ci[3, 2], smixres.ci[3, 2], smeares.ci[3, 2])
s.ci <- c(swoores.ci[3, 3], smixres.ci[3, 3], smeares.ci[3, 3])
plot(1:3, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:3, labels = c("Forest", "Shrub", "Agric"))
segments(x0 = 1:3, y0 = l.ci, y1 = s.ci)
points(1:3, l.ci, lwd = 1.5, pch = "-")
points(1:3, s.ci, lwd = 1.5, pch = "-")

#Jaccard(jac.boot.between_mean4.tiff)
#Intercept
mean.ci <- c(jwoores.ci[1, 1], jtalres.ci[1, 1], jshrres.ci[1, 1], jmeares.ci[1, 1])
l.ci <- c(jwoores.ci[1, 2], jtalres.ci[1, 2], jshrres.ci[1, 2], jmeares.ci[1, 2])
s.ci <- c(jwoores.ci[1, 3], jtalres.ci[1, 3], jshrres.ci[1, 3], jmeares.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(jwoores.ci[4, 1], jtalres.ci[4, 1], jshrres.ci[4, 1], jmeares.ci[4, 1])
l.ci <- c(jwoores.ci[4, 2], jtalres.ci[4, 2], jshrres.ci[4, 2], jmeares.ci[4, 2])
s.ci <- c(jwoores.ci[4, 3], jtalres.ci[4, 3], jshrres.ci[4, 3], jmeares.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(jwoores.ci[2, 1], jtalres.ci[2, 1], jshrres.ci[2, 1], jmeares.ci[2, 1])
l.ci <- c(jwoores.ci[2, 2], jtalres.ci[2, 2], jshrres.ci[2, 2], jmeares.ci[2, 2])
s.ci <- c(jwoores.ci[2, 3], jtalres.ci[2, 3], jshrres.ci[2, 3], jmeares.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(jwoores.ci[3, 1], jtalres.ci[3, 1], jshrres.ci[3, 1], jmeares.ci[3, 1])
l.ci <- c(jwoores.ci[3, 2], jtalres.ci[3, 2], jshrres.ci[3, 2], jmeares.ci[3, 2])
s.ci <- c(jwoores.ci[3, 3], jtalres.ci[3, 3], jshrres.ci[3, 3], jmeares.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

par(mfrow = c(2, 2))
#Sorensen(sor.boot.between_mean4.tiff)
#Intercept
mean.ci <- c(swoores.ci[1, 1], stalres.ci[1, 1], sshrres.ci[1, 1], smeares.ci[1, 1])
l.ci <- c(swoores.ci[1, 2], stalres.ci[1, 2], sshrres.ci[1, 2], smeares.ci[1, 2])
s.ci <- c(swoores.ci[1, 3], stalres.ci[1, 3], sshrres.ci[1, 3], smeares.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(swoores.ci[4, 1], stalres.ci[4, 1], sshrres.ci[4, 1], smeares.ci[4, 1])
l.ci <- c(swoores.ci[4, 2], stalres.ci[4, 2], sshrres.ci[4, 2], smeares.ci[4, 2])
s.ci <- c(swoores.ci[4, 3], stalres.ci[4, 3], sshrres.ci[4, 3], smeares.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(swoores.ci[2, 1], stalres.ci[2, 1], sshrres.ci[2, 1], smeares.ci[2, 1])
l.ci <- c(swoores.ci[2, 2], stalres.ci[2, 2], sshrres.ci[2, 2], smeares.ci[2, 2])
s.ci <- c(swoores.ci[2, 3], stalres.ci[2, 3], sshrres.ci[2, 3], smeares.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(swoores.ci[3, 1], stalres.ci[3, 1], sshrres.ci[3, 1], smeares.ci[3, 1])
l.ci <- c(swoores.ci[3, 2], stalres.ci[3, 2], sshrres.ci[3, 2], smeares.ci[3, 2])
s.ci <- c(swoores.ci[3, 3], stalres.ci[3, 3], sshrres.ci[3, 3], smeares.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")


#Biotopes
mfma <- c(mf, mm, ma)
sfma <- c(sf, sm, sa)
tfma <- c(tf, tm, ta)
wfma <- c(wf, wm, wa)
mixf <- c(sf, sm, sa, tf, tm, ta)

mea.bio <- landsc_within(b.div.mean, mfma, mfma)
sho.bio <- landsc_within(b.div.mean, sfma, sfma)
tal.bio <- landsc_within(b.div.mean, tfma, tfma)
woo.bio <- landsc_within(b.div.mean, wfma, wfma)
mix.bio <- landsc_within(b.div.mean, mixf, mixf)

#DISSIMILARITY (dissimilaritybiotopes_all.tiff)
par(mfrow=c(2, 2))
m.data <- c(mean(woo.bio[, 4]), mean(mix.bio[, 4]), mean(mea.bio[, 4]))
m.li <- c(mean(woo.bio[, 4]) - sd(woo.bio[, 4]), mean(mix.bio[, 4]) - sd(mix.bio[, 4]), mean(mea.bio[, 4]) - sd(mea.bio[, 4]))
m.lu <- c(mean(woo.bio[, 4]) + sd(woo.bio[, 4]), mean(mix.bio[, 4]) + sd(mix.bio[, 4]), mean(mea.bio[, 4]) + sd(mea.bio[, 4]))
plot(1:3, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Forest", "Mixed", "Agric"))
segments(x0 = 1:3, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c( mean(woo.bio[, 5]), mean(mix.bio[, 5]), mean(mea.bio[, 5]))
m.li <- c(mean(woo.bio[, 5]) - sd(woo.bio[, 5]), mean(mix.bio[, 5]) - sd(mix.bio[, 5]), mean(mea.bio[, 5]) - sd(mea.bio[, 5]))
m.lu <- c(mean(woo.bio[, 5]) + sd(woo.bio[, 5]), mean(mix.bio[, 5]) + sd(mix.bio[, 5]), mean(mea.bio[, 5]) + sd(mea.bio[, 5]))
plot(1:3, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Forest", "Mixed", "Agric"))
segments(x0 = 1:3, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

##with all biotopes
m.data <- c(mean(woo.bio[, 4]), mean(tal.bio[, 4]), mean(sho.bio[, 4]), mean(mea.bio[, 4]))
m.li <- c(mean(woo.bio[, 4]) - sd(woo.bio[, 4]), mean(tal.bio[, 4]) - sd(tal.bio[, 4]), mean(sho.bio[, 4]) - sd(sho.bio[, 4]), mean(mea.bio[, 4]) - sd(mea.bio[, 4]))
m.lu <- c(mean(woo.bio[, 4]) + sd(woo.bio[, 4]), mean(tal.bio[, 4]) + sd(tal.bio[, 4]), mean(sho.bio[, 4]) + sd(sho.bio[, 4]), mean(mea.bio[, 4]) + sd(mea.bio[, 4]))
plot(1:4, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

m.data <- c(mean(woo.bio[, 5]), mean(tal.bio[, 5]), mean(sho.bio[, 5]), mean(mea.bio[, 5]))
m.li <- c(mean(woo.bio[, 5]) - sd(woo.bio[, 5]), mean(tal.bio[, 5]) - sd(tal.bio[, 5]), mean(sho.bio[, 5]) - sd(sho.bio[, 5]), mean(mea.bio[, 5]) - sd(mea.bio[, 5]))
m.lu <- c(mean(woo.bio[, 5]) + sd(woo.bio[, 5]), mean(tal.bio[, 5]) + sd(tal.bio[, 5]), mean(sho.bio[, 5]) + sd(sho.bio[, 5]), mean(mea.bio[, 5]) + sd(mea.bio[, 5]))
plot(1:4, 1 - m.data, xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - m.li, y1 = 1 - m.lu, col = 1)

#Jaccard Estimator
jmodelwoo.bio <- lm((1 - woo.bio[, 4]) ~ woo.bio[, 6])
jmodelmix.bio <- lm((1 - mix.bio[, 4]) ~ mix.bio[, 6])
jmodelmea.bio <- lm((1 - mea.bio[, 4]) ~ mea.bio[, 6])
jmodelsho.bio <- lm((1 - sho.bio[, 4]) ~ sho.bio[, 6])
jmodeltal.bio <- lm((1 - tal.bio[, 4]) ~ tal.bio[, 6])

#Sorensen Estimator
smodelwoo.bio <- lm((1 - woo.bio[, 5]) ~ woo.bio[, 6])
smodelmix.bio <- lm((1 - mix.bio[, 5]) ~ mix.bio[, 6])
smodelmea.bio <- lm((1 - mea.bio[, 5]) ~ mea.bio[, 6])
smodelsho.bio <- lm((1 - sho.bio[, 5]) ~ sho.bio[, 6])
smodeltal.bio <- lm((1 - tal.bio[, 5]) ~ tal.bio[, 6])

par(mfrow=c(3, 2)) #Figure - eachbiotope_mean3.tiff
plot(woo.bio[, 6], 1 - woo.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jmodelwoo.bio)[[1]], b = coef(jmodelwoo.bio)[[2]], col = 2, lwd = 4.5)
plot(woo.bio[, 6], 1 - woo.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(smodelwoo.bio)[[1]], b = coef(smodelwoo.bio)[[2]], col = 2, lwd = 4.5)

plot(mix.bio[, 6], 1 - mix.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodelmix.bio)[[1]], b = coef(jmodelmix.bio)[[2]], col = 2, lwd = 4.5)
plot(mix.bio[, 6], 1 - mix.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodelmix.bio)[[1]], b = coef(smodelmix.bio)[[2]], col = 2, lwd = 4.5)

plot(mea.bio[, 6], 1 - mea.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jmodelmea.bio)[[1]], b = coef(jmodelmea.bio)[[2]], col = 2, lwd = 4.5)
plot(mea.bio[, 6], 1 - mea.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(smodelmea.bio)[[1]], b = coef(smodelmea.bio)[[2]], col = 2, lwd = 4.5)

par(mfrow=c(4, 2)) #Figure - eachbiotope_mean4.tiff
plot(woo.bio[, 6], 1 - woo.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(jmodelwoo.bio)[[1]], b = coef(jmodelwoo.bio)[[2]], col = 2, lwd = 4.5)
plot(woo.bio[, 6], 1 - woo.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(a = coef(smodelwoo.bio)[[1]], b = coef(smodelwoo.bio)[[2]], col = 2, lwd = 4.5)

plot(tal.bio[, 6], 1 - tal.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodeltal.bio)[[1]], b = coef(jmodeltal.bio)[[2]], col = 2, lwd = 4.5)
plot(tal.bio[, 6], 1 - tal.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodeltal.bio)[[1]], b = coef(smodeltal.bio)[[2]], col = 2, lwd = 4.5)

plot(sho.bio[, 6], 1 - sho.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(jmodelsho.bio)[[1]], b = coef(jmodelsho.bio)[[2]], col = 2, lwd = 4.5)
plot(sho.bio[, 6], 1 - sho.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(a = coef(smodelsho.bio)[[1]], b = coef(smodelsho.bio)[[2]], col = 2, lwd = 4.5)

plot(mea.bio[, 6], 1 - mea.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(jmodelmea.bio)[[1]], b = coef(jmodelmea.bio)[[2]], col = 2, lwd = 4.5)
plot(mea.bio[, 6], 1 - mea.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(a = coef(smodelmea.bio)[[1]], b = coef(smodelmea.bio)[[2]], col = 2, lwd = 4.5)

#Mean 
par(mfrow=c(3, 2)) #Figure - eachbiotope_mean3.tiff
plot(woo.bio[, 6], 1 - woo.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(woo.bio[woo.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 445) & (woo.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 1810) & (woo.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(woo.bio[(woo.bio[, 6] > 1811), 4]), 5690), col = 2, lwd = 4.5)
plot(woo.bio[, 6], 1 - woo.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(woo.bio[woo.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 445) & (woo.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 1810) & (woo.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(woo.bio[(woo.bio[, 6] > 1811), 5]), 5690), col = 2, lwd = 4.5)

plot(mix.bio[, 6], 1 - mix.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(mix.bio[mix.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(mix.bio[(mix.bio[, 6] <= 445) & (mix.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(mix.bio[(mix.bio[, 6] <= 1810) & (mix.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(mix.bio[(mix.bio[, 6] > 1811), 4]), 5690), col = 2, lwd = 4.5)
plot(mix.bio[, 6], 1 - mix.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(mix.bio[mix.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(mix.bio[(mix.bio[, 6] <= 445) & (mix.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(mix.bio[(mix.bio[, 6] <= 1810) & (mix.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(mix.bio[(mix.bio[, 6] > 1811), 5]), 5690), col = 2, lwd = 4.5)

plot(mea.bio[, 6], 1 - mea.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(mea.bio[mea.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 445) & (mea.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 1810) & (mea.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(mea.bio[(mea.bio[, 6] > 1810), 4]), 5690), col = 2, lwd = 4.5)
plot(mea.bio[, 6], 1 - mea.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(mea.bio[mea.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 445) & (mea.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 1810) & (mea.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(mea.bio[(mea.bio[, 6] > 1810), 5]), 5690), col = 2, lwd = 4.5)

par(mfrow=c(4, 2)) #Figure - eachbiotope_mean4.tiff
plot(woo.bio[, 6], 1 - woo.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(woo.bio[woo.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 445) & (woo.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 1810) & (woo.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(woo.bio[(woo.bio[, 6] > 1811), 4]), 5690), col = 2, lwd = 4.5)
plot(woo.bio[, 6], 1 - woo.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Forest")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(woo.bio[woo.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 445) & (woo.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(woo.bio[(woo.bio[, 6] <= 1810) & (woo.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(woo.bio[(woo.bio[, 6] > 1811), 5]), 5690), col = 2, lwd = 4.5)

plot(tal.bio[, 6], 1 - tal.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(tal.bio[tal.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(tal.bio[(tal.bio[, 6] <= 445) & (tal.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(tal.bio[(tal.bio[, 6] <= 1810) & (tal.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(tal.bio[(tal.bio[, 6] > 1811), 4]), 5690), col = 2, lwd = 4.5)
plot(tal.bio[, 6], 1 - tal.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(tal.bio[tal.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(tal.bio[(tal.bio[, 6] <= 445) & (tal.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(tal.bio[(tal.bio[, 6] <= 1810) & (tal.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(tal.bio[(tal.bio[, 6] > 1811), 5]), 5690), col = 2, lwd = 4.5)

plot(sho.bio[, 6], 1 - sho.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(sho.bio[sho.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(sho.bio[(sho.bio[, 6] <= 445) & (sho.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(sho.bio[(sho.bio[, 6] <= 1810) & (sho.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(sho.bio[(sho.bio[, 6] > 1811), 4]), 5690), col = 2, lwd = 4.5)
plot(sho.bio[, 6], 1 - sho.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Shrub")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(sho.bio[sho.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(sho.bio[(sho.bio[, 6] <= 445) & (sho.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(sho.bio[(sho.bio[, 6] <= 1810) & (sho.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(sho.bio[(sho.bio[, 6] > 1811), 5]), 5690), col = 2, lwd = 4.5)

plot(mea.bio[, 6], 1 - mea.bio[, 4], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(mea.bio[mea.bio[, 6] <= 115, 4]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 445) & (mea.bio[, 6] > 115), 4]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 1810) & (mea.bio[, 6] > 445), 4]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(mea.bio[(mea.bio[, 6] > 1810), 4]), 5690), col = 2, lwd = 4.5)
plot(mea.bio[, 6], 1 - mea.bio[, 5], ylim = c(0, 1), xlim = c(0, 7500), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Agric")
abline(v = c(115, 455, 1810), col = "gray", lwd = 1.5, lty = 2)
lines(1:115, rep(1 - mean(mea.bio[mea.bio[, 6] <= 115, 5]), 115), col = 2, lwd = 4.5)
lines(116:445, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 445) & (mea.bio[, 6] > 115), 5]), 330), col = 2, lwd = 4.5)
lines(446:1810, rep(1 - mean(mea.bio[(mea.bio[, 6] <= 1810) & (mea.bio[, 6] > 445), 5]), 1365), col = 2, lwd = 4.5)
lines(1811:7500, rep(1 - mean(mea.bio[(mea.bio[, 6] > 1810), 5]), 5690), col = 2, lwd = 4.5)

# bootstrapping with 2000 replications
jforres <- boot(data = as.data.frame(woo.bio), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
sforres <- boot(data = as.data.frame(woo.bio), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmixres <- boot(data = as.data.frame(mix.bio), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smixres <- boot(data = as.data.frame(mix.bio), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jtalres <- boot(data = as.data.frame(tal.bio), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
stalres <- boot(data = as.data.frame(tal.bio), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jshrres <- boot(data = as.data.frame(sho.bio), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
sshrres <- boot(data = as.data.frame(sho.bio), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

jmeares <- boot(data = as.data.frame(mea.bio), statistic = bs.jac, R = 2000, formula = (1 - Jabd) ~ ipcc)
smeares <- boot(data = as.data.frame(mea.bio), statistic = bs.sor, R = 2000, formula = (1 - Labd) ~ ipcc)

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

temp1 <- boot.ci(jtalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jtalres, type="bca", index=2) # slope
temp3 <- boot.ci(jtalres, type="bca", index=3) # r.square
temp4 <- boot.ci(jtalres, type="bca", index=4) # mean

jtalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(jshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(jshrres, type="bca", index=2) # slope
temp3 <- boot.ci(jshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(jshrres, type="bca", index=4) # mean

jshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

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

temp1 <- boot.ci(stalres, type="bca", index=1) # intercept 
temp2 <- boot.ci(stalres, type="bca", index=2) # slope
temp3 <- boot.ci(stalres, type="bca", index=3) # r.square
temp4 <- boot.ci(stalres, type="bca", index=4) # mean

stalres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(sshrres, type="bca", index=1) # intercept 
temp2 <- boot.ci(sshrres, type="bca", index=2) # slope
temp3 <- boot.ci(sshrres, type="bca", index=3) # r.square
temp4 <- boot.ci(sshrres, type="bca", index=4) # mean

sshrres.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

temp1 <- boot.ci(smeares, type="bca", index=1) # intercept 
temp2 <- boot.ci(smeares, type="bca", index=2) # slope
temp3 <- boot.ci(smeares, type="bca", index=3) # r.square
temp4 <- boot.ci(smeares, type="bca", index=4) # mean

smeares.ci <- rbind(cbind(temp1$t0, temp1$bca[4], temp1$bca[5]), cbind(temp2$t0, temp2$bca[4], temp2$bca[5]), cbind(temp3$t0, temp3$bca[4], temp3$bca[5]), cbind(temp4$t0, temp4$bca[4], temp4$bca[5]))

par(mfrow = c(2, 2))
#Jaccard(jac.boot.biotope_mean3.tiff)
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
#Sorensen(sor.boot.biotope_mean3.tiff)
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

#Jaccard(jac.boot.biotope_mean4.tiff)
#Intercept
mean.ci <- c(jforres.ci[1, 1], jtalres.ci[1, 1], jshrres.ci[1, 1], jmeares.ci[1, 1])
l.ci <- c(jforres.ci[1, 2], jtalres.ci[1, 2], jshrres.ci[1, 2], jmeares.ci[1, 2])
s.ci <- c(jforres.ci[1, 3], jtalres.ci[1, 3], jshrres.ci[1, 3], jmeares.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Jaccard Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(jforres.ci[4, 1], jtalres.ci[4, 1], jshrres.ci[4, 1], jmeares.ci[4, 1])
l.ci <- c(jforres.ci[4, 2], jtalres.ci[4, 2], jshrres.ci[4, 2], jmeares.ci[4, 2])
s.ci <- c(jforres.ci[4, 3], jtalres.ci[4, 3], jshrres.ci[4, 3], jmeares.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(jforres.ci[2, 1], jtalres.ci[2, 1], jshrres.ci[2, 1], jmeares.ci[2, 1])
l.ci <- c(jforres.ci[2, 2], jtalres.ci[2, 2], jshrres.ci[2, 2], jmeares.ci[2, 2])
s.ci <- c(jforres.ci[2, 3], jtalres.ci[2, 3], jshrres.ci[2, 3], jmeares.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(jforres.ci[3, 1], jtalres.ci[3, 1], jshrres.ci[3, 1], jmeares.ci[3, 1])
l.ci <- c(jforres.ci[3, 2], jtalres.ci[3, 2], jshrres.ci[3, 2], jmeares.ci[3, 2])
s.ci <- c(jforres.ci[3, 3], jtalres.ci[3, 3], jshrres.ci[3, 3], jmeares.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

par(mfrow = c(2, 2))
#Sorensen(sor.boot.biotope_mean4.tiff)
#Intercept
mean.ci <- c(sforres.ci[1, 1], stalres.ci[1, 1], sshrres.ci[1, 1], smeares.ci[1, 1])
l.ci <- c(sforres.ci[1, 2], stalres.ci[1, 2], sshrres.ci[1, 2], smeares.ci[1, 2])
s.ci <- c(sforres.ci[1, 3], stalres.ci[1, 3], sshrres.ci[1, 3], smeares.ci[1, 3])
plot(1:4, mean.ci, xaxt = "n", ylim =c(0, 1), ylab = "Intercept", xlab = "", pch = "-", lwd = 2)
title("All Years - Sorensen Estimator", outer = TRUE, cex = 1.5)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#Dissimilarity
mean.ci <- c(sforres.ci[4, 1], stalres.ci[4, 1], sshrres.ci[4, 1], smeares.ci[4, 1])
l.ci <- c(sforres.ci[4, 2], stalres.ci[4, 2], sshrres.ci[4, 2], smeares.ci[4, 2])
s.ci <- c(sforres.ci[4, 3], stalres.ci[4, 3], sshrres.ci[4, 3], smeares.ci[4, 3])
plot(1:4, 1 - mean.ci, xaxt = "n", ylim =c(0, 1), ylab = expression(~beta*"-diversity"), xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = 1 - l.ci, y1 = 1 - s.ci)
points(1:4, 1 - l.ci, lwd = 1.5, pch = "-")
points(1:4, 1 - s.ci, lwd = 1.5, pch = "-")

#Slope
mean.ci <- c(sforres.ci[2, 1], stalres.ci[2, 1], sshrres.ci[2, 1], smeares.ci[2, 1])
l.ci <- c(sforres.ci[2, 2], stalres.ci[2, 2], sshrres.ci[2, 2], smeares.ci[2, 2])
s.ci <- c(sforres.ci[2, 3], stalres.ci[2, 3], sshrres.ci[2, 3], smeares.ci[2, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "Slope", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

#R-Square
mean.ci <- c(sforres.ci[3, 1], stalres.ci[3, 1], sshrres.ci[3, 1], smeares.ci[3, 1])
l.ci <- c(sforres.ci[3, 2], stalres.ci[3, 2], sshrres.ci[3, 2], smeares.ci[3, 2])
s.ci <- c(sforres.ci[3, 3], stalres.ci[3, 3], sshrres.ci[3, 3], smeares.ci[3, 3])
plot(1:4, mean.ci, xaxt = "n", ylim = c(min(l.ci), max(s.ci)), ylab = "R-Square", xlab = "", pch = "-", lwd = 2)
axis(1, at = 1:4, labels = c("Forest", "Tall", "Short", "Agric"))
segments(x0 = 1:4, y0 = l.ci, y1 = s.ci)
points(1:4, l.ci, lwd = 1.5, pch = "-")
points(1:4, s.ci, lwd = 1.5, pch = "-")

