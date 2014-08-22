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

idmea <- 1:28
idmix <- 57:84
idfor <- 29:56

###ALL YEARS####
meamixdata <- landsc_between(b.div, idmea, idmix); comment(meamixdata) <- "MEADOW MIXED"
meafordata <- landsc_between(b.div, idmea, idfor); comment(meafordata) <- "MEADOW FOREST"
mixfordata <- landsc_between(b.div, idmix, idfor); comment(mixfordata) <- "MIXED FOREST"

sum.meamix <- figure.div(meamixdata)
sum.meafor <- figure.div(meafordata)
sum.mixfor <- figure.div(mixfordata)

###FIRST YEAR####
meamixdata1 <- landsc_between(b.div1, idmea, idmix); comment(meamixdata1) <- "MEADOW MIXED 1"
meafordata1 <- landsc_between(b.div1, idmea, idfor); comment(meafordata1) <- "MEADOW FOREST 1"
mixfordata1 <- landsc_between(b.div1, idmix, idfor); comment(mixfordata1) <- "MIXED FOREST 1"

sum.meamix1 <- figure.div(meamixdata1)
sum.meafor1 <- figure.div(meafordata1)
sum.mixfor1 <- figure.div(mixfordata1)

###SECOND YEAR####
meamixdata2 <- landsc_between(b.div2, idmea, idmix); comment(meamixdata2) <- "MEADOW MIXED 2"
meafordata2 <- landsc_between(b.div2, idmea, idfor); comment(meafordata2) <- "MEADOW FOREST 2"
mixfordata2 <- landsc_between(b.div2, idmix, idfor); comment(mixfordata2) <- "MIXED FOREST 2"

sum.meamix2 <- figure.div(meamixdata2)
sum.meafor2 <- figure.div(meafordata2)
sum.mixfor2 <- figure.div(mixfordata2)


### between landscapes (dissimilaritybetween.tiff) #####
##ALL YEARS
par(mfrow = c(3, 2))
sum.mean <- cbind(sum.meamix[, 6]/sum.meamix[, 1], sum.meafor[, 6]/sum.meafor[, 1], sum.mixfor[, 6]/sum.mixfor[, 1])
sum.sd <- cbind(sqrt((sum.meamix[, 7]/(sum.meamix[, 1]) - (sum.meamix[, 6]/sum.meamix[, 1])^2)), sqrt((sum.meafor[, 7]/(sum.meafor[, 1]) - (sum.meafor[, 6]/sum.meafor[, 1])^2)), sqrt((sum.mixfor[, 7]/(sum.mixfor[, 1]) - (sum.mixfor[, 6]/sum.mixfor[, 1])^2)))
plot(1:15, 1 - sum.mean[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:15, y0 = 1 - sum.mean[, 1] - sum.sd[, 1], y1 = 1 - sum.mean[, 1] + sum.sd[, 1], col = 1)
points(1:15, 1 - sum.mean[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean[, 2] - sum.sd[, 2], y1 = 1 - sum.mean[, 2] + sum.sd[, 2], col = 2)
points(1:15, 1 - sum.mean[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean[, 3] - sum.sd[, 3], y1 = 1 - sum.mean[, 3] + sum.sd[, 3], col = 3)
legend("bottomright", c("meamix", "mixfor","meafor"), pch = 1, col = c(1, 3, 2), bty="n")

sum.mean <- cbind(sum.meamix[, 8]/sum.meamix[, 1], sum.meafor[, 8]/sum.meafor[, 1], sum.mixfor[, 8]/sum.mixfor[, 1])
sum.sd <- cbind(sqrt((sum.meamix[, 9]/(sum.meamix[, 1]) - (sum.meamix[, 8]/sum.meamix[, 1])^2)), sqrt((sum.meafor[, 9]/(sum.meafor[, 1]) - (sum.meafor[, 8]/sum.meafor[, 1])^2)), sqrt((sum.mixfor[, 9]/(sum.mixfor[, 1]) - (sum.mixfor[, 8]/sum.mixfor[, 1])^2)))
plot(1:15, 1 - sum.mean[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:15, y0 = 1 - sum.mean[, 1] - sum.sd[, 1], y1 = 1 - sum.mean[, 1] + sum.sd[, 1], col = 1)
points(1:15, 1 - sum.mean[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean[, 2] - sum.sd[, 2], y1 = 1 - sum.mean[, 2] + sum.sd[, 2], col = 2)
points(1:15, 1 - sum.mean[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean[, 3] - sum.sd[, 3], y1 = 1 - sum.mean[, 3] + sum.sd[, 3], col = 3)

##FIRST YEAR
sum.mean1 <- cbind(sum.meamix1[, 6]/sum.meamix1[, 1], sum.meafor1[, 6]/sum.meafor1[, 1], sum.mixfor1[, 6]/sum.mixfor1[, 1])
sum.sd1 <- cbind(sqrt((sum.meamix1[, 7]/(sum.meamix1[, 1]) - (sum.meamix1[, 6]/sum.meamix1[, 1])^2)), sqrt((sum.meafor1[, 7]/(sum.meafor1[, 1]) - (sum.meafor1[, 6]/sum.meafor1[, 1])^2)), sqrt((sum.mixfor1[, 7]/(sum.mixfor1[, 1]) - (sum.mixfor1[, 6]/sum.mixfor1[, 1])^2)))
plot(1:15, 1 - sum.mean1[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:15, y0 = 1 - sum.mean1[, 1] - sum.sd1[, 1], y1 = 1 - sum.mean1[, 1] + sum.sd1[, 1], col = 1)
points(1:15, 1 - sum.mean1[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean1[, 2] - sum.sd1[, 2], y1 = 1 - sum.mean1[, 2] + sum.sd1[, 2], col = 2)
points(1:15, 1 - sum.mean1[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean1[, 3] - sum.sd1[, 3], y1 = 1 - sum.mean1[, 3] + sum.sd1[, 3], col = 3)

sum.mean1 <- cbind(sum.meamix1[, 8]/sum.meamix1[, 1], sum.meafor1[, 8]/sum.meafor1[, 1], sum.mixfor1[, 8]/sum.mixfor1[, 1])
sum.sd1 <- cbind(sqrt((sum.meamix1[, 9]/(sum.meamix1[, 1]) - (sum.meamix1[, 8]/sum.meamix1[, 1])^2)), sqrt((sum.meafor1[, 9]/(sum.meafor1[, 1]) - (sum.meafor1[, 8]/sum.meafor1[, 1])^2)), sqrt((sum.mixfor1[, 9]/(sum.mixfor1[, 1]) - (sum.mixfor1[, 8]/sum.mixfor1[, 1])^2)))
plot(1:15, 1 - sum.mean1[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:15, y0 = 1 - sum.mean1[, 1] - sum.sd1[, 1], y1 = 1 - sum.mean1[, 1] + sum.sd1[, 1], col = 1)
points(1:15, 1 - sum.mean1[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean1[, 2] - sum.sd1[, 2], y1 = 1 - sum.mean1[, 2] + sum.sd1[, 2], col = 2)
points(1:15, 1 - sum.mean1[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean1[, 3] - sum.sd1[, 3], y1 = 1 - sum.mean1[, 3] + sum.sd1[, 3], col = 3)

##SECOND YEAR
sum.mean2 <- cbind(sum.meamix2[, 6]/sum.meamix2[, 1], sum.meafor2[, 6]/sum.meafor2[, 1], sum.mixfor2[, 6]/sum.mixfor2[, 1])
sum.sd2 <- cbind(sqrt((sum.meamix2[, 7]/(sum.meamix2[, 1]) - (sum.meamix2[, 6]/sum.meamix2[, 1])^2)), sqrt((sum.meafor2[, 7]/(sum.meafor2[, 1]) - (sum.meafor2[, 6]/sum.meafor2[, 1])^2)), sqrt((sum.mixfor2[, 7]/(sum.mixfor2[, 1]) - (sum.mixfor2[, 6]/sum.mixfor2[, 1])^2)))
plot(1:15, 1 - sum.mean2[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:15, y0 = 1 - sum.mean2[, 1] - sum.sd2[, 1], y1 = 1 - sum.mean2[, 1] + sum.sd2[, 1], col = 1)
points(1:15, 1 - sum.mean2[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean2[, 2] - sum.sd2[, 2], y1 = 1 - sum.mean2[, 2] + sum.sd2[, 2], col = 2)
points(1:15, 1 - sum.mean2[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean2[, 3] - sum.sd2[, 3], y1 = 1 - sum.mean2[, 3] + sum.sd2[, 3], col = 3)

sum.mean2 <- cbind(sum.meamix2[, 8]/sum.meamix2[, 1], sum.meafor2[, 8]/sum.meafor2[, 1], sum.mixfor2[, 8]/sum.mixfor2[, 1])
sum.sd2 <- cbind(sqrt((sum.meamix2[, 9]/(sum.meamix2[, 1]) - (sum.meamix2[, 8]/sum.meamix2[, 1])^2)), sqrt((sum.meafor2[, 9]/(sum.meafor2[, 1]) - (sum.meafor2[, 8]/sum.meafor2[, 1])^2)), sqrt((sum.mixfor2[, 9]/(sum.mixfor2[, 1]) - (sum.mixfor2[, 8]/sum.mixfor2[, 1])^2)))
plot(1:15, 1 - sum.mean2[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:15, y0 = 1 - sum.mean2[, 1] - sum.sd2[, 1], y1 = 1 - sum.mean2[, 1] + sum.sd2[, 1], col = 1)
points(1:15, 1 - sum.mean2[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean2[, 2] - sum.sd2[, 2], y1 = 1 - sum.mean2[, 2] + sum.sd2[, 2], col = 2)
points(1:15, 1 - sum.mean2[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = 1 - sum.mean2[, 3] - sum.sd2[, 3], y1 = 1 - sum.mean2[, 3] + sum.sd2[, 3], col = 3)

### between landscapes (similaritybetween.tiff) #####
##ALL YEARS
par(mfrow = c(3, 2))
sum.mean <- cbind(sum.meamix[, 6]/sum.meamix[, 1], sum.meafor[, 6]/sum.meafor[, 1], sum.mixfor[, 6]/sum.mixfor[, 1])
sum.sd <- cbind(sqrt((sum.meamix[, 7]/(sum.meamix[, 1]) - (sum.meamix[, 6]/sum.meamix[, 1])^2)), sqrt((sum.meafor[, 7]/(sum.meafor[, 1]) - (sum.meafor[, 6]/sum.meafor[, 1])^2)), sqrt((sum.mixfor[, 7]/(sum.mixfor[, 1]) - (sum.mixfor[, 6]/sum.mixfor[, 1])^2)))
plot(1:15, sum.mean[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:15, y0 = sum.mean[, 1] - sum.sd[, 1], y1 = sum.mean[, 1] + sum.sd[, 1], col = 1)
points(1:15, sum.mean[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = sum.mean[, 2] - sum.sd[, 2], y1 = sum.mean[, 2] + sum.sd[, 2], col = 2)
points(1:15, sum.mean[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = sum.mean[, 3] - sum.sd[, 3], y1 = sum.mean[, 3] + sum.sd[, 3], col = 3)
legend("topright", c("meamix", "mixfor","meafor"), pch = 1, col = c(1, 3, 2), bty="n")

sum.mean <- cbind(sum.meamix[, 8]/sum.meamix[, 1], sum.meafor[, 8]/sum.meafor[, 1], sum.mixfor[, 8]/sum.mixfor[, 1])
sum.sd <- cbind(sqrt((sum.meamix[, 9]/(sum.meamix[, 1]) - (sum.meamix[, 8]/sum.meamix[, 1])^2)), sqrt((sum.meafor[, 9]/(sum.meafor[, 1]) - (sum.meafor[, 8]/sum.meafor[, 1])^2)), sqrt((sum.mixfor[, 9]/(sum.mixfor[, 1]) - (sum.mixfor[, 8]/sum.mixfor[, 1])^2)))
plot(1:15, sum.mean[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:15, y0 = sum.mean[, 1] - sum.sd[, 1], y1 = sum.mean[, 1] + sum.sd[, 1], col = 1)
points(1:15, sum.mean[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = sum.mean[, 2] - sum.sd[, 2], y1 = sum.mean[, 2] + sum.sd[, 2], col = 2)
points(1:15, sum.mean[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = sum.mean[, 3] - sum.sd[, 3], y1 = sum.mean[, 3] + sum.sd[, 3], col = 3)

##FIRST YEAR
sum.mean1 <- cbind(sum.meamix1[, 6]/sum.meamix1[, 1], sum.meafor1[, 6]/sum.meafor1[, 1], sum.mixfor1[, 6]/sum.mixfor1[, 1])
sum.sd1 <- cbind(sqrt((sum.meamix1[, 7]/(sum.meamix1[, 1]) - (sum.meamix1[, 6]/sum.meamix1[, 1])^2)), sqrt((sum.meafor1[, 7]/(sum.meafor1[, 1]) - (sum.meafor1[, 6]/sum.meafor1[, 1])^2)), sqrt((sum.mixfor1[, 7]/(sum.mixfor1[, 1]) - (sum.mixfor1[, 6]/sum.mixfor1[, 1])^2)))
plot(1:15, sum.mean1[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:15, y0 = sum.mean1[, 1] - sum.sd1[, 1], y1 = sum.mean1[, 1] + sum.sd1[, 1], col = 1)
points(1:15, sum.mean1[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = sum.mean1[, 2] - sum.sd1[, 2], y1 = sum.mean1[, 2] + sum.sd1[, 2], col = 2)
points(1:15, sum.mean1[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = sum.mean1[, 3] - sum.sd1[, 3], y1 = sum.mean1[, 3] + sum.sd1[, 3], col = 3)

sum.mean1 <- cbind(sum.meamix1[, 8]/sum.meamix1[, 1], sum.meafor1[, 8]/sum.meafor1[, 1], sum.mixfor1[, 8]/sum.mixfor1[, 1])
sum.sd1 <- cbind(sqrt((sum.meamix1[, 9]/(sum.meamix1[, 1]) - (sum.meamix1[, 8]/sum.meamix1[, 1])^2)), sqrt((sum.meafor1[, 9]/(sum.meafor1[, 1]) - (sum.meafor1[, 8]/sum.meafor1[, 1])^2)), sqrt((sum.mixfor1[, 9]/(sum.mixfor1[, 1]) - (sum.mixfor1[, 8]/sum.mixfor1[, 1])^2)))
plot(1:15, sum.mean1[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:15, y0 = sum.mean1[, 1] - sum.sd1[, 1], y1 = sum.mean1[, 1] + sum.sd1[, 1], col = 1)
points(1:15, sum.mean1[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = sum.mean1[, 2] - sum.sd1[, 2], y1 = sum.mean1[, 2] + sum.sd1[, 2], col = 2)
points(1:15, sum.mean1[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = sum.mean1[, 3] - sum.sd1[, 3], y1 = sum.mean1[, 3] + sum.sd1[, 3], col = 3)

##SECOND YEAR
sum.mean2 <- cbind(sum.meamix2[, 6]/sum.meamix2[, 1], sum.meafor2[, 6]/sum.meafor2[, 1], sum.mixfor2[, 6]/sum.mixfor2[, 1])
sum.sd2 <- cbind(sqrt((sum.meamix2[, 7]/(sum.meamix2[, 1]) - (sum.meamix2[, 6]/sum.meamix2[, 1])^2)), sqrt((sum.meafor2[, 7]/(sum.meafor2[, 1]) - (sum.meafor2[, 6]/sum.meafor2[, 1])^2)), sqrt((sum.mixfor2[, 7]/(sum.mixfor2[, 1]) - (sum.mixfor2[, 6]/sum.mixfor2[, 1])^2)))
plot(1:15, sum.mean2[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:15, y0 = sum.mean2[, 1] - sum.sd2[, 1], y1 = sum.mean2[, 1] + sum.sd2[, 1], col = 1)
points(1:15, sum.mean2[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = sum.mean2[, 2] - sum.sd2[, 2], y1 = sum.mean2[, 2] + sum.sd2[, 2], col = 2)
points(1:15, sum.mean2[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = sum.mean2[, 3] - sum.sd2[, 3], y1 = sum.mean2[, 3] + sum.sd2[, 3], col = 3)

sum.mean2 <- cbind(sum.meamix2[, 8]/sum.meamix2[, 1], sum.meafor2[, 8]/sum.meafor2[, 1], sum.mixfor2[, 8]/sum.mixfor2[, 1])
sum.sd2 <- cbind(sqrt((sum.meamix2[, 9]/(sum.meamix2[, 1]) - (sum.meamix2[, 8]/sum.meamix2[, 1])^2)), sqrt((sum.meafor2[, 9]/(sum.meafor2[, 1]) - (sum.meafor2[, 8]/sum.meafor2[, 1])^2)), sqrt((sum.mixfor2[, 9]/(sum.mixfor2[, 1]) - (sum.mixfor2[, 8]/sum.mixfor2[, 1])^2)))
plot(1:15, sum.mean2[, 1], ylim = c(0, 1), type = "b", xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:15, y0 = sum.mean2[, 1] - sum.sd2[, 1], y1 = sum.mean2[, 1] + sum.sd2[, 1], col = 1)
points(1:15, sum.mean2[, 2], col = 2, type = "b")
segments(x0 = 1:15, y0 = sum.mean2[, 2] - sum.sd2[, 2], y1 = sum.mean2[, 2] + sum.sd2[, 2], col = 2)
points(1:15, sum.mean2[, 3], col = 3, type = "b")
segments(x0 = 1:15, y0 = sum.mean2[, 3] - sum.sd2[, 3], y1 = sum.mean2[, 3] + sum.sd2[, 3], col = 3)

##FUNCTION FILTER####
figure.div <- function(base){
  #Jun, Sun, Jad, Sad
  Jun <- matrix(0, nrow = 15, ncol = 2)
  Sun <- matrix(0, nrow = 15, ncol = 2)
  Jad <- matrix(0, nrow = 15, ncol = 2)
  Sad <- matrix(0, nrow = 15, ncol = 2)
  freq <- matrix(0, nrow = 15)
  for(i in 1:(dim(base)[1])){
    if(base[i, 21] < 500){
      Jun[1, 1] <- Jun[1, 1] + base[i, 15]
      Sun[1, 1] <- Sun[1, 1] + base[i, 16]
      Jad[1, 1] <- Jad[1, 1] + base[i, 19]
      Sad[1, 1] <- Sad[1, 1] + base[i, 20]
      freq[1] <- freq[1] + 1
      Jun[1, 2] <- Jun[1, 2] + base[i, 15]^2
      Sun[1, 2] <- Sun[1, 2] + base[i, 16]^2
      Jad[1, 2] <- Jad[1, 2] + base[i, 19]^2
      Sad[1, 2] <- Sad[1, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 500) & (base[i, 21] < 1000)){
      Jun[2, 1] <- Jun[2, 1] + base[i, 15]
      Sun[2, 1] <- Sun[2, 1] + base[i, 16]
      Jad[2, 1] <- Jad[2, 1] + base[i, 19]
      Sad[2, 1] <- Sad[2, 1] + base[i, 20]
      freq[2] <- freq[2] + 1
      Jun[2, 2] <- Jun[2, 2] + base[i, 15]^2
      Sun[2, 2] <- Sun[2, 2] + base[i, 16]^2
      Jad[2, 2] <- Jad[2, 2] + base[i, 19]^2
      Sad[2, 2] <- Sad[2, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 1000) & (base[i, 21] < 1500)){
      Jun[3, 1] <- Jun[3, 1] + base[i, 15]
      Sun[3, 1] <- Sun[3, 1] + base[i, 16]
      Jad[3, 1] <- Jad[3, 1] + base[i, 19]
      Sad[3, 1] <- Sad[3, 1] + base[i, 20]
      freq[3] <- freq[3] + 1
      Jun[3, 2] <- Jun[3, 2] + base[i, 15]^2
      Sun[3, 2] <- Sun[3, 2] + base[i, 16]^2
      Jad[3, 2] <- Jad[3, 2] + base[i, 19]^2
      Sad[3, 2] <- Sad[3, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 1500) & (base[i, 21] < 2000)){
      Jun[4, 1] <- Jun[4, 1] + base[i, 15]
      Sun[4, 1] <- Sun[4, 1] + base[i, 16]
      Jad[4, 1] <- Jad[4, 1] + base[i, 19]
      Sad[4, 1] <- Sad[4, 1] + base[i, 20]
      freq[4] <- freq[4] + 1
      Jun[4, 2] <- Jun[4, 2] + base[i, 15]^2
      Sun[4, 2] <- Sun[4, 2] + base[i, 16]^2
      Jad[4, 2] <- Jad[4, 2] + base[i, 19]^2
      Sad[4, 2] <- Sad[4, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 2000) & (base[i, 21] < 2500)){
      Jun[5, 1] <- Jun[5, 1] + base[i, 15]
      Sun[5, 1] <- Sun[5, 1] + base[i, 16]
      Jad[5, 1] <- Jad[5, 1] + base[i, 19]
      Sad[5, 1] <- Sad[5, 1] + base[i, 20]
      freq[5] <- freq[5] + 1
      Jun[5, 2] <- Jun[5, 2] + base[i, 15]^2
      Sun[5, 2] <- Sun[5, 2] + base[i, 16]^2
      Jad[5, 2] <- Jad[5, 2] + base[i, 19]^2
      Sad[5, 2] <- Sad[5, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 2500) & (base[i, 21] < 3000)){
      Jun[6, 1] <- Jun[6, 1] + base[i, 15]
      Sun[6, 1] <- Sun[6, 1] + base[i, 16]
      Jad[6, 1] <- Jad[6, 1] + base[i, 19]
      Sad[6, 1] <- Sad[6, 1] + base[i, 20]
      freq[6] <- freq[6] + 1
      Jun[6, 2] <- Jun[6, 2] + base[i, 15]^2
      Sun[6, 2] <- Sun[6, 2] + base[i, 16]^2
      Jad[6, 2] <- Jad[6, 2] + base[i, 19]^2
      Sad[6, 2] <- Sad[6, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 3000) & (base[i, 21] < 3500)){
      Jun[7, 1] <- Jun[7, 1] + base[i, 15]
      Sun[7, 1] <- Sun[7, 1] + base[i, 16]
      Jad[7, 1] <- Jad[7, 1] + base[i, 19]
      Sad[7, 1] <- Sad[7, 1] + base[i, 20]
      freq[7] <- freq[7] + 1
      Jun[7, 2] <- Jun[7, 2] + base[i, 15]^2
      Sun[7, 2] <- Sun[7, 2] + base[i, 16]^2
      Jad[7, 2] <- Jad[7, 2] + base[i, 19]^2
      Sad[7, 2] <- Sad[7, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 3500) & (base[i, 21] < 4000)){
      Jun[8, 1] <- Jun[8, 1] + base[i, 15]
      Sun[8, 1] <- Sun[8, 1] + base[i, 16]
      Jad[8, 1] <- Jad[8, 1] + base[i, 19]
      Sad[8, 1] <- Sad[8, 1] + base[i, 20]
      freq[8] <- freq[8] + 1
      Jun[8, 2] <- Jun[8, 2] + base[i, 15]^2
      Sun[8, 2] <- Sun[8, 2] + base[i, 16]^2
      Jad[8, 2] <- Jad[8, 2] + base[i, 19]^2
      Sad[8, 2] <- Sad[8, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 4000) & (base[i, 21] < 4500)){
      Jun[9, 1] <- Jun[9, 1] + base[i, 15]
      Sun[9, 1] <- Sun[9, 1] + base[i, 16]
      Jad[9, 1] <- Jad[9, 1] + base[i, 19]
      Sad[9, 1] <- Sad[9, 1] + base[i, 20]
      freq[9] <- freq[9] + 1
      Jun[9, 2] <- Jun[9, 2] + base[i, 15]^2
      Sun[9, 2] <- Sun[9, 2] + base[i, 16]^2
      Jad[9, 2] <- Jad[9, 2] + base[i, 19]^2
      Sad[9, 2] <- Sad[9, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 4500) & (base[i, 21] < 5000)){
      Jun[10, 1] <- Jun[10, 1] + base[i, 15]
      Sun[10, 1] <- Sun[10, 1] + base[i, 16]
      Jad[10, 1] <- Jad[10, 1] + base[i, 19]
      Sad[10, 1] <- Sad[10, 1] + base[i, 20]
      freq[10] <- freq[10] + 1
      Jun[10, 2] <- Jun[10, 2] + base[i, 15]^2
      Sun[10, 2] <- Sun[10, 2] + base[i, 16]^2
      Jad[10, 2] <- Jad[10, 2] + base[i, 19]^2
      Sad[10, 2] <- Sad[10, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 5000) & (base[i, 21] < 5500)){
      Jun[11, 1] <- Jun[11, 1] + base[i, 15]
      Sun[11, 1] <- Sun[11, 1] + base[i, 16]
      Jad[11, 1] <- Jad[11, 1] + base[i, 19]
      Sad[11, 1] <- Sad[11, 1] + base[i, 20]
      freq[11] <- freq[11] + 1
      Jun[11, 2] <- Jun[11, 2] + base[i, 15]^2
      Sun[11, 2] <- Sun[11, 2] + base[i, 16]^2
      Jad[11, 2] <- Jad[11, 2] + base[i, 19]^2
      Sad[11, 2] <- Sad[11, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 5500) & (base[i, 21] < 6000)){
      Jun[12, 1] <- Jun[12, 1] + base[i, 15]
      Sun[12, 1] <- Sun[12, 1] + base[i, 16]
      Jad[12, 1] <- Jad[12, 1] + base[i, 19]
      Sad[12, 1] <- Sad[12, 1] + base[i, 20]
      freq[12] <- freq[12] + 1
      Jun[12, 2] <- Jun[12, 2] + base[i, 15]^2
      Sun[12, 2] <- Sun[12, 2] + base[i, 16]^2
      Jad[12, 2] <- Jad[12, 2] + base[i, 19]^2
      Sad[12, 2] <- Sad[12, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 6000) & (base[i, 21] < 6500)){
      Jun[13, 1] <- Jun[13, 1] + base[i, 15]
      Sun[13, 1] <- Sun[13, 1] + base[i, 16]
      Jad[13, 1] <- Jad[13, 1] + base[i, 19]
      Sad[13, 1] <- Sad[13, 1] + base[i, 20]
      freq[13] <- freq[13] + 1
      Jun[13, 2] <- Jun[13, 2] + base[i, 15]^2
      Sun[13, 2] <- Sun[13, 2] + base[i, 16]^2
      Jad[13, 2] <- Jad[13, 2] + base[i, 19]^2
      Sad[13, 2] <- Sad[13, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 6500) & (base[i, 21] < 7000)){
      Jun[14, 1] <- Jun[14, 1] + base[i, 15]
      Sun[14, 1] <- Sun[14, 1] + base[i, 16]
      Jad[14, 1] <- Jad[14, 1] + base[i, 19]
      Sad[14, 1] <- Sad[14, 1] + base[i, 20]
      freq[14] <- freq[14] + 1
      Jun[14, 2] <- Jun[14, 2] + base[i, 15]^2
      Sun[14, 2] <- Sun[14, 2] + base[i, 16]^2
      Jad[14, 2] <- Jad[14, 2] + base[i, 19]^2
      Sad[14, 2] <- Sad[14, 2] + base[i, 20]^2
    }else{
      Jun[15, 1] <- Jun[15, 1] + base[i, 15]
      Sun[15, 1] <- Sun[15, 1] + base[i, 16]
      Jad[15, 1] <- Jad[15, 1] + base[i, 19]
      Sad[15, 1] <- Sad[15, 1] + base[i, 20]
      freq[15] <- freq[15] + 1
      Jun[15, 2] <- Jun[15, 2] + base[i, 15]^2
      Sun[15, 2] <- Sun[15, 2] + base[i, 16]^2
      Jad[15, 2] <- Jad[15, 2] + base[i, 19]^2
      Sad[15, 2] <- Sad[15, 2] + base[i, 20]^2
    }
  }
  
  par(mfrow=c(2, 2), oma = c(0, 0, 3, 0))
  plot(1 - Jun[, 1]/freq, ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard unadjusted")
  mtext(comment(base), outer = TRUE, cex = 1.5)
  segments(x0 = 1:15, y0 = 1 - (Jun[, 1]/freq) - sqrt((Jun[, 2]/(freq) - (Jun[, 1]/freq)^2)), y1 = 1 - (Jun[, 1]/freq) + sqrt((Jun[, 2]/(freq) - (Jun[, 1]/freq)^2)), col = "black")
  #points(1 - (Jun[, 1]/freq) - sqrt((Jun[, 2]/(freq) - (Jun[, 1]/freq)^2)), pch = 20, col = 2)
  #points(1 - (Jun[, 1]/freq) + sqrt((Jun[, 2]/(freq) - (Jun[, 1]/freq)^2)), pch = 20, col = 2)
  plot(1 - Sun[, 1]/freq, ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen unadjusted")
  segments(x0 = 1:15, y0 = 1 - (Sun[, 1]/freq) - sqrt((Sun[, 2]/(freq) - (Sun[, 1]/freq)^2)), y1 = 1 - (Sun[, 1]/freq) + sqrt((Sun[, 2]/(freq) - (Sun[, 1]/freq)^2)), col = "black")
  #points(1 - (Sun[, 1]/freq) - sqrt((Sun[, 2]/(freq) - (Sun[, 1]/freq)^2)), pch = 20, col = 2)
  #points(1 - (Sun[, 1]/freq) + sqrt((Sun[, 2]/(freq) - (Sun[, 1]/freq)^2)), pch = 20, col = 2)
  plot(1 - Jad[, 1]/freq, ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
  segments(x0 = 1:15, y0 = 1 - (Jad[, 1]/freq) - sqrt((Jad[, 2]/(freq) - (Jad[, 1]/freq)^2)), y1 = 1 - (Jad[, 1]/freq) + sqrt((Jad[, 2]/(freq) - (Jad[, 1]/freq)^2)), col = "black")
  #points(1 - (Jad[, 1]/freq) - sqrt((Jad[, 2]/(freq) - (Jad[, 1]/freq)^2)), pch = 20, col = 2)
  #points(1 - (Jad[, 1]/freq) + sqrt((Jad[, 2]/(freq) - (Jad[, 1]/freq)^2)), pch = 20, col = 2)
  plot(1 - Sad[, 1]/freq, ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
  segments(x0 = 1:15, y0 = 1 - (Sad[, 1]/freq) - sqrt((Sad[, 2]/(freq) - (Sad[, 1]/freq)^2)), y1 = 1 - (Sad[, 1]/freq) + sqrt((Sad[, 2]/(freq) - (Sad[, 1]/freq)^2)), col = "black")
  #points(1 - (Sad[, 1]/freq) - sqrt((Sad[, 2]/(freq) - (Sad[, 1]/freq)^2)), pch = 20, col = 2)
  #points(1 - (Sad[, 1]/freq) + sqrt((Sad[, 2]/(freq) - (Sad[, 1]/freq)^2)), pch = 20, col = 2)
  result <- cbind(freq, Jun, Sun, Jad, Sad)
  colnames(result) <- c("freq","Jun", "Jun^2", "Lun", "Lun^2", "Jabd", "Jabd^2", "Labd", "Labd^2")
  return(result) 
}