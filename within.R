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

idmea <- 1:28
idmix <- 57:84
idfor <- 29:56

###ALL YEARS####
meadata <- landsc_within(b.div, idmea, idmea); comment(meadata) <- "MEADOW"
mixdata <- landsc_within(b.div, idmix, idmix); comment(mixdata) <- "MIXED"
fordata <- landsc_within(b.div, idfor, idfor); comment(fordata) <- "FOREST"

sum.mea <- land.div(meadata)
sum.mix <- land.div(mixdata)
sum.for <- land.div(fordata)

###FIRST YEAR####
meadata1 <- landsc_within(b.div1, idmea, idmea); comment(meadata1) <- "MEADOW 1"
mixdata1 <- landsc_within(b.div1, idmix, idmix); comment(mixdata1) <- "MIXED 1"
fordata1 <- landsc_within(b.div1, idfor, idfor); comment(fordata1) <- "FOREST 1"

sum.mea1 <- land.div(meadata1)
sum.mix1 <- land.div(mixdata1)
sum.for1 <- land.div(fordata1)

###SECOND YEAR####
meadata2 <- landsc_within(b.div2, idmea, idmea); comment(meadata2) <- "MEADOW 2"
mixdata2 <- landsc_within(b.div2, idmix, idmix); comment(mixdata2) <- "MIXED 2"
fordata2 <- landsc_within(b.div2, idfor, idfor); comment(fordata2) <- "FOREST 2"

sum.mea2 <- land.div(meadata2)
sum.mix2 <- land.div(mixdata2)
sum.for2 <- land.div(fordata2)

### LANDSCAPE vs SCALE (eachlandscape6.tiff) #####
##ALL YEARS
par(mfrow = c(3, 2))
sum.mean <- c(sum.mea[, 6]/sum.mea[, 1], sum.mix[, 6]/sum.mix[, 1], sum.for[, 6]/sum.for[, 1])
sum.sd <- c(sqrt((sum.mea[, 7]/(sum.mea[, 1]) - (sum.mea[, 6]/sum.mea[, 1])^2)), sqrt((sum.mix[, 7]/(sum.mix[, 1]) - (sum.mix[, 6]/sum.mix[, 1])^2)), sqrt((sum.for[, 7]/(sum.for[, 1]) - (sum.for[, 6]/sum.for[, 1])^2)))
plot(1:15, 1 - sum.mean, ylim = c(0, 1), xaxt = "n", xlab = "Agric - Mixed - Forest", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:15, labels = rep(1:5, 3))
abline(v = 5.5, col = "gray")
abline(v = 10.5, col = "gray")
segments(x0 = 1:15, y0 = 1 - sum.mean - sum.sd, y1 = 1 - sum.mean + sum.sd, col = "black")
lines(1:5, rep(mean(1 - sum.mean[1:5]), 5), col = 2)
lines(6:10, rep(mean(1 - sum.mean[6:10]), 5), col = 3)
lines(11:15, rep(mean(1 - sum.mean[11:15]), 5), col = 4)

sum.mean <- c(sum.mea[, 8]/sum.mea[, 1], sum.mix[, 8]/sum.mix[, 1], sum.for[, 8]/sum.for[, 1])
sum.sd <- c(sqrt((sum.mea[, 9]/(sum.mea[, 1]) - (sum.mea[, 8]/sum.mea[, 1])^2)), sqrt((sum.mix[, 9]/(sum.mix[, 1]) - (sum.mix[, 8]/sum.mix[, 1])^2)), sqrt((sum.for[, 9]/(sum.for[, 1]) - (sum.for[, 8]/sum.for[, 1])^2)))
plot(1:15, 1 - sum.mean, ylim = c(0, 1), xaxt = "n", xlab = "Agric - Mixed - Forest", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:15, labels = rep(1:5, 3))
abline(v = 5.5, col = "gray")
abline(v = 10.5, col = "gray")
segments(x0 = 1:15, y0 = 1 - sum.mean - sum.sd, y1 = 1 - sum.mean + sum.sd, col = "black")
lines(1:5, rep(mean(1 - sum.mean[1:5]), 5), col = 2)
lines(6:10, rep(mean(1 - sum.mean[6:10]), 5), col = 3)
lines(11:15, rep(mean(1 - sum.mean[11:15]), 5), col = 4)

##FIRST YEAR
par(mfrow = c(3, 2))
sum.mean1 <- c(sum.mea1[, 6]/sum.mea1[, 1], sum.mix1[, 6]/sum.mix1[, 1], sum.for1[, 6]/sum.for1[, 1])
sum.sd1 <- c(sqrt((sum.mea1[, 7]/(sum.mea1[, 1]) - (sum.mea1[, 6]/sum.mea1[, 1])^2)), sqrt((sum.mix1[, 7]/(sum.mix1[, 1]) - (sum.mix1[, 6]/sum.mix1[, 1])^2)), sqrt((sum.for1[, 7]/(sum.for1[, 1]) - (sum.for1[, 6]/sum.for1[, 1])^2)))
plot(1:15, 1 - sum.mean1, ylim = c(0, 1), xaxt = "n", xlab = "Agric - Mixed - Forest", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:15, labels = rep(1:5, 3))
abline(v = 5.5, col = "gray")
abline(v = 10.5, col = "gray")
segments(x0 = 1:15, y0 = 1 - sum.mean1 - sum.sd1, y1 = 1 - sum.mean1 + sum.sd1, col = "black")
lines(1:5, rep(mean(1 - sum.mean1[1:5]), 5), col = 2)
lines(6:10, rep(mean(1 - sum.mean1[6:10]), 5), col = 3)
lines(11:15, rep(mean(1 - sum.mean1[11:15]), 5), col = 4)

sum.mean1 <- c(sum.mea1[, 8]/sum.mea1[, 1], sum.mix1[, 8]/sum.mix1[, 1], sum.for1[, 8]/sum.for1[, 1])
sum.sd1 <- c(sqrt((sum.mea1[, 9]/(sum.mea1[, 1]) - (sum.mea1[, 8]/sum.mea1[, 1])^2)), sqrt((sum.mix1[, 9]/(sum.mix1[, 1]) - (sum.mix1[, 8]/sum.mix1[, 1])^2)), sqrt((sum.for1[, 9]/(sum.for1[, 1]) - (sum.for1[, 8]/sum.for1[, 1])^2)))
plot(1:15, 1 - sum.mean1, ylim = c(0, 1), xaxt = "n", xlab = "Agric - Mixed - Forest", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:15, labels = rep(1:5, 3))
abline(v = 5.5, col = "gray")
abline(v = 10.5, col = "gray")
segments(x0 = 1:15, y0 = 1 - sum.mean1 - sum.sd1, y1 = 1 - sum.mean1 + sum.sd1, col = "black")
lines(1:5, rep(mean(1 - sum.mean1[1:5]), 5), col = 2)
lines(6:10, rep(mean(1 - sum.mean1[6:10]), 5), col = 3)
lines(11:15, rep(mean(1 - sum.mean1[11:15]), 5), col = 4)

##SECOND YEAR
par(mfrow = c(3, 2))
sum.mean2 <- c(sum.mea2[, 6]/sum.mea2[, 1], sum.mix2[, 6]/sum.mix2[, 1], sum.for2[, 6]/sum.for2[, 1])
sum.sd2 <- c(sqrt((sum.mea2[, 7]/(sum.mea2[, 1]) - (sum.mea2[, 6]/sum.mea2[, 1])^2)), sqrt((sum.mix2[, 7]/(sum.mix2[, 1]) - (sum.mix2[, 6]/sum.mix2[, 1])^2)), sqrt((sum.for2[, 7]/(sum.for2[, 1]) - (sum.for2[, 6]/sum.for2[, 1])^2)))
plot(1:15, 1 - sum.mean2, ylim = c(0, 1), xaxt = "n", xlab = "Agric - Mixed - Forest", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:15, labels = rep(1:5, 3))
abline(v = 5.5, col = "gray")
abline(v = 10.5, col = "gray")
segments(x0 = 1:15, y0 = 1 - sum.mean2 - sum.sd2, y1 = 1 - sum.mean2 + sum.sd2, col = "black")
lines(1:5, rep(mean(1 - sum.mean2[1:5]), 5), col = 2)
lines(6:10, rep(mean(1 - sum.mean2[6:10]), 5), col = 3)
lines(11:15, rep(mean(1 - sum.mean2[11:15]), 5), col = 4)

sum.mean2 <- c(sum.mea2[, 8]/sum.mea2[, 1], sum.mix2[, 8]/sum.mix2[, 1], sum.for2[, 8]/sum.for2[, 1])
sum.sd2 <- c(sqrt((sum.mea2[, 9]/(sum.mea2[, 1]) - (sum.mea2[, 8]/sum.mea2[, 1])^2)), sqrt((sum.mix2[, 9]/(sum.mix2[, 1]) - (sum.mix2[, 8]/sum.mix2[, 1])^2)), sqrt((sum.for2[, 9]/(sum.for2[, 1]) - (sum.for2[, 8]/sum.for2[, 1])^2)))
plot(1:15, 1 - sum.mean2, ylim = c(0, 1), xaxt = "n", xlab = "Agric - Mixed - Forest", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:15, labels = rep(1:5, 3))
abline(v = 5.5, col = "gray")
abline(v = 10.5, col = "gray")
segments(x0 = 1:15, y0 = 1 - sum.mean2 - sum.sd2, y1 = 1 - sum.mean2 + sum.sd2, col = "black")
lines(1:5, rep(mean(1 - sum.mean2[1:5]), 5), col = 2)
lines(6:10, rep(mean(1 - sum.mean2[6:10]), 5), col = 3)
lines(11:15, rep(mean(1 - sum.mean2[11:15]), 5), col = 4)


## WITHIN LANDSCAPE ####
#DISSIMILARITY (dissimilaritywithin.tiff)
par(mfrow = c(3, 2), oma = c(0, 0, 3, 0))
#ALL YEARS
plot(1:3, c(1 - mean(meadata[, 19]), 1 - mean(mixdata[, 19]), 1 - mean(fordata[, 19])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:3, y0 = c(1 - mean(meadata[, 19]) - sd(meadata[, 19]), 1 - mean(mixdata[, 19]) - sd(mixdata[, 19]), 1 - mean(fordata[, 19]) - sd(fordata[, 19])) , y1 = c(1 - mean(meadata[, 19]) + sd(meadata[, 19]), 1 - mean(mixdata[, 19]) + sd(mixdata[, 19]), 1 - mean(fordata[, 19]) + sd(fordata[, 19])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))
plot(1:3, c(1 - mean(meadata[, 20]), 1 - mean(mixdata[, 20]), 1 - mean(fordata[, 20])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:3, y0 = c(1 - mean(meadata[, 20]) - sd(meadata[, 20]), 1 - mean(mixdata[, 20]) - sd(mixdata[, 20]), 1 - mean(fordata[, 20]) - sd(fordata[, 20])) , y1 = c(1 - mean(meadata[, 20]) + sd(meadata[, 20]), 1 - mean(mixdata[, 20]) + sd(mixdata[, 20]), 1 - mean(fordata[, 20]) + sd(fordata[, 20])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))

#FIRST YEAR
plot(1:3, c(1 - mean(meadata1[, 19]), 1 - mean(mixdata1[, 19]), 1 - mean(fordata1[, 19])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:3, y0 = c(1 - mean(meadata1[, 19]) - sd(meadata1[, 19]), 1 - mean(mixdata1[, 19]) - sd(mixdata1[, 19]), 1 - mean(fordata1[, 19]) - sd(fordata1[, 19])) , y1 = c(1 - mean(meadata1[, 19]) + sd(meadata1[, 19]), 1 - mean(mixdata1[, 19]) + sd(mixdata1[, 19]), 1 - mean(fordata1[, 19]) + sd(fordata1[, 19])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))
plot(1:3, c(1 - mean(meadata1[, 20]), 1 - mean(mixdata1[, 20]), 1 - mean(fordata1[, 20])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:3, y0 = c(1 - mean(meadata1[, 20]) - sd(meadata1[, 20]), 1 - mean(mixdata1[, 20]) - sd(mixdata1[, 20]), 1 - mean(fordata1[, 20]) - sd(fordata1[, 20])) , y1 = c(1 - mean(meadata1[, 20]) + sd(meadata1[, 20]), 1 - mean(mixdata1[, 20]) + sd(mixdata1[, 20]), 1 - mean(fordata1[, 20]) + sd(fordata1[, 20])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))

#SECOND YEAR
plot(1:3, c(1 - mean(meadata2[, 19]), 1 - mean(mixdata2[, 19]), 1 - mean(fordata2[, 19])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:3, y0 = c(1 - mean(meadata2[, 19]) - sd(meadata2[, 19]), 1 - mean(mixdata2[, 19]) - sd(mixdata2[, 19]), 1 - mean(fordata2[, 19]) - sd(fordata2[, 19])) , y1 = c(1 - mean(meadata2[, 19]) + sd(meadata2[, 19]), 1 - mean(mixdata2[, 19]) + sd(mixdata2[, 19]), 1 - mean(fordata2[, 19]) + sd(fordata2[, 19])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))
plot(1:3, c(1 - mean(meadata2[, 20]), 1 - mean(mixdata2[, 20]), 1 - mean(fordata2[, 20])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:3, y0 = c(1 - mean(meadata2[, 20]) - sd(meadata2[, 20]), 1 - mean(mixdata2[, 20]) - sd(mixdata2[, 20]), 1 - mean(fordata2[, 20]) - sd(fordata2[, 20])) , y1 = c(1 - mean(meadata2[, 20]) + sd(meadata2[, 20]), 1 - mean(mixdata2[, 20]) + sd(mixdata2[, 20]), 1 - mean(fordata2[, 20]) + sd(fordata2[, 20])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))

#SIMILARITY (similaritywithin.tiff)
#ALL YEARS
plot(1:3, c(mean(meadata[, 19]), mean(mixdata[, 19]), mean(fordata[, 19])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:3, y0 = c(mean(meadata[, 19]) - sd(meadata[, 19]), mean(mixdata[, 19]) - sd(mixdata[, 19]), mean(fordata[, 19]) - sd(fordata[, 19])) , y1 = c(mean(meadata[, 19]) + sd(meadata[, 19]), mean(mixdata[, 19]) + sd(mixdata[, 19]), mean(fordata[, 19]) + sd(fordata[, 19])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))
plot(1:3, c(mean(meadata[, 20]), mean(mixdata[, 20]), mean(fordata[, 20])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:3, y0 = c(mean(meadata[, 20]) - sd(meadata[, 20]), mean(mixdata[, 20]) - sd(mixdata[, 20]), mean(fordata[, 20]) - sd(fordata[, 20])) , y1 = c(mean(meadata[, 20]) + sd(meadata[, 20]), mean(mixdata[, 20]) + sd(mixdata[, 20]), mean(fordata[, 20]) + sd(fordata[, 20])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))

#FIRST YEAR
plot(1:3, c(mean(meadata1[, 19]), mean(mixdata1[, 19]), mean(fordata1[, 19])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:3, y0 = c(mean(meadata1[, 19]) - sd(meadata1[, 19]), mean(mixdata1[, 19]) - sd(mixdata1[, 19]), mean(fordata1[, 19]) - sd(fordata1[, 19])) , y1 = c(mean(meadata1[, 19]) + sd(meadata1[, 19]), mean(mixdata1[, 19]) + sd(mixdata1[, 19]), mean(fordata1[, 19]) + sd(fordata1[, 19])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))
plot(1:3, c(mean(meadata1[, 20]), mean(mixdata1[, 20]), mean(fordata1[, 20])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:3, y0 = c(mean(meadata1[, 20]) - sd(meadata1[, 20]), mean(mixdata1[, 20]) - sd(mixdata1[, 20]), mean(fordata1[, 20]) - sd(fordata1[, 20])) , y1 = c(mean(meadata1[, 20]) + sd(meadata1[, 20]), mean(mixdata1[, 20]) + sd(mixdata1[, 20]), mean(fordata1[, 20]) + sd(fordata1[, 20])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))

#SECOND YEAR
plot(1:3, c(mean(meadata2[, 19]), mean(mixdata2[, 19]), mean(fordata2[, 19])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:3, y0 = c(mean(meadata2[, 19]) - sd(meadata2[, 19]), mean(mixdata2[, 19]) - sd(mixdata2[, 19]), mean(fordata2[, 19]) - sd(fordata2[, 19])) , y1 = c(mean(meadata2[, 19]) + sd(meadata2[, 19]), mean(mixdata2[, 19]) + sd(mixdata2[, 19]), mean(fordata2[, 19]) + sd(fordata2[, 19])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))
plot(1:3, c(mean(meadata2[, 20]), mean(mixdata2[, 20]), mean(fordata2[, 20])), ylim = c(0, 1), xlab = "", xaxt = "n", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:3, y0 = c(mean(meadata2[, 20]) - sd(meadata2[, 20]), mean(mixdata2[, 20]) - sd(mixdata2[, 20]), mean(fordata2[, 20]) - sd(fordata2[, 20])) , y1 = c(mean(meadata2[, 20]) + sd(meadata2[, 20]), mean(mixdata2[, 20]) + sd(mixdata2[, 20]), mean(fordata2[, 20]) + sd(fordata2[, 20])), col = "black")
axis(1, at= 1:3, labels = c("Agric", "Mixed", "Forest"))

##FUNCTION FILTER####
land.div <- function(base){
  #Jun, Sun, Jad, Sad
  Jun <- matrix(0, nrow = 5, ncol = 2)
  Sun <- matrix(0, nrow = 5, ncol = 2)
  Jad <- matrix(0, nrow = 5, ncol = 2)
  Sad <- matrix(0, nrow = 5, ncol = 2)
  freq <- matrix(0, nrow = 5)
  for(i in 1:(dim(base)[1])){
    if(base[i, 21] < 165){
      Jun[1, 1] <- Jun[1, 1] + base[i, 15]
      Sun[1, 1] <- Sun[1, 1] + base[i, 16]
      Jad[1, 1] <- Jad[1, 1] + base[i, 19]
      Sad[1, 1] <- Sad[1, 1] + base[i, 20]
      freq[1] <- freq[1] + 1
      Jun[1, 2] <- Jun[1, 2] + base[i, 15]^2
      Sun[1, 2] <- Sun[1, 2] + base[i, 16]^2
      Jad[1, 2] <- Jad[1, 2] + base[i, 19]^2
      Sad[1, 2] <- Sad[1, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 165) & (base[i, 21] < 325)){
      Jun[2, 1] <- Jun[2, 1] + base[i, 15]
      Sun[2, 1] <- Sun[2, 1] + base[i, 16]
      Jad[2, 1] <- Jad[2, 1] + base[i, 19]
      Sad[2, 1] <- Sad[2, 1] + base[i, 20]
      freq[2] <- freq[2] + 1
      Jun[2, 2] <- Jun[2, 2] + base[i, 15]^2
      Sun[2, 2] <- Sun[2, 2] + base[i, 16]^2
      Jad[2, 2] <- Jad[2, 2] + base[i, 19]^2
      Sad[2, 2] <- Sad[2, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 325) & (base[i, 21] < 645)){
      Jun[3, 1] <- Jun[3, 1] + base[i, 15]
      Sun[3, 1] <- Sun[3, 1] + base[i, 16]
      Jad[3, 1] <- Jad[3, 1] + base[i, 19]
      Sad[3, 1] <- Sad[3, 1] + base[i, 20]
      freq[3] <- freq[3] + 1
      Jun[3, 2] <- Jun[3, 2] + base[i, 15]^2
      Sun[3, 2] <- Sun[3, 2] + base[i, 16]^2
      Jad[3, 2] <- Jad[3, 2] + base[i, 19]^2
      Sad[3, 2] <- Sad[3, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 645) & (base[i, 21] < 1285)){
      Jun[4, 1] <- Jun[4, 1] + base[i, 15]
      Sun[4, 1] <- Sun[4, 1] + base[i, 16]
      Jad[4, 1] <- Jad[4, 1] + base[i, 19]
      Sad[4, 1] <- Sad[4, 1] + base[i, 20]
      freq[4] <- freq[4] + 1
      Jun[4, 2] <- Jun[4, 2] + base[i, 15]^2
      Sun[4, 2] <- Sun[4, 2] + base[i, 16]^2
      Jad[4, 2] <- Jad[4, 2] + base[i, 19]^2
      Sad[4, 2] <- Sad[4, 2] + base[i, 20]^2
    }else if((base[i, 21] >= 1285) & (base[i, 21] < 2000)){
      Jun[5, 1] <- Jun[5, 1] + base[i, 15]
      Sun[5, 1] <- Sun[5, 1] + base[i, 16]
      Jad[5, 1] <- Jad[5, 1] + base[i, 19]
      Sad[5, 1] <- Sad[5, 1] + base[i, 20]
      freq[5] <- freq[5] + 1
      Jun[5, 2] <- Jun[5, 2] + base[i, 15]^2
      Sun[5, 2] <- Sun[5, 2] + base[i, 16]^2
      Jad[5, 2] <- Jad[5, 2] + base[i, 19]^2
      Sad[5, 2] <- Sad[5, 2] + base[i, 20]^2
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