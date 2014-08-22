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

#For diferent landscapes and biotopes####
wf <- c(29, 30, 31, 32, 33, 35, 37, 38, 40, 41, 43, 44, 47, 48, 50, 51, 52, 53, 54, 55)
wm <- c(73, 80, 81, 83)
wa <- c(22, 26)
tf <- c(34, 49)
tm <- c(57, 58, 59, 61, 62, 63, 69, 71, 72, 74, 76)
ta <- c(3, 4, 5, 6, 7, 28)
sf <- c(36, 39, 42, 45, 46)
sm <- c(60, 64, 65, 66, 67, 70, 78, 79, 82)
sa <- c(2, 8, 10, 11, 12, 13, 20)
mf <- c(56)
mm <- c(68, 75, 77, 84)
ma <- c(1, 9, 14, 15, 16, 17, 18, 19, 21, 23, 24, 25, 27)

#ALL YEARS
forest <- c(wf, tf, sf, mf) 
datafor <- landsc_within(b.div, forest, forest); comment(datafor) <- "DATAFOR"
intfor <- figure.div(datafor)

mixed <- c(wm, tm, sm, mm)
datamix <- landsc_within(b.div, mixed, mixed); comment(datamix) <- "DATAMIX"
intmix <- figure.div(datamix)

meadow <- c(wa, ta, sa, ma)
datamea <- landsc_within(b.div, meadow, meadow); comment(datamea) <- "DATAMEA"
intmea <- figure.div(datamea)

#FIRST YEAR
datafor1 <- landsc_within(b.div1, forest, forest); comment(datafor1) <- "DATAFOR 1"
intfor1 <- figure.div(datafor1)

datamix1 <- landsc_within(b.div1, mixed, mixed); comment(datamix1) <- "DATAMIX 1"
intmix1 <- figure.div(datamix1)

datamea1 <- landsc_within(b.div1, meadow, meadow); comment(datamea1) <- "DATAMEA 1"
intmea1 <- figure.div(datamea1)

#SECOND YEAR
datafor2 <- landsc_within(b.div2, forest, forest); comment(datafor2) <- "DATAFOR 2"
intfor2 <- figure.div(datafor2)

datamix2 <- landsc_within(b.div2, mixed, mixed); comment(datamix2) <- "DATAMIX 2"
intmix2 <- figure.div(datamix2)

datamea2 <- landsc_within(b.div2, meadow, meadow); comment(datamea2) <- "DATAMEA 2"
intmea2 <- figure.div(datamea2)

intf <- rbind(intfor[1:4, ], intfor1[1:4, ], intfor2[1:4, ])
intm <- rbind(intmix[1:4, ], intmix1[1:4, ], intmix2[1:4, ])
inta <- rbind(intmea[1:4, ], intmea1[1:4, ], intmea2[1:4, ])

#DISSIMILARITY (eachlandscape7.tiff)#
##FOREST
par(mfrow=c(3, 2), oma = c(0, 0, 3, 0))
plot(1 - intf[, 6]/intf[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
title("Forest, Mixed and Agriculture", outer = TRUE, cex = 1.5)
segments(x0 = 1:12, y0 = 1 - (intf[, 6]/intf[, 1]) - sqrt((intf[, 7]/(intf[, 1]) - (intf[, 6]/intf[, 1])^2)), y1 = 1 - (intf[, 6]/intf[, 1]) + sqrt((intf[, 7]/(intf[, 1]) - (intf[, 6]/intf[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
abline(v = 4.5, col = "gray")
abline(v = 8.5, col = "gray")
plot(1 - intf[, 8]/intf[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:12, y0 = 1 - (intf[, 8]/intf[, 1]) - sqrt((intf[, 9]/(intf[, 1]) - (intf[, 8]/intf[, 1])^2)), y1 = 1 - (intf[, 8]/intf[, 1]) + sqrt((intf[, 9]/(intf[, 1]) - (intf[, 8]/intf[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
abline(v = 4.5, col = "gray")
abline(v = 8.5, col = "gray")

##MIXED
plot(1 - intm[, 6]/intm[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:12, y0 = 1 - (intm[, 6]/intm[, 1]) - sqrt((intm[, 7]/(intm[, 1]) - (intm[, 6]/intm[, 1])^2)), y1 = 1 - (intm[, 6]/intm[, 1]) + sqrt((intm[, 7]/(intm[, 1]) - (intm[, 6]/intm[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
abline(v = 4.5, col = "gray")
abline(v = 8.5, col = "gray")
plot(1 - intm[, 8]/intf[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:12, y0 = 1 - (intm[, 8]/intm[, 1]) - sqrt((intm[, 9]/(intm[, 1]) - (intm[, 8]/intm[, 1])^2)), y1 = 1 - (intm[, 8]/intm[, 1]) + sqrt((intm[, 9]/(intm[, 1]) - (intm[, 8]/intm[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
abline(v = 4.5, col = "gray")
abline(v = 8.5, col = "gray")

##Agriculture
plot(1 - inta[, 6]/inta[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:12, y0 = 1 - (inta[, 6]/inta[, 1]) - sqrt((inta[, 7]/(inta[, 1]) - (inta[, 6]/inta[, 1])^2)), y1 = 1 - (inta[, 6]/inta[, 1]) + sqrt((inta[, 7]/(inta[, 1]) - (inta[, 6]/inta[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
abline(v = 4.5, col = "gray")
abline(v = 8.5, col = "gray")
plot(1 - inta[, 8]/inta[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:12, y0 = 1 - (inta[, 8]/inta[, 1]) - sqrt((inta[, 9]/(inta[, 1]) - (inta[, 8]/inta[, 1])^2)), y1 = 1 - (inta[, 8]/inta[, 1]) + sqrt((inta[, 9]/(inta[, 1]) - (inta[, 8]/inta[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
abline(v = 4.5, col = "gray")
abline(v = 8.5, col = "gray")

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
