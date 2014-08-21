###### EACH LANDSCAPE #############
#### FOR ALL INTERACTIONS
smea <- c(1, 2, 3, 4, 8, 9, 10, 11, 15, 16, 17, 18, 22, 23, 24, 25)
mmea <- c(4, 5, 6, 7, 11, 12, 13, 14, 18, 19, 20, 21, 25, 26, 27, 28)
bmea <- c(7, 14, 18, 28)

smix <- c(57, 58, 59, 60, 64, 65, 66, 67, 71, 72, 73, 74, 78, 79, 80, 81)
mmix <- c(60, 61, 62, 63, 67, 68, 69, 70, 74, 75, 76, 77, 81, 82, 83, 84)
bmix <- c(63, 70, 74, 82)

sfor <- c(29, 30, 31, 32, 36, 37, 38, 39, 43, 44, 45, 46, 50, 51, 52, 53)
mfor <- c(32, 33, 34, 35, 39, 40, 41, 42, 46, 47, 48, 49, 53, 54, 55, 56)
bfor <- c(32, 40, 47, 56)

#ALL YEARS
smeadata <- landsc_within(b.div, smea, smea); comment(smeadata) <- "SMALL MEADOW"
mmeadata <- landsc_within(b.div, mmea, mmea); comment(mmeadata) <- "MEDIUM MEADOW"
bmeadata <- landsc_within(b.div, bmea, bmea); comment(bmeadata) <- "BIG MEADOW"

smixdata <- landsc_within(b.div, smix, smix); comment(smixdata) <- "SMALL MIXED"
mmixdata <- landsc_within(b.div, mmix, mmix); comment(mmixdata) <- "MEDIUM MIXED"
bmixdata <- landsc_within(b.div, bmix, bmix); comment(bmixdata) <- "BIG MIXED"

sfordata <- landsc_within(b.div, sfor, sfor); comment(sfordata) <- "SMALL FOREST"
mfordata <- landsc_within(b.div, mfor, mfor); comment(mfordata) <- "MEDIUM FOREST"
bfordata <- landsc_within(b.div, bfor, bfor); comment(bfordata) <- "BIG FOREST"

#FIRST YEAR
smeadata1 <- landsc_within(b.div1, smea, smea); comment(smeadata1) <- "SMALL MEADOW 1"
mmeadata1 <- landsc_within(b.div1, mmea, mmea); comment(mmeadata1) <- "MEDIUM MEADOW 1"
bmeadata1 <- landsc_within(b.div1, bmea, bmea); comment(bmeadata1) <- "BIG MEADOW 1"

smixdata1 <- landsc_within(b.div1, smix, smix); comment(smixdata1) <- "SMALL MIXED 1"
mmixdata1 <- landsc_within(b.div1, mmix, mmix); comment(mmixdata1) <- "MEDIUM MIXED 1"
bmixdata1 <- landsc_within(b.div1, bmix, bmix); comment(bmixdata1) <- "BIG MIXED 1"

sfordata1 <- landsc_within(b.div1, sfor, sfor); comment(sfordata1) <- "SMALL FOREST 1"
mfordata1 <- landsc_within(b.div1, mfor, mfor); comment(mfordata1) <- "MEDIUM FOREST 1"
bfordata1 <- landsc_within(b.div1, bfor, bfor); comment(bfordata1) <- "BIG FOREST 1"

#SECOND YEAR
smeadata2 <- landsc_within(b.div2, smea, smea); comment(smeadata2) <- "SMALL MEADOW 2"
mmeadata2 <- landsc_within(b.div2, mmea, mmea); comment(mmeadata2) <- "MEDIUM MEADOW 2"
bmeadata2 <- landsc_within(b.div2, bmea, bmea); comment(bmeadata2) <- "BIG MEADOW 2"

smixdata2 <- landsc_within(b.div2, smix, smix); comment(smixdata2) <- "SMALL MIXED 2"
mmixdata2 <- landsc_within(b.div2, mmix, mmix); comment(mmixdata2) <- "MEDIUM MIXED 2"
bmixdata2 <- landsc_within(b.div2, bmix, bmix); comment(bmixdata2) <- "BIG MIXED 2"

sfordata2 <- landsc_within(b.div2, sfor, sfor); comment(sfordata2) <- "SMALL FOREST 2"
mfordata2 <- landsc_within(b.div2, mfor, mfor); comment(mfordata2) <- "MEDIUM FOREST 2"
bfordata2 <- landsc_within(b.div2, bfor, bfor); comment(bfordata2) <- "BIG FOREST 2"

###FOR SIZE (#eachlandscape2.tiff) ####
#ALL YEARS
meadata <- cbind(c(mean(smeadata[, 19]), mean(mmeadata[, 19]), mean(bmeadata[, 19])), c(mean(smeadata[, 20]), mean(mmeadata[, 20]), mean(bmeadata[, 20])), c(sd(smeadata[, 19]), sd(mmeadata[, 19]), sd(bmeadata[, 19])), c(sd(smeadata[, 20]), sd(mmeadata[, 20]), sd(bmeadata[, 20])))
mixdata <- cbind(c(mean(smixdata[, 19]), mean(mmixdata[, 19]), mean(bmixdata[, 19])), c(mean(smixdata[, 20]), mean(mmixdata[, 20]), mean(bmixdata[, 20])), c(sd(smixdata[, 19]), sd(mmixdata[, 19]), sd(bmixdata[, 19])), c(sd(smixdata[, 20]), sd(mmixdata[, 20]), sd(bmixdata[, 20])))
fordata <- cbind(c(mean(sfordata[, 19]), mean(mfordata[, 19]), mean(bfordata[, 19])), c(mean(sfordata[, 20]), mean(mfordata[, 20]), mean(bfordata[, 20])), c(sd(sfordata[, 19]), sd(mfordata[, 19]), sd(bfordata[, 19])), c(sd(sfordata[, 20]), sd(mfordata[, 20]), sd(bfordata[, 20])))
mmfdata <- rbind(meadata, mixdata, fordata)
colnames(mmfdata) <- c("mJad", "mLad", "sdJad", "sdLad")
#(mmfdata <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), mmfdata))
mmfdata <- as.data.frame(mmfdata)

par(mfrow=c(3, 2))
plot(1 - mmfdata$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata$mJad) - mmfdata$sdJad, y1 = (1 - mmfdata$mJad) + mmfdata$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - mmfdata$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata$mLad) - mmfdata$sdLad, y1 = (1 - mmfdata$mLad) + mmfdata$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata$mLad[7:9]), 3), col = 4)

#FIRST YEAR
meadata1 <- cbind(c(mean(smeadata1[, 19]), mean(mmeadata1[, 19]), mean(bmeadata1[, 19])), c(mean(smeadata1[, 20]), mean(mmeadata1[, 20]), mean(bmeadata1[, 20])), c(sd(smeadata1[, 19]), sd(mmeadata1[, 19]), sd(bmeadata1[, 19])), c(sd(smeadata1[, 20]), sd(mmeadata1[, 20]), sd(bmeadata1[, 20])))
mixdata1 <- cbind(c(mean(smixdata1[, 19]), mean(mmixdata1[, 19]), mean(bmixdata1[, 19])), c(mean(smixdata1[, 20]), mean(mmixdata1[, 20]), mean(bmixdata1[, 20])), c(sd(smixdata1[, 19]), sd(mmixdata1[, 19]), sd(bmixdata1[, 19])), c(sd(smixdata1[, 20]), sd(mmixdata1[, 20]), sd(bmixdata1[, 20])))
fordata1 <- cbind(c(mean(sfordata1[, 19]), mean(mfordata1[, 19]), mean(bfordata1[, 19])), c(mean(sfordata1[, 20]), mean(mfordata1[, 20]), mean(bfordata1[, 20])), c(sd(sfordata1[, 19]), sd(mfordata1[, 19]), sd(bfordata1[, 19])), c(sd(sfordata1[, 20]), sd(mfordata1[, 20]), sd(bfordata1[, 20])))
mmfdata1 <- rbind(meadata1, mixdata1, fordata1)
colnames(mmfdata1) <- c("mJad", "mLad", "sdJad", "sdLad")
#(mmfdata1 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), mmfdata1))
mmfdata1 <- as.data.frame(mmfdata1)

plot(1 - mmfdata1$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata1$mJad) - mmfdata1$sdJad, y1 = (1 - mmfdata1$mJad) + mmfdata1$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata1$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata1$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata1$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata1$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata1$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata1$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - mmfdata1$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata1$mLad) - mmfdata1$sdLad, y1 = (1 - mmfdata1$mLad) + mmfdata1$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata1$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata1$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata1$mLad[7:9]), 3), col = 4)

#SECOND YEAR
meadata2 <- cbind(c(mean(smeadata2[, 19]), mean(mmeadata2[, 19]), mean(bmeadata2[, 19])), c(mean(smeadata2[, 20]), mean(mmeadata2[, 20]), mean(bmeadata2[, 20])), c(sd(smeadata2[, 19]), sd(mmeadata2[, 19]), sd(bmeadata2[, 19])), c(sd(smeadata2[, 20]), sd(mmeadata2[, 20]), sd(bmeadata2[, 20])))
mixdata2 <- cbind(c(mean(smixdata2[, 19]), mean(mmixdata2[, 19]), mean(bmixdata2[, 19])), c(mean(smixdata2[, 20]), mean(mmixdata2[, 20]), mean(bmixdata2[, 20])), c(sd(smixdata2[, 19]), sd(mmixdata2[, 19]), sd(bmixdata2[, 19])), c(sd(smixdata2[, 20]), sd(mmixdata2[, 20]), sd(bmixdata2[, 20])))
fordata2 <- cbind(c(mean(sfordata2[, 19]), mean(mfordata2[, 19]), mean(bfordata2[, 19])), c(mean(sfordata2[, 20]), mean(mfordata2[, 20]), mean(bfordata2[, 20])), c(sd(sfordata2[, 19]), sd(mfordata2[, 19]), sd(bfordata2[, 19])), c(sd(sfordata2[, 20]), sd(mfordata2[, 20]), sd(bfordata2[, 20])))
mmfdata2 <- rbind(meadata2, mixdata2, fordata2)
colnames(mmfdata2) <- c("mJad", "mLad", "sdJad", "sdLad")
#(mmfdata2 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), mmfdata2))
mmfdata2 <- as.data.frame(mmfdata2)

plot(1 - mmfdata2$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata2$mJad) - mmfdata2$sdJad, y1 = (1 - mmfdata2$mJad) + mmfdata2$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata2$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata2$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata2$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata2$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata2$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata2$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - mmfdata2$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata2$mLad) - mmfdata2$sdLad, y1 = (1 - mmfdata2$mLad) + mmfdata2$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata2$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata2$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata2$mLad[7:9]), 3), col = 4)

###FOR LANDSCAPE (#eachlandscape4.tiff) #####
#ALL YEARS
smammf <- cbind(c(mean(smeadata[, 19]), mean(smixdata[, 19]), mean(sfordata[, 19])), c(mean(smeadata[, 20]), mean(smixdata[, 20]), mean(sfordata[, 20])), c(sd(smeadata[, 19]), sd(smixdata[, 19]), sd(sfordata[, 19])), c(sd(smeadata[, 20]), sd(smixdata[, 20]), sd(sfordata[, 20])))
medmmf <- cbind(c(mean(mmeadata[, 19]), mean(mmixdata[, 19]), mean(mfordata[, 19])), c(mean(smeadata[, 20]), mean(mmixdata[, 20]), mean(mfordata[, 20])), c(sd(mmeadata[, 19]), sd(mmixdata[, 19]), sd(mfordata[, 19])), c(sd(mmeadata[, 20]), sd(mmixdata[, 20]), sd(mfordata[, 20])))
bigmmf <- cbind(c(mean(bmeadata[, 19]), mean(bmixdata[, 19]), mean(bfordata[, 19])), c(mean(sfordata[, 20]), mean(mmixdata[, 20]), mean(bfordata[, 20])), c(sd(bmeadata[, 19]), sd(bmixdata[, 19]), sd(bfordata[, 19])), c(sd(bmeadata[, 20]), sd(bmixdata[, 20]), sd(bfordata[, 20])))
sizedata <- rbind(smammf, medmmf, bigmmf)
colnames(sizedata) <- c("mJad", "mLad", "sdJad", "sdLad")
#(sizedata <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), sizedata))
sizedata <- as.data.frame(sizedata)

par(mfrow=c(3, 2))
plot(1 - sizedata$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata$mJad) - sizedata$sdJad, y1 = (1 - sizedata$mJad) + sizedata$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata$mJad[7:9]), 3), col = 4)
#modsize1 <- lm(1 - sizedata$mJad[1:3] ~ c(80, 320, 1280)); summary(modsize1)
#modsize2 <- lm(1 - sizedata$mJad[4:6] ~ c(80, 320, 1280)); summary(modsize2)
#modsize3 <- lm(1 - sizedata$mJad[7:9] ~ c(80, 320, 1280)); summary(modsize3)
#lines(1:3, predict(modsize1), col = 2)
#lines(4:6, predict(modsize2), col = 3)
#lines(7:9, predict(modsize3), col = 4)

plot(1 - sizedata$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata$mLad) - sizedata$sdLad, y1 = (1 - sizedata$mLad) + sizedata$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata$mLad[7:9]), 3), col = 4)

#FIRST YEAR
smammf1 <- cbind(c(mean(smeadata1[, 19]), mean(smixdata1[, 19]), mean(sfordata1[, 19])), c(mean(smeadata1[, 20]), mean(smixdata1[, 20]), mean(sfordata1[, 20])), c(sd(smeadata1[, 19]), sd(smixdata1[, 19]), sd(sfordata1[, 19])), c(sd(smeadata1[, 20]), sd(smixdata1[, 20]), sd(sfordata1[, 20])))
medmmf1 <- cbind(c(mean(mmeadata1[, 19]), mean(mmixdata1[, 19]), mean(mfordata1[, 19])), c(mean(smeadata1[, 20]), mean(mmixdata1[, 20]), mean(mfordata1[, 20])), c(sd(mmeadata1[, 19]), sd(mmixdata1[, 19]), sd(mfordata1[, 19])), c(sd(mmeadata1[, 20]), sd(mmixdata1[, 20]), sd(mfordata1[, 20])))
bigmmf1 <- cbind(c(mean(bmeadata1[, 19]), mean(bmixdata1[, 19]), mean(bfordata1[, 19])), c(mean(sfordata1[, 20]), mean(mmixdata1[, 20]), mean(bfordata1[, 20])), c(sd(bmeadata1[, 19]), sd(bmixdata1[, 19]), sd(bfordata1[, 19])), c(sd(bmeadata1[, 20]), sd(bmixdata1[, 20]), sd(bfordata1[, 20])))
sizedata1 <- rbind(smammf1, medmmf1, bigmmf1)
colnames(sizedata1) <- c("mJad", "mLad", "sdJad", "sdLad")
#(sizedata1 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), sizedata1))
sizedata1 <- as.data.frame(sizedata1)

plot(1 - sizedata1$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata1$mJad) - sizedata1$sdJad, y1 = (1 - sizedata1$mJad) + sizedata1$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata1$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata1$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata1$mJad[7:9]), 3), col = 4)
#modsize1 <- lm(1 - sizedata1$mJad[1:3] ~ c(80, 320, 1280)); summary(modsize1)
#modsize2 <- lm(1 - sizedata1$mJad[4:6] ~ c(80, 320, 1280)); summary(modsize2)
#modsize3 <- lm(1 - sizedata1$mJad[7:9] ~ c(80, 320, 1280)); summary(modsize3)
#lines(1:3, predict(modsize1), col = 2)
#lines(4:6, predict(modsize2), col = 3)
#lines(7:9, predict(modsize3), col = 4)

plot(1 - sizedata1$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata1$mLad) - sizedata1$sdLad, y1 = (1 - sizedata1$mLad) + sizedata1$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata1$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata1$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata1$mLad[7:9]), 3), col = 4)

#SECOND YEAR
smammf2 <- cbind(c(mean(smeadata2[, 19]), mean(smixdata2[, 19]), mean(sfordata2[, 19])), c(mean(smeadata2[, 20]), mean(smixdata2[, 20]), mean(sfordata2[, 20])), c(sd(smeadata2[, 19]), sd(smixdata2[, 19]), sd(sfordata2[, 19])), c(sd(smeadata2[, 20]), sd(smixdata2[, 20]), sd(sfordata2[, 20])))
medmmf2 <- cbind(c(mean(mmeadata2[, 19]), mean(mmixdata2[, 19]), mean(mfordata2[, 19])), c(mean(smeadata2[, 20]), mean(mmixdata2[, 20]), mean(mfordata2[, 20])), c(sd(mmeadata2[, 19]), sd(mmixdata2[, 19]), sd(mfordata2[, 19])), c(sd(mmeadata2[, 20]), sd(mmixdata2[, 20]), sd(mfordata2[, 20])))
bigmmf2 <- cbind(c(mean(bmeadata2[, 19]), mean(bmixdata2[, 19]), mean(bfordata2[, 19])), c(mean(sfordata2[, 20]), mean(mmixdata2[, 20]), mean(bfordata2[, 20])), c(sd(bmeadata2[, 19]), sd(bmixdata2[, 19]), sd(bfordata2[, 19])), c(sd(bmeadata2[, 20]), sd(bmixdata2[, 20]), sd(bfordata2[, 20])))
sizedata2 <- rbind(smammf2, medmmf2, bigmmf2)
colnames(sizedata2) <- c("mJad", "mLad", "sdJad", "sdLad")
#(sizedata2 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), sizedata2))
sizedata2 <- as.data.frame(sizedata2)

plot(1 - sizedata2$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata2$mJad) - sizedata2$sdJad, y1 = (1 - sizedata2$mJad) + sizedata2$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata2$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata2$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata2$mJad[7:9]), 3), col = 4)
#modsize1 <- lm(1 - sizedata2$mJad[1:3] ~ c(80, 320, 1280)); summary(modsize1)
#modsize2 <- lm(1 - sizedata2$mJad[4:6] ~ c(80, 320, 1280)); summary(modsize2)
#modsize3 <- lm(1 - sizedata2$mJad[7:9] ~ c(80, 320, 1280)); summary(modsize3)
#lines(1:3, predict(modsize1), col = 2)
#lines(4:6, predict(modsize2), col = 3)
#lines(7:9, predict(modsize3), col = 4)

plot(1 - sizedata2$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata2$mLad) - sizedata2$sdLad, y1 = (1 - sizedata2$mLad) + sizedata2$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata2$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata2$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata2$mLad[7:9]), 3), col = 4)

#### INSIDE INTERACTIONS ####
smea1 <- c(1, 2, 3, 4)
smea2 <- c(8, 9, 10, 11)
smea3 <- c(15, 16, 17, 18)
smea4 <- c(22, 23, 24, 25)
mmea1 <- c(4, 5, 6, 7)
mmea2 <- c(11, 12, 13, 14)
mmea3 <- c(18, 19, 20, 21)
mmea4 <- c(25, 26, 27, 28)
bmea <- c(7, 14, 18, 28)

smix1 <- c(57, 58, 59, 60)
smix2 <- c(64, 65, 66, 67)
smix3 <- c(71, 72, 73, 74)
smix4 <- c(78, 79, 80, 81)
mmix1 <- c(60, 61, 62, 63)
mmix2 <- c(67, 68, 69, 70)
mmix3 <- c(74, 75, 76, 77)
mmix4 <- c(81, 82, 83, 84)
bmix <- c(63, 70, 74, 82)

sfor1 <- c(29, 30, 31, 32)
sfor2 <- c(36, 37, 38, 39)
sfor3 <- c(43, 44, 45, 46)
sfor4 <- c(50, 51, 52, 53)
mfor1 <- c(32, 33, 34, 35)
mfor2 <- c(39, 40, 41, 42)
mfor3 <- c(46, 47, 48, 49)
mfor4 <- c(53, 54, 55, 56)
bfor <- c(32, 40, 47, 56)

#ALL YEARS
smeadata <- rbind(landsc_within(b.div, smea1, smea1), landsc_within(b.div, smea2, smea2), landsc_within(b.div, smea3, smea3), landsc_within(b.div, smea4, smea4))
mmeadata <- rbind(landsc_within(b.div, mmea1, mmea1), landsc_within(b.div, mmea2, mmea2), landsc_within(b.div, mmea3, mmea3), landsc_within(b.div, mmea4, mmea4))
bmeadata <- landsc_within(b.div, bmea, bmea) 
comment(smeadata) <- "SMALL MEADOW"
comment(mmeadata) <- "MEDIUM MEADOW"
comment(bmeadata) <- "BIG MEADOW"

smixdata <- rbind(landsc_within(b.div, smix1, smix1), landsc_within(b.div, smix2, smix2), landsc_within(b.div, smix3, smix3), landsc_within(b.div, smix4, smix4)) 
mmixdata <- rbind(landsc_within(b.div, mmix1, mmix1), landsc_within(b.div, mmix2, mmix2), landsc_within(b.div, mmix3, mmix3), landsc_within(b.div, mmix4, mmix4))
bmixdata <- landsc_within(b.div, bmix, bmix)
comment(smixdata) <- "SMALL MIXED"
comment(mmixdata) <- "MEDIUM MIXED"
comment(bmixdata) <- "BIG MIXED"

sfordata <- rbind(landsc_within(b.div, sfor1, sfor1), landsc_within(b.div, sfor2, sfor2), landsc_within(b.div, sfor3, sfor3), landsc_within(b.div, sfor4, sfor4))
mfordata <- rbind(landsc_within(b.div, mfor1, mfor1), landsc_within(b.div, mfor2, mfor2), landsc_within(b.div, mfor3, mfor3), landsc_within(b.div, mfor4, mfor4))
bfordata <- landsc_within(b.div, bfor, bfor) 
comment(sfordata) <- "SMALL FOREST"
comment(mfordata) <- "MEDIUM FOREST"
comment(bfordata) <- "BIG FOREST"

#FIRST YEAR
smeadata1 <- rbind(landsc_within(b.div1, smea1, smea1), landsc_within(b.div1, smea2, smea2), landsc_within(b.div1, smea3, smea3), landsc_within(b.div1, smea4, smea4))
mmeadata1 <- rbind(landsc_within(b.div1, mmea1, mmea1), landsc_within(b.div1, mmea2, mmea2), landsc_within(b.div1, mmea3, mmea3), landsc_within(b.div1, mmea4, mmea4))
bmeadata1 <- landsc_within(b.div1, bmea, bmea)
comment(smeadata1) <- "SMALL MEADOW 1"
comment(mmeadata1) <- "MEDIUM MEADOW 1"
comment(bmeadata1) <- "BIG MEADOW 1"

smixdata1 <- rbind(landsc_within(b.div1, smix1, smix1), landsc_within(b.div1, smix2, smix2), landsc_within(b.div1, smix3, smix3), landsc_within(b.div1, smix4, smix4))
mmixdata1 <- rbind(landsc_within(b.div1, mmix1, mmix1), landsc_within(b.div1, mmix2, mmix2), landsc_within(b.div1, mmix3, mmix3), landsc_within(b.div1, mmix4, mmix4))
bmixdata1 <- landsc_within(b.div1, bmix, bmix)
comment(smixdata1) <- "SMALL MIXED 1"
comment(mmixdata1) <- "MEDIUM MIXED 1"
comment(bmixdata1) <- "BIG MIXED 1"

sfordata1 <- rbind(landsc_within(b.div1, sfor1, sfor1), landsc_within(b.div1, sfor2, sfor2), landsc_within(b.div1, sfor3, sfor3), landsc_within(b.div1, sfor4, sfor4))
mfordata1 <- rbind(landsc_within(b.div1, mfor1, mfor1), landsc_within(b.div1, mfor2, mfor2), landsc_within(b.div1, mfor3, mfor3), landsc_within(b.div1, mfor4, mfor4))
bfordata1 <- landsc_within(b.div1, bfor, bfor)
comment(sfordata1) <- "SMALL FOREST 1"
comment(mfordata1) <- "MEDIUM FOREST 1"
comment(bfordata1) <- "BIG FOREST 1"

#SECOND YEAR
smeadata2 <- rbind(landsc_within(b.div2, smea1, smea1), landsc_within(b.div2, smea2, smea2), landsc_within(b.div2, smea3, smea3), landsc_within(b.div2, smea4, smea4))
mmeadata2 <- rbind(landsc_within(b.div2, mmea1, mmea1), landsc_within(b.div2, mmea2, mmea2), landsc_within(b.div2, mmea3, mmea3), landsc_within(b.div2, mmea4, mmea4))
bmeadata2 <- landsc_within(b.div2, bmea, bmea)
comment(smeadata2) <- "SMALL MEADOW 2"
comment(mmeadata2) <- "MEDIUM MEADOW 2"
comment(bmeadata2) <- "BIG MEADOW 2"

smixdata2 <- rbind(landsc_within(b.div2, smix1, smix1), landsc_within(b.div2, smix2, smix2), landsc_within(b.div2, smix3, smix3), landsc_within(b.div2, smix4, smix4))
mmixdata2 <- rbind(landsc_within(b.div2, mmix1, mmix1), landsc_within(b.div2, mmix2, mmix2), landsc_within(b.div2, mmix3, mmix3), landsc_within(b.div2, mmix4, mmix4))
bmixdata2 <- landsc_within(b.div2, bmix, bmix)
comment(smixdata2) <- "SMALL MIXED 2"
comment(mmixdata2) <- "MEDIUM MIXED 2"
comment(bmixdata2) <- "BIG MIXED 2"

sfordata2 <- rbind(landsc_within(b.div2, sfor1, sfor1), landsc_within(b.div2, sfor2, sfor2), landsc_within(b.div2, sfor3, sfor3), landsc_within(b.div2, sfor4, sfor4))
mfordata2 <- rbind(landsc_within(b.div2, mfor1, mfor1), landsc_within(b.div2, mfor2, mfor2), landsc_within(b.div2, mfor3, mfor3), landsc_within(b.div2, mfor4, mfor4))
bfordata2 <- landsc_within(b.div2, bfor, bfor)
comment(sfordata2) <- "SMALL FOREST 2"
comment(mfordata2) <- "MEDIUM FOREST 2"
comment(bfordata2) <- "BIG FOREST 2"

###FOR SIZE (#eachlandscape3.tiff) #####
#ALL YEARS
meadata <- cbind(c(mean(smeadata[, 19]), mean(mmeadata[, 19]), mean(bmeadata[, 19])), c(mean(smeadata[, 20]), mean(mmeadata[, 20]), mean(bmeadata[, 20])), c(sd(smeadata[, 19]), sd(mmeadata[, 19]), sd(bmeadata[, 19])), c(sd(smeadata[, 20]), sd(mmeadata[, 20]), sd(bmeadata[, 20])))
mixdata <- cbind(c(mean(smixdata[, 19]), mean(mmixdata[, 19]), mean(bmixdata[, 19])), c(mean(smixdata[, 20]), mean(mmixdata[, 20]), mean(bmixdata[, 20])), c(sd(smixdata[, 19]), sd(mmixdata[, 19]), sd(bmixdata[, 19])), c(sd(smixdata[, 20]), sd(mmixdata[, 20]), sd(bmixdata[, 20])))
fordata <- cbind(c(mean(sfordata[, 19]), mean(mfordata[, 19]), mean(bfordata[, 19])), c(mean(sfordata[, 20]), mean(mfordata[, 20]), mean(bfordata[, 20])), c(sd(sfordata[, 19]), sd(mfordata[, 19]), sd(bfordata[, 19])), c(sd(sfordata[, 20]), sd(mfordata[, 20]), sd(bfordata[, 20])))
mmfdata <- rbind(meadata, mixdata, fordata)
colnames(mmfdata) <- c("mJad", "mLad", "sdJad", "sdLad")
#(mmfdata <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), mmfdata))
mmfdata <- as.data.frame(mmfdata)

par(mfrow=c(3, 2))
plot(1 - mmfdata$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata$mJad) - mmfdata$sdJad, y1 = (1 - mmfdata$mJad) + mmfdata$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - mmfdata$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata$mLad) - mmfdata$sdLad, y1 = (1 - mmfdata$mLad) + mmfdata$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata$mLad[7:9]), 3), col = 4)

#FIRST YEAR
meadata1 <- cbind(c(mean(smeadata1[, 19]), mean(mmeadata1[, 19]), mean(bmeadata1[, 19])), c(mean(smeadata1[, 20]), mean(mmeadata1[, 20]), mean(bmeadata1[, 20])), c(sd(smeadata1[, 19]), sd(mmeadata1[, 19]), sd(bmeadata1[, 19])), c(sd(smeadata1[, 20]), sd(mmeadata1[, 20]), sd(bmeadata1[, 20])))
mixdata1 <- cbind(c(mean(smixdata1[, 19]), mean(mmixdata1[, 19]), mean(bmixdata1[, 19])), c(mean(smixdata1[, 20]), mean(mmixdata1[, 20]), mean(bmixdata1[, 20])), c(sd(smixdata1[, 19]), sd(mmixdata1[, 19]), sd(bmixdata1[, 19])), c(sd(smixdata1[, 20]), sd(mmixdata1[, 20]), sd(bmixdata1[, 20])))
fordata1 <- cbind(c(mean(sfordata1[, 19]), mean(mfordata1[, 19]), mean(bfordata1[, 19])), c(mean(sfordata1[, 20]), mean(mfordata1[, 20]), mean(bfordata1[, 20])), c(sd(sfordata1[, 19]), sd(mfordata1[, 19]), sd(bfordata1[, 19])), c(sd(sfordata1[, 20]), sd(mfordata1[, 20]), sd(bfordata1[, 20])))
mmfdata1 <- rbind(meadata1, mixdata1, fordata1)
colnames(mmfdata1) <- c("mJad", "mLad", "sdJad", "sdLad")
#(mmfdata1 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), mmfdata1))
mmfdata1 <- as.data.frame(mmfdata1)

plot(1 - mmfdata1$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata1$mJad) - mmfdata1$sdJad, y1 = (1 - mmfdata1$mJad) + mmfdata1$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata1$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata1$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata1$mJad[7:9]), 3), col = 4)
#modsize1 <- lm(1 - sizedata1$mJad[1:3] ~ c(80, 320, 1280)); summary(modsize1)
#modsize2 <- lm(1 - sizedata1$mJad[4:6] ~ c(80, 320, 1280)); summary(modsize2)
#modsize3 <- lm(1 - sizedata1$mJad[7:9] ~ c(80, 320, 1280)); summary(modsize3)
#lines(1:3, predict(modsize1), col = 2)
#lines(4:6, predict(modsize2), col = 3)
#lines(7:9, predict(modsize3), col = 4)

plot(1 - mmfdata1$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata1$mLad) - mmfdata1$sdLad, y1 = (1 - mmfdata1$mLad) + mmfdata1$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata1$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata1$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata1$mLad[7:9]), 3), col = 4)

#SECOND YEAR
meadata2 <- cbind(c(mean(smeadata2[, 19]), mean(mmeadata2[, 19]), mean(bmeadata2[, 19])), c(mean(smeadata2[, 20]), mean(mmeadata2[, 20]), mean(bmeadata2[, 20])), c(sd(smeadata2[, 19]), sd(mmeadata2[, 19]), sd(bmeadata2[, 19])), c(sd(smeadata2[, 20]), sd(mmeadata2[, 20]), sd(bmeadata2[, 20])))
mixdata2 <- cbind(c(mean(smixdata2[, 19]), mean(mmixdata2[, 19]), mean(bmixdata2[, 19])), c(mean(smixdata2[, 20]), mean(mmixdata2[, 20]), mean(bmixdata2[, 20])), c(sd(smixdata2[, 19]), sd(mmixdata2[, 19]), sd(bmixdata2[, 19])), c(sd(smixdata2[, 20]), sd(mmixdata2[, 20]), sd(bmixdata2[, 20])))
fordata2 <- cbind(c(mean(sfordata2[, 19]), mean(mfordata2[, 19]), mean(bfordata2[, 19])), c(mean(sfordata2[, 20]), mean(mfordata2[, 20]), mean(bfordata2[, 20])), c(sd(sfordata2[, 19]), sd(mfordata2[, 19]), sd(bfordata2[, 19])), c(sd(sfordata2[, 20]), sd(mfordata2[, 20]), sd(bfordata2[, 20])))
mmfdata2 <- rbind(meadata2, mixdata2, fordata2)
colnames(mmfdata2) <- c("mJad", "mLad", "sdJad", "sdLad")
#(mmfdata2 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), mmfdata2))
mmfdata2 <- as.data.frame(mmfdata2)

plot(1 - mmfdata2$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata2$mJad) - mmfdata2$sdJad, y1 = (1 - mmfdata2$mJad) + mmfdata2$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata2$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata2$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata2$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata2$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata2$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata2$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - mmfdata2$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Agric - Mixed - Forest")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - mmfdata2$mLad) - mmfdata2$sdLad, y1 = (1 - mmfdata2$mLad) + mmfdata2$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("small", "medium", "big"), 3))
lines(1:3, rep(mean(1 - mmfdata2$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - mmfdata2$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - mmfdata2$mLad[7:9]), 3), col = 4)

###FOR LANDSCAPE (#eachlandscape5.tiff) #####
#ALL YEARS
smadata <- cbind(c(mean(smeadata[, 19]), mean(smixdata[, 19]), mean(sfordata[, 19])), c(mean(smeadata[, 20]), mean(smixdata[, 20]), mean(sfordata[, 20])), c(sd(smeadata[, 19]), sd(smixdata[, 19]), sd(sfordata[, 19])), c(sd(smeadata[, 20]), sd(smixdata[, 20]), sd(sfordata[, 20])))
meddata <- cbind(c(mean(mmeadata[, 19]), mean(mmixdata[, 19]), mean(mfordata[, 19])), c(mean(mmeadata[, 20]), mean(mmixdata[, 20]), mean(mfordata[, 20])), c(sd(mmeadata[, 19]), sd(mmixdata[, 19]), sd(mfordata[, 19])), c(sd(mmeadata[, 20]), sd(mmixdata[, 20]), sd(mfordata[, 20])))
bigdata <- cbind(c(mean(bmeadata[, 19]), mean(bmixdata[, 19]), mean(bfordata[, 19])), c(mean(bmeadata[, 20]), mean(bmixdata[, 20]), mean(bfordata[, 20])), c(sd(bmeadata[, 19]), sd(bmixdata[, 19]), sd(bfordata[, 19])), c(sd(bmeadata[, 20]), sd(bmixdata[, 20]), sd(bfordata[, 20])))
sizedata <- rbind(smadata, meddata, bigdata)
colnames(sizedata) <- c("mJad", "mLad", "sdJad", "sdLad")
#(sizedata <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), sizedata))
sizedata <- as.data.frame(sizedata)

par(mfrow=c(3, 2))
plot(1 - sizedata$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata$mJad) - sizedata$sdJad, y1 = (1 - sizedata$mJad) + sizedata$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata$mJad[7:9]), 3), col = 4)
#modsize1 <- lm(1 - sizedata$mJad[1:3] ~ c(80, 320, 1280)); summary(modsize1)
#modsize2 <- lm(1 - sizedata$mJad[4:6] ~ c(80, 320, 1280)); summary(modsize2)
#modsize3 <- lm(1 - sizedata$mJad[7:9] ~ c(80, 320, 1280)); summary(modsize3)
#lines(1:3, predict(modsize1), col = 2)
#lines(4:6, predict(modsize2), col = 3)
#lines(7:9, predict(modsize3), col = 4)

plot(1 - sizedata$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata$mLad) - sizedata$sdLad, y1 = (1 - sizedata$mLad) + sizedata$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata$mLad[7:9]), 3), col = 4)

#FIRST YEAR
smadata1 <- cbind(c(mean(smeadata1[, 19]), mean(smixdata1[, 19]), mean(sfordata1[, 19])), c(mean(smeadata1[, 20]), mean(smixdata1[, 20]), mean(sfordata1[, 20])), c(sd(smeadata1[, 19]), sd(smixdata1[, 19]), sd(sfordata1[, 19])), c(sd(smeadata1[, 20]), sd(smixdata1[, 20]), sd(sfordata1[, 20])))
meddata1 <- cbind(c(mean(mmeadata1[, 19]), mean(mmixdata1[, 19]), mean(mfordata1[, 19])), c(mean(mmeadata1[, 20]), mean(mmixdata1[, 20]), mean(mfordata1[, 20])), c(sd(mmeadata1[, 19]), sd(mmixdata1[, 19]), sd(mfordata1[, 19])), c(sd(mmeadata1[, 20]), sd(mmixdata1[, 20]), sd(mfordata1[, 20])))
bigdata1 <- cbind(c(mean(bmeadata1[, 19]), mean(bmixdata1[, 19]), mean(bfordata1[, 19])), c(mean(bmeadata1[, 20]), mean(bmixdata1[, 20]), mean(bfordata1[, 20])), c(sd(bmeadata1[, 19]), sd(bmixdata1[, 19]), sd(bfordata1[, 19])), c(sd(bmeadata1[, 20]), sd(bmixdata1[, 20]), sd(bfordata1[, 20])))
sizedata1 <- rbind(smadata1, meddata1, bigdata1)
colnames(sizedata1) <- c("mJad", "mLad", "sdJad", "sdLad")
#(sizedata1 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), sizedata1))
sizedata1 <- as.data.frame(sizedata1)

plot(1 - sizedata1$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata1$mJad) - sizedata1$sdJad, y1 = (1 - sizedata1$mJad) + sizedata1$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata1$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata1$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata1$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata1$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata1$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata1$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - sizedata1$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata1$mLad) - sizedata1$sdLad, y1 = (1 - sizedata1$mLad) + sizedata1$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata1$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata1$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata1$mLad[7:9]), 3), col = 4)

#SECOND YEAR
smadata2 <- cbind(c(mean(smeadata2[, 19]), mean(smixdata2[, 19]), mean(sfordata2[, 19])), c(mean(smeadata2[, 20]), mean(smixdata2[, 20]), mean(sfordata2[, 20])), c(sd(smeadata2[, 19]), sd(smixdata2[, 19]), sd(sfordata2[, 19])), c(sd(smeadata2[, 20]), sd(smixdata2[, 20]), sd(sfordata2[, 20])))
meddata2 <- cbind(c(mean(mmeadata2[, 19]), mean(mmixdata2[, 19]), mean(mfordata2[, 19])), c(mean(mmeadata2[, 20]), mean(mmixdata2[, 20]), mean(mfordata2[, 20])), c(sd(mmeadata2[, 19]), sd(mmixdata2[, 19]), sd(mfordata2[, 19])), c(sd(mmeadata2[, 20]), sd(mmixdata2[, 20]), sd(mfordata2[, 20])))
bigdata2 <- cbind(c(mean(bmeadata2[, 19]), mean(bmixdata2[, 19]), mean(bfordata2[, 19])), c(mean(bmeadata2[, 20]), mean(bmixdata2[, 20]), mean(bfordata2[, 20])), c(sd(bmeadata2[, 19]), sd(bmixdata2[, 19]), sd(bfordata2[, 19])), c(sd(bmeadata2[, 20]), sd(bmixdata2[, 20]), sd(bfordata2[, 20])))
sizedata2 <- rbind(smadata2, meddata2, bigdata2)
colnames(sizedata2) <- c("mJad", "mLad", "sdJad", "sdLad")
#(sizedata2 <- cbind(rep(c("small", "medium", "big"), 3), rep(c("agric", "mixed", "forest"), each = 3), sizedata2))
sizedata2 <- as.data.frame(sizedata2)

plot(1 - sizedata2$mJad, ylim=c(0, 1), ylab = "dissimilarity", main = "Jaccard", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata2$mJad) - sizedata2$sdJad, y1 = (1 - sizedata2$mJad) + sizedata2$sdJad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata2$mJad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata2$mJad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata2$mJad[7:9]), 3), col = 4)
#modmmf1 <- lm(1 - mmfdata2$mJad[1:3] ~ c(80, 320, 1280)); summary(modmmf1)
#modmmf2 <- lm(1 - mmfdata2$mJad[4:6] ~ c(80, 320, 1280)); summary(modmmf2)
#modmmf3 <- lm(1 - mmfdata2$mJad[7:9] ~ c(80, 320, 1280)); summary(modmmf3)
#lines(1:3, predict(modmmf1), col = 2)
#lines(4:6, predict(modmmf2), col = 3)
#lines(7:9, predict(modmmf3), col = 4)

plot(1 - sizedata2$mLad, ylim=c(0, 1), ylab = "dissimilarity", main = "Sorensen", xaxt = "n", xlab = "Small - Medium - Big")
abline(h = 0.5, cex = .5, col = "gray")
segments(x0 = 1:9, y0 = (1 - sizedata2$mLad) - sizedata2$sdLad, y1 = (1 - sizedata2$mLad) + sizedata2$sdLad, col = "black")
axis(1, at = 1:9, labels= rep(c("agric", "mixed", "forest"), 3))
lines(1:3, rep(mean(1 - sizedata2$mLad[1:3]), 3), col = 2)
lines(4:6, rep(mean(1 - sizedata2$mLad[4:6]), 3), col = 3)
lines(7:9, rep(mean(1 - sizedata2$mLad[7:9]), 3), col = 4)


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
