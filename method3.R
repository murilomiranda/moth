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