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
#ALL YEARS
agr.mea <- landsc_within(b.div, ma, ma)
shr.shr <- landsc_within(b.div, sm, sm)
tal.shr <- landsc_within(b.div, tm, tm)
for.woo <- landsc_within(b.div, wf, wf)
mix.shr <- landsc_within(b.div, c(sm, tm), c(sm, tm))

#FIRST YEAR
agr.mea1 <- landsc_within(b.div1, ma, ma)
shr.shr1 <- landsc_within(b.div1, sm, sm)
tal.shr1 <- landsc_within(b.div1, tm, tm)
for.woo1 <- landsc_within(b.div1, wf, wf)
mix.shr1 <- landsc_within(b.div1, c(sm, tm), c(sm, tm))

#SECOND YEAR
agr.mea2 <- landsc_within(b.div2, ma, ma)
shr.shr2 <- landsc_within(b.div2, sm, sm)
tal.shr2 <- landsc_within(b.div2, tm, tm)
for.woo2 <- landsc_within(b.div2, wf, wf)
mix.shr2 <- landsc_within(b.div2, c(sm, tm), c(sm, tm))

#DISSIMILARITY (dissimilaritywithin2.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:3, 1 - c(mean(agr.mea[, 19]), mean(mix.shr[, 19]), mean(for.woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(agr.mea[, 19]) - sd(agr.mea[, 19]), mean(mix.shr[, 19]) - sd(mix.shr[, 19]), mean(for.woo[, 19]) - sd(for.woo[, 19])), y1 = 1 - c(mean(agr.mea[, 19]) + sd(agr.mea[, 19]), mean(mix.shr[, 19]) + sd(mix.shr[, 19]), mean(for.woo[, 19]) + sd(for.woo[, 19])), col = 1)

plot(1:3, 1 - c(mean(agr.mea[, 20]), mean(mix.shr[, 20]), mean(for.woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(agr.mea[, 20]) - sd(agr.mea[, 20]), mean(mix.shr[, 20]) - sd(mix.shr[, 20]), mean(for.woo[, 20]) - sd(for.woo[, 20])), y1 = 1 - c(mean(agr.mea[, 20]) + sd(agr.mea[, 20]), mean(mix.shr[, 20]) + sd(mix.shr[, 20]), mean(for.woo[, 20]) + sd(for.woo[, 20])), col = 1)

#FIRST YEAR
plot(1:3, 1 - c(mean(agr.mea1[, 19]), mean(mix.shr1[, 19]), mean(for.woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(agr.mea1[, 19]) - sd(agr.mea1[, 19]), mean(mix.shr1[, 19]) - sd(mix.shr1[, 19]), mean(for.woo1[, 19]) - sd(for.woo1[, 19])), y1 = 1 - c(mean(agr.mea1[, 19]) + sd(agr.mea1[, 19]), mean(mix.shr1[, 19]) + sd(mix.shr1[, 19]), mean(for.woo1[, 19]) + sd(for.woo1[, 19])), col = 1)

plot(1:3, 1 - c(mean(agr.mea1[, 20]), mean(mix.shr1[, 20]), mean(for.woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(agr.mea1[, 20]) - sd(agr.mea1[, 20]), mean(mix.shr1[, 20]) - sd(mix.shr1[, 20]), mean(for.woo1[, 20]) - sd(for.woo1[, 20])), y1 = 1 - c(mean(agr.mea1[, 20]) + sd(agr.mea1[, 20]), mean(mix.shr1[, 20]) + sd(mix.shr1[, 20]), mean(for.woo1[, 20]) + sd(for.woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:3, 1 - c(mean(agr.mea2[, 19]), mean(mix.shr2[, 19]), mean(for.woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(agr.mea2[, 19]) - sd(agr.mea2[, 19]), mean(mix.shr2[, 19]) - sd(mix.shr2[, 19]), mean(for.woo2[, 19]) - sd(for.woo2[, 19])), y1 = 1 - c(mean(agr.mea2[, 19]) + sd(agr.mea2[, 19]), mean(mix.shr2[, 19]) + sd(mix.shr2[, 19]), mean(for.woo2[, 19]) + sd(for.woo2[, 19])), col = 1)

plot(1:3, 1 - c(mean(agr.mea2[, 20]), mean(mix.shr2[, 20]), mean(for.woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(agr.mea2[, 20]) - sd(agr.mea2[, 20]), mean(mix.shr2[, 20]) - sd(mix.shr2[, 20]), mean(for.woo2[, 20]) - sd(for.woo2[, 20])), y1 = 1 - c(mean(agr.mea2[, 20]) + sd(agr.mea2[, 20]), mean(mix.shr2[, 20]) + sd(mix.shr2[, 20]), mean(for.woo2[, 20]) + sd(for.woo2[, 20])), col = 1)


#SIMILARITY (similaritywithin2.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:3, c(mean(agr.mea[, 19]), mean(mix.shr[, 19]), mean(for.woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(agr.mea[, 19]) - sd(agr.mea[, 19]), mean(mix.shr[, 19]) - sd(mix.shr[, 19]), mean(for.woo[, 19]) - sd(for.woo[, 19])), y1 = c(mean(agr.mea[, 19]) + sd(agr.mea[, 19]), mean(mix.shr[, 19]) + sd(mix.shr[, 19]), mean(for.woo[, 19]) + sd(for.woo[, 19])), col = 1)

plot(1:3, c(mean(agr.mea[, 20]), mean(mix.shr[, 20]), mean(for.woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(agr.mea[, 20]) - sd(agr.mea[, 20]), mean(mix.shr[, 20]) - sd(mix.shr[, 20]), mean(for.woo[, 20]) - sd(for.woo[, 20])), y1 = c(mean(agr.mea[, 20]) + sd(agr.mea[, 20]), mean(mix.shr[, 20]) + sd(mix.shr[, 20]), mean(for.woo[, 20]) + sd(for.woo[, 20])), col = 1)

#FIRST YEAR
plot(1:3, c(mean(agr.mea1[, 19]), mean(mix.shr1[, 19]), mean(for.woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(agr.mea1[, 19]) - sd(agr.mea1[, 19]), mean(mix.shr1[, 19]) - sd(mix.shr1[, 19]), mean(for.woo1[, 19]) - sd(for.woo1[, 19])), y1 = c(mean(agr.mea1[, 19]) + sd(agr.mea1[, 19]), mean(mix.shr1[, 19]) + sd(mix.shr1[, 19]), mean(for.woo1[, 19]) + sd(for.woo1[, 19])), col = 1)

plot(1:3, c(mean(agr.mea1[, 20]), mean(mix.shr1[, 20]), mean(for.woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(agr.mea1[, 20]) - sd(agr.mea1[, 20]), mean(mix.shr1[, 20]) - sd(mix.shr1[, 20]), mean(for.woo1[, 20]) - sd(for.woo1[, 20])), y1 = c(mean(agr.mea1[, 20]) + sd(agr.mea1[, 20]), mean(mix.shr1[, 20]) + sd(mix.shr1[, 20]), mean(for.woo1[, 20]) + sd(for.woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:3, c(mean(agr.mea2[, 19]), mean(mix.shr2[, 19]), mean(for.woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(agr.mea2[, 19]) - sd(agr.mea2[, 19]), mean(mix.shr2[, 19]) - sd(mix.shr2[, 19]), mean(for.woo2[, 19]) - sd(for.woo2[, 19])), y1 = c(mean(agr.mea2[, 19]) + sd(agr.mea2[, 19]), mean(mix.shr2[, 19]) + sd(mix.shr2[, 19]), mean(for.woo2[, 19]) + sd(for.woo2[, 19])), col = 1)

plot(1:3, c(mean(agr.mea2[, 20]), mean(mix.shr2[, 20]), mean(for.woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(agr.mea2[, 20]) - sd(agr.mea2[, 20]), mean(mix.shr2[, 20]) - sd(mix.shr2[, 20]), mean(for.woo2[, 20]) - sd(for.woo2[, 20])), y1 = c(mean(agr.mea2[, 20]) + sd(agr.mea2[, 20]), mean(mix.shr2[, 20]) + sd(mix.shr2[, 20]), mean(for.woo2[, 20]) + sd(for.woo2[, 20])), col = 1)

##with all biotopes
#DISSIMILARITY (dissimilaritywithin3.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:4, 1 - c(mean(agr.mea[, 19]), mean(shr.shr[, 19]), mean(tal.shr[, 19]), mean(for.woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(agr.mea[, 19]) - sd(agr.mea[, 19]), mean(shr.shr[, 19]) - sd(shr.shr[, 19]), mean(tal.shr[, 19]) - sd(tal.shr[, 19]), mean(for.woo[, 19]) - sd(for.woo[, 19])), y1 = 1 - c(mean(agr.mea[, 19]) + sd(agr.mea[, 19]), mean(shr.shr[, 19]) + sd(shr.shr[, 19]), mean(tal.shr[, 19]) + sd(tal.shr[, 19]), mean(for.woo[, 19]) + sd(for.woo[, 19])), col = 1)

plot(1:4, 1 - c(mean(agr.mea[, 20]), mean(shr.shr[, 20]), mean(tal.shr[, 20]), mean(for.woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(agr.mea[, 20]) - sd(agr.mea[, 20]), mean(shr.shr[, 20]) - sd(shr.shr[, 20]), mean(tal.shr[, 20]) - sd(tal.shr[, 20]), mean(for.woo[, 20]) - sd(for.woo[, 20])), y1 = 1 - c(mean(agr.mea[, 20]) + sd(agr.mea[, 20]), mean(shr.shr[, 20]) + sd(shr.shr[, 20]), mean(tal.shr[, 20]) + sd(tal.shr[, 20]), mean(for.woo[, 20]) + sd(for.woo[, 20])), col = 1)

#FIRST YEAR
plot(1:4, 1 - c(mean(agr.mea1[, 19]), mean(shr.shr1[, 19]), mean(tal.shr1[, 19]), mean(for.woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(agr.mea1[, 19]) - sd(agr.mea1[, 19]), mean(shr.shr1[, 19]) - sd(shr.shr1[, 19]), mean(tal.shr1[, 19]) - sd(tal.shr1[, 19]), mean(for.woo1[, 19]) - sd(for.woo1[, 19])), y1 = 1 - c(mean(agr.mea1[, 19]) + sd(agr.mea1[, 19]), mean(shr.shr1[, 19]) + sd(shr.shr1[, 19]), mean(tal.shr1[, 19]) + sd(tal.shr1[, 19]), mean(for.woo1[, 19]) + sd(for.woo1[, 19])), col = 1)

plot(1:4, 1 - c(mean(agr.mea1[, 20]), mean(shr.shr1[, 20]), mean(tal.shr1[, 20]), mean(for.woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(agr.mea1[, 20]) - sd(agr.mea1[, 20]), mean(shr.shr1[, 20]) - sd(shr.shr1[, 20]), mean(tal.shr1[, 20]) - sd(tal.shr1[, 20]), mean(for.woo1[, 20]) - sd(for.woo1[, 20])), y1 = 1 - c(mean(agr.mea1[, 20]) + sd(agr.mea1[, 20]), mean(shr.shr1[, 20]) + sd(shr.shr1[, 20]), mean(tal.shr1[, 20]) + sd(tal.shr1[, 20]), mean(for.woo1[, 20]) + sd(for.woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:4, 1 - c(mean(agr.mea2[, 19]), mean(shr.shr2[, 19]), mean(tal.shr2[, 19]), mean(for.woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(agr.mea2[, 19]) - sd(agr.mea2[, 19]), mean(shr.shr2[, 19]) - sd(shr.shr2[, 19]), mean(tal.shr2[, 19]) - sd(tal.shr2[, 19]), mean(for.woo2[, 19]) - sd(for.woo2[, 19])), y1 = 1 - c(mean(agr.mea2[, 19]) + sd(agr.mea2[, 19]), mean(shr.shr2[, 19]) + sd(shr.shr2[, 19]), mean(tal.shr2[, 19]) + sd(tal.shr2[, 19]), mean(for.woo2[, 19]) + sd(for.woo2[, 19])), col = 1)

plot(1:4, 1 - c(mean(agr.mea2[, 20]), mean(shr.shr2[, 20]), mean(tal.shr2[, 20]), mean(for.woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(agr.mea2[, 20]) - sd(agr.mea2[, 20]), mean(shr.shr2[, 20]) - sd(shr.shr2[, 20]), mean(tal.shr2[, 20]) - sd(tal.shr2[, 20]), mean(for.woo2[, 20]) - sd(for.woo2[, 20])), y1 = 1 - c(mean(agr.mea2[, 20]) + sd(agr.mea2[, 20]), mean(shr.shr2[, 20]) + sd(shr.shr2[, 20]), mean(tal.shr2[, 20]) + sd(tal.shr2[, 20]), mean(for.woo2[, 20]) + sd(for.woo2[, 20])), col = 1)


#SIMILARITY (similaritywithin3.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:4, c(mean(agr.mea[, 19]), mean(shr.shr[, 19]), mean(tal.shr[, 19]), mean(for.woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(agr.mea[, 19]) - sd(agr.mea[, 19]), mean(shr.shr[, 19]) - sd(shr.shr[, 19]), mean(tal.shr[, 19]) - sd(tal.shr[, 19]), mean(for.woo[, 19]) - sd(for.woo[, 19])), y1 = c(mean(agr.mea[, 19]) + sd(agr.mea[, 19]), mean(shr.shr[, 19]) + sd(shr.shr[, 19]), mean(tal.shr[, 19]) + sd(tal.shr[, 19]), mean(for.woo[, 19]) + sd(for.woo[, 19])), col = 1)

plot(1:4, c(mean(agr.mea[, 20]), mean(shr.shr[, 20]), mean(tal.shr[, 20]), mean(for.woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(agr.mea[, 20]) - sd(agr.mea[, 20]), mean(shr.shr[, 20]) - sd(shr.shr[, 20]), mean(tal.shr[, 20]) - sd(tal.shr[, 20]), mean(for.woo[, 20]) - sd(for.woo[, 20])), y1 = c(mean(agr.mea[, 20]) + sd(agr.mea[, 20]), mean(shr.shr[, 20]) + sd(shr.shr[, 20]), mean(tal.shr[, 20]) + sd(tal.shr[, 20]), mean(for.woo[, 20]) + sd(for.woo[, 20])), col = 1)

#FIRST YEAR
plot(1:4, c(mean(agr.mea1[, 19]), mean(shr.shr1[, 19]), mean(tal.shr1[, 19]), mean(for.woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(agr.mea1[, 19]) - sd(agr.mea1[, 19]), mean(shr.shr1[, 19]) - sd(shr.shr1[, 19]), mean(tal.shr1[, 19]) - sd(tal.shr1[, 19]), mean(for.woo1[, 19]) - sd(for.woo1[, 19])), y1 = c(mean(agr.mea1[, 19]) + sd(agr.mea1[, 19]), mean(shr.shr1[, 19]) + sd(shr.shr1[, 19]), mean(tal.shr1[, 19]) + sd(tal.shr1[, 19]), mean(for.woo1[, 19]) + sd(for.woo1[, 19])), col = 1)

plot(1:4, c(mean(agr.mea1[, 20]), mean(shr.shr1[, 20]), mean(tal.shr1[, 20]), mean(for.woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(agr.mea1[, 20]) - sd(agr.mea1[, 20]), mean(shr.shr1[, 20]) - sd(shr.shr1[, 20]), mean(tal.shr1[, 20]) - sd(tal.shr1[, 20]), mean(for.woo1[, 20]) - sd(for.woo1[, 20])), y1 = c(mean(agr.mea1[, 20]) + sd(agr.mea1[, 20]), mean(shr.shr1[, 20]) + sd(shr.shr1[, 20]), mean(tal.shr1[, 20]) + sd(tal.shr1[, 20]), mean(for.woo1[, 20]) + sd(for.woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:4, c(mean(agr.mea2[, 19]), mean(shr.shr2[, 19]), mean(tal.shr2[, 19]), mean(for.woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(agr.mea2[, 19]) - sd(agr.mea2[, 19]), mean(shr.shr2[, 19]) - sd(shr.shr2[, 19]), mean(tal.shr2[, 19]) - sd(tal.shr2[, 19]), mean(for.woo2[, 19]) - sd(for.woo2[, 19])), y1 = c(mean(agr.mea2[, 19]) + sd(agr.mea2[, 19]), mean(shr.shr2[, 19]) + sd(shr.shr2[, 19]), mean(tal.shr2[, 19]) + sd(tal.shr2[, 19]), mean(for.woo2[, 19]) + sd(for.woo2[, 19])), col = 1)

plot(1:4, c(mean(agr.mea2[, 20]), mean(shr.shr2[, 20]), mean(tal.shr2[, 20]), mean(for.woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(agr.mea2[, 20]) - sd(agr.mea2[, 20]), mean(shr.shr2[, 20]) - sd(shr.shr2[, 20]), mean(tal.shr2[, 20]) - sd(tal.shr2[, 20]), mean(for.woo2[, 20]) - sd(for.woo2[, 20])), y1 = c(mean(agr.mea2[, 20]) + sd(agr.mea2[, 20]), mean(shr.shr2[, 20]) + sd(shr.shr2[, 20]), mean(tal.shr2[, 20]) + sd(tal.shr2[, 20]), mean(for.woo2[, 20]) + sd(for.woo2[, 20])), col = 1)
