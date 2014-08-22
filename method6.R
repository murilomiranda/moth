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

##between landscapes#####
#ALL YEARS
mea <- landsc_between(b.div, ma, c(mm, mf))
shr <- landsc_between(b.div, sm, c(sa, sf))
tal <- landsc_between(b.div, tm, c(ta, tf))
woo <- landsc_between(b.div, wf, c(wa, wm))
mix <- rbind(shr, tal)

#FIRST YEAR
mea1 <- landsc_between(b.div1, ma, c(mm, mf))
shr1 <- landsc_between(b.div1, sm, c(sa, sf))
tal1 <- landsc_between(b.div1, tm, c(ta, tf))
woo1 <- landsc_between(b.div1, wf, c(wa, wm))
mix1 <- rbind(shr1, tal1)

#SECOND YEAR
mea2 <- landsc_between(b.div2, ma, c(mm, mf))
shr2 <- landsc_between(b.div2, sm, c(sa, sf))
tal2 <- landsc_between(b.div2, tm, c(ta, tf))
woo2 <- landsc_between(b.div2, wf, c(wa, wm))
mix2 <- rbind(shr2, tal2)

#DISSIMILARITY (dissimilaritybetween2.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:3, 1 - c(mean(mea[, 19]), mean(mix[, 19]), mean(woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(mea[, 19]) - sd(mea[, 19]), mean(mix[, 19]) - sd(mix[, 19]), mean(woo[, 19]) - sd(woo[, 19])), y1 = 1 - c(mean(mea[, 19]) + sd(mea[, 19]), mean(mix[, 19]) + sd(mix[, 19]), mean(woo[, 19]) + sd(woo[, 19])), col = 1)

plot(1:3, 1 - c(mean(mea[, 20]), mean(mix[, 20]), mean(woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(mea[, 20]) - sd(mea[, 20]), mean(mix[, 20]) - sd(mix[, 20]), mean(woo[, 20]) - sd(woo[, 20])), y1 = 1 - c(mean(mea[, 20]) + sd(mea[, 20]), mean(mix[, 20]) + sd(mix[, 20]), mean(woo[, 20]) + sd(woo[, 20])), col = 1)

#FIRST YEAR
plot(1:3, 1 - c(mean(mea1[, 19]), mean(mix1[, 19]), mean(woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(mea1[, 19]) - sd(mea1[, 19]), mean(mix1[, 19]) - sd(mix1[, 19]), mean(woo1[, 19]) - sd(woo1[, 19])), y1 = 1 - c(mean(mea1[, 19]) + sd(mea1[, 19]), mean(mix1[, 19]) + sd(mix1[, 19]), mean(woo1[, 19]) + sd(woo1[, 19])), col = 1)

plot(1:3, 1 - c(mean(mea1[, 20]), mean(mix1[, 20]), mean(woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(mea1[, 20]) - sd(mea1[, 20]), mean(mix1[, 20]) - sd(mix1[, 20]), mean(woo1[, 20]) - sd(woo1[, 20])), y1 = 1 - c(mean(mea1[, 20]) + sd(mea1[, 20]), mean(mix1[, 20]) + sd(mix1[, 20]), mean(woo1[, 20]) + sd(woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:3, 1 - c(mean(mea2[, 19]), mean(mix2[, 19]), mean(woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(mea2[, 19]) - sd(mea2[, 19]), mean(mix2[, 19]) - sd(mix2[, 19]), mean(woo2[, 19]) - sd(woo2[, 19])), y1 = 1 - c(mean(mea2[, 19]) + sd(mea2[, 19]), mean(mix2[, 19]) + sd(mix2[, 19]), mean(woo2[, 19]) + sd(woo2[, 19])), col = 1)

plot(1:3, 1 - c(mean(mea2[, 20]), mean(mix2[, 20]), mean(woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = 1 - c(mean(mea2[, 20]) - sd(mea2[, 20]), mean(mix2[, 20]) - sd(mix2[, 20]), mean(woo2[, 20]) - sd(woo2[, 20])), y1 = 1 - c(mean(mea2[, 20]) + sd(mea2[, 20]), mean(mix2[, 20]) + sd(mix2[, 20]), mean(woo2[, 20]) + sd(woo2[, 20])), col = 1)


#SIMILARITY (similaritybetween2.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:3, c(mean(mea[, 19]), mean(mix[, 19]), mean(woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(mea[, 19]) - sd(mea[, 19]), mean(mix[, 19]) - sd(mix[, 19]), mean(woo[, 19]) - sd(woo[, 19])), y1 = c(mean(mea[, 19]) + sd(mea[, 19]), mean(mix[, 19]) + sd(mix[, 19]), mean(woo[, 19]) + sd(woo[, 19])), col = 1)

plot(1:3, c(mean(mea[, 20]), mean(mix[, 20]), mean(woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(mea[, 20]) - sd(mea[, 20]), mean(mix[, 20]) - sd(mix[, 20]), mean(woo[, 20]) - sd(woo[, 20])), y1 = c(mean(mea[, 20]) + sd(mea[, 20]), mean(mix[, 20]) + sd(mix[, 20]), mean(woo[, 20]) + sd(woo[, 20])), col = 1)

#FIRST YEAR
plot(1:3, c(mean(mea1[, 19]), mean(mix1[, 19]), mean(woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(mea1[, 19]) - sd(mea1[, 19]), mean(mix1[, 19]) - sd(mix1[, 19]), mean(woo1[, 19]) - sd(woo1[, 19])), y1 = c(mean(mea1[, 19]) + sd(mea1[, 19]), mean(mix1[, 19]) + sd(mix1[, 19]), mean(woo1[, 19]) + sd(woo1[, 19])), col = 1)

plot(1:3, c(mean(mea1[, 20]), mean(mix1[, 20]), mean(woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(mea1[, 20]) - sd(mea1[, 20]), mean(mix1[, 20]) - sd(mix1[, 20]), mean(woo1[, 20]) - sd(woo1[, 20])), y1 = c(mean(mea1[, 20]) + sd(mea1[, 20]), mean(mix1[, 20]) + sd(mix1[, 20]), mean(woo1[, 20]) + sd(woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:3, c(mean(mea2[, 19]), mean(mix2[, 19]), mean(woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(mea2[, 19]) - sd(mea2[, 19]), mean(mix2[, 19]) - sd(mix2[, 19]), mean(woo2[, 19]) - sd(woo2[, 19])), y1 = c(mean(mea2[, 19]) + sd(mea2[, 19]), mean(mix2[, 19]) + sd(mix2[, 19]), mean(woo2[, 19]) + sd(woo2[, 19])), col = 1)

plot(1:3, c(mean(mea2[, 20]), mean(mix2[, 20]), mean(woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:3, labels = c("Agric", "Mixed", "Forest"))
segments(x0 = 1:3, y0 = c(mean(mea2[, 20]) - sd(mea2[, 20]), mean(mix2[, 20]) - sd(mix2[, 20]), mean(woo2[, 20]) - sd(woo2[, 20])), y1 = c(mean(mea2[, 20]) + sd(mea2[, 20]), mean(mix2[, 20]) + sd(mix2[, 20]), mean(woo2[, 20]) + sd(woo2[, 20])), col = 1)

##with all biotopes
#DISSIMILARITY (dissimilaritybetween3.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:4, 1 - c(mean(mea[, 19]), mean(shr[, 19]), mean(tal[, 19]), mean(woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(mea[, 19]) - sd(mea[, 19]), mean(shr[, 19]) - sd(shr[, 19]), mean(tal[, 19]) - sd(tal[, 19]), mean(woo[, 19]) - sd(woo[, 19])), y1 = 1 - c(mean(mea[, 19]) + sd(mea[, 19]), mean(shr[, 19]) + sd(shr[, 19]), mean(tal[, 19]) + sd(tal[, 19]), mean(woo[, 19]) + sd(woo[, 19])), col = 1)

plot(1:4, 1 - c(mean(mea[, 20]), mean(shr[, 20]), mean(tal[, 20]), mean(woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(mea[, 20]) - sd(mea[, 20]), mean(shr[, 20]) - sd(shr[, 20]), mean(tal[, 20]) - sd(tal[, 20]), mean(woo[, 20]) - sd(woo[, 20])), y1 = 1 - c(mean(mea[, 20]) + sd(mea[, 20]), mean(shr[, 20]) + sd(shr[, 20]), mean(tal[, 20]) + sd(tal[, 20]), mean(woo[, 20]) + sd(woo[, 20])), col = 1)

#FIRST YEAR
plot(1:4, 1 - c(mean(mea1[, 19]), mean(shr1[, 19]), mean(tal1[, 19]), mean(woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(mea1[, 19]) - sd(mea1[, 19]), mean(shr1[, 19]) - sd(shr1[, 19]), mean(tal1[, 19]) - sd(tal1[, 19]), mean(woo1[, 19]) - sd(woo1[, 19])), y1 = 1 - c(mean(mea1[, 19]) + sd(mea1[, 19]), mean(shr1[, 19]) + sd(shr1[, 19]), mean(tal1[, 19]) + sd(tal1[, 19]), mean(woo1[, 19]) + sd(woo1[, 19])), col = 1)

plot(1:4, 1 - c(mean(mea1[, 20]), mean(shr1[, 20]), mean(tal1[, 20]), mean(woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(mea1[, 20]) - sd(mea1[, 20]), mean(shr1[, 20]) - sd(shr1[, 20]), mean(tal1[, 20]) - sd(tal1[, 20]), mean(woo1[, 20]) - sd(woo1[, 20])), y1 = 1 - c(mean(mea1[, 20]) + sd(mea1[, 20]), mean(shr1[, 20]) + sd(shr1[, 20]), mean(tal1[, 20]) + sd(tal1[, 20]), mean(woo1[, 20]) + sd(woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:4, 1 - c(mean(mea2[, 19]), mean(shr2[, 19]), mean(tal2[, 19]), mean(woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(mea2[, 19]) - sd(mea2[, 19]), mean(shr2[, 19]) - sd(shr2[, 19]), mean(tal2[, 19]) - sd(tal2[, 19]), mean(woo2[, 19]) - sd(woo2[, 19])), y1 = 1 - c(mean(mea2[, 19]) + sd(mea2[, 19]), mean(shr2[, 19]) + sd(shr2[, 19]), mean(tal2[, 19]) + sd(tal2[, 19]), mean(woo2[, 19]) + sd(woo2[, 19])), col = 1)

plot(1:4, 1 - c(mean(mea2[, 20]), mean(shr2[, 20]), mean(tal2[, 20]), mean(woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = 1 - c(mean(mea2[, 20]) - sd(mea2[, 20]), mean(shr2[, 20]) - sd(shr2[, 20]), mean(tal2[, 20]) - sd(tal2[, 20]), mean(woo2[, 20]) - sd(woo2[, 20])), y1 = 1 - c(mean(mea2[, 20]) + sd(mea2[, 20]), mean(shr2[, 20]) + sd(shr2[, 20]), mean(tal2[, 20]) + sd(tal2[, 20]), mean(woo2[, 20]) + sd(woo2[, 20])), col = 1)


#SIMILARITY (similaritybetween3.tiff)
par(mfrow=c(3, 2))
#ALL YEARS
plot(1:4, c(mean(mea[, 19]), mean(shr[, 19]), mean(tal[, 19]), mean(woo[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(mea[, 19]) - sd(mea[, 19]), mean(shr[, 19]) - sd(shr[, 19]), mean(tal[, 19]) - sd(tal[, 19]), mean(woo[, 19]) - sd(woo[, 19])), y1 = c(mean(mea[, 19]) + sd(mea[, 19]), mean(shr[, 19]) + sd(shr[, 19]), mean(tal[, 19]) + sd(tal[, 19]), mean(woo[, 19]) + sd(woo[, 19])), col = 1)

plot(1:4, c(mean(mea[, 20]), mean(shr[, 20]), mean(tal[, 20]), mean(woo[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(mea[, 20]) - sd(mea[, 20]), mean(shr[, 20]) - sd(shr[, 20]), mean(tal[, 20]) - sd(tal[, 20]), mean(woo[, 20]) - sd(woo[, 20])), y1 = c(mean(mea[, 20]) + sd(mea[, 20]), mean(shr[, 20]) + sd(shr[, 20]), mean(tal[, 20]) + sd(tal[, 20]), mean(woo[, 20]) + sd(woo[, 20])), col = 1)

#FIRST YEAR
plot(1:4, c(mean(mea1[, 19]), mean(shr1[, 19]), mean(tal1[, 19]), mean(woo1[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(mea1[, 19]) - sd(mea1[, 19]), mean(shr1[, 19]) - sd(shr1[, 19]), mean(tal1[, 19]) - sd(tal1[, 19]), mean(woo1[, 19]) - sd(woo1[, 19])), y1 = c(mean(mea1[, 19]) + sd(mea1[, 19]), mean(shr1[, 19]) + sd(shr1[, 19]), mean(tal1[, 19]) + sd(tal1[, 19]), mean(woo1[, 19]) + sd(woo1[, 19])), col = 1)

plot(1:4, c(mean(mea1[, 20]), mean(shr1[, 20]), mean(tal1[, 20]), mean(woo1[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(mea1[, 20]) - sd(mea1[, 20]), mean(shr1[, 20]) - sd(shr1[, 20]), mean(tal1[, 20]) - sd(tal1[, 20]), mean(woo1[, 20]) - sd(woo1[, 20])), y1 = c(mean(mea1[, 20]) + sd(mea1[, 20]), mean(shr1[, 20]) + sd(shr1[, 20]), mean(tal1[, 20]) + sd(tal1[, 20]), mean(woo1[, 20]) + sd(woo1[, 20])), col = 1)

#SECOND YEAR
plot(1:4, c(mean(mea2[, 19]), mean(shr2[, 19]), mean(tal2[, 19]), mean(woo2[, 19])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(mea2[, 19]) - sd(mea2[, 19]), mean(shr2[, 19]) - sd(shr2[, 19]), mean(tal2[, 19]) - sd(tal2[, 19]), mean(woo2[, 19]) - sd(woo2[, 19])), y1 = c(mean(mea2[, 19]) + sd(mea2[, 19]), mean(shr2[, 19]) + sd(shr2[, 19]), mean(tal2[, 19]) + sd(tal2[, 19]), mean(woo2[, 19]) + sd(woo2[, 19])), col = 1)

plot(1:4, c(mean(mea2[, 20]), mean(shr2[, 20]), mean(tal2[, 20]), mean(woo2[, 20])), xaxt = "n", ylim=c(0, 1), xlab = "", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
axis(1, at = 1:4, labels = c("Agric", "Short", "Tall", "Forest"))
segments(x0 = 1:4, y0 = c(mean(mea2[, 20]) - sd(mea2[, 20]), mean(shr2[, 20]) - sd(shr2[, 20]), mean(tal2[, 20]) - sd(tal2[, 20]), mean(woo2[, 20]) - sd(woo2[, 20])), y1 = c(mean(mea2[, 20]) + sd(mea2[, 20]), mean(shr2[, 20]) + sd(shr2[, 20]), mean(tal2[, 20]) + sd(tal2[, 20]), mean(woo2[, 20]) + sd(woo2[, 20])), col = 1)

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