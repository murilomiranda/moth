#READ DISTANCE BETWEEN TRAPS####
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

#VERIFY IF THE SAME SPECIES APPEARS MORE THAN TWICE IN THE SAME PERIOD####
for(p in 1:6){
  for(s1 in 1:84){
    species_s1 <- cbind(as.vector(data[data[, 2] == s1 & data[, 3] == p, 4]), data[data[, 2] == s1 & data[, 3] == p, 5])
    if(length(union(species_s1[, 1], species_s1[, 1])) != dim(species_s1)[1]){
      cat("P", p, "S1", s1, "\n")
    }
  }
}

#Calculate the b-diversity for each period####
#p - period
#s1 and s2 - sampling point
#n and m - no. of observed individuals in sampling points s1 and s2
#nsp1 and nsp2 - no. of observed species in sampling points s1 and s2
#nsh - no. of observed shared species in s1 and s2
#fm1 - the observed number of shared species that occur once in s2
#fm2 - the observed number of shared species that occur twice in s2
#f1m - the observed number of shared species that occur once in s1
#f2m - the observed number of shared species that occur twice in s1
#u_pt1 - U unadjusted
#v_pt1 - V unadjusted
#Junad - Jaccard estimator unadjusted
#Lunad - Sorensen estimator unadjusted
#U - 
#V -
#Jabd - Jaccard estimator adjusted
#Labd - Sorensen estimator adjusted
source('chaom.R')
period1 <- chao(1, 1, "tperiod1")
comment(period1) <- "Period1"
period2 <- chao(2, 2, "tperiod2")
comment(period2) <- "Period2"
period3 <- chao(3, 3, "tperiod3")
comment(period3) <- "Period3"
period4 <- chao(4, 4, "tperiod4")
comment(period4) <- "Period4"
period5 <- chao(5, 5, "tperiod5")
comment(period5) <- "Period5"
period6 <- chao(6, 6, "tperiod6")
comment(period6) <- "Period6"

## For first year
period123 <- trisamples(period1, period2, period3)
comment(period123) <- "Year 1"

## For second year
period456 <- trisamples(period4, period5, period6)
comment(period456) <- "Year 2"

## For all traps
period.all <- bisamples(period123, period456)
comment(period.all) <- "All"

figure.year(period1)
figure.year(period2)
figure.year(period3)
figure.year(period4)
figure.year(period5)
figure.year(period6)
figure.year(period123)
figure.year(period456)
figure.year(period.all)

#SIMILARITY####
par(mfrow=c(3, 3))
plot(DIST1[, 3], period1[, 19], main = "Period 1", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period2[, 19], main = "Period 2", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period3[, 19], main = "Period 3", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period4[, 19], main = "Period 4", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period5[, 19], main = "Period 5", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period6[, 19], main = "Period 6", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period123[, 19], main = "First Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period456[, 19], main = "Second Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], period.all[, 19], main = "ALL", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#DISSIMILARITY####
par(mfrow=c(3, 3))
plot(DIST1[, 3], 1 - period1[, 19], main = "Period 1", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period2[, 19], main = "Period 2", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period3[, 19], main = "Period 3", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period4[, 19], main = "Period 4", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period5[, 19], main = "Period 5", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period6[, 19], main = "Period 6", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period123[, 19], main = "First Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period456[, 19], main = "Second Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(DIST1[, 3], 1 - period.all[, 19], main = "ALL", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#BIG-DATASET####
b.div <- rbind(cbind(period1, DIST1[, 3], DIST2[, 3]), cbind(period2, DIST1[, 3], DIST2[, 3]), 
               cbind(period3, DIST1[, 3], DIST2[, 3]), cbind(period4, DIST1[, 3], DIST2[, 3]), 
               cbind(period5, DIST1[, 3], DIST2[, 3]), cbind(period6, DIST1[, 3], DIST2[, 3]))
colnames(b.div) <- c("p", "s1", "s2", "n", "nsp1", "m", "nsp2", "nsh", "fm1", "fm2", "f1m", "f2m", "u_pt1", "v_pt1", "Junad", "Lunad", "U", "V", "Jabd", "Labd", "ipcc", "igeoe")
head(b.div)
b.div1 <- rbind(cbind(period1, DIST1[, 3], DIST2[, 3]), cbind(period2, DIST1[, 3], DIST2[, 3]), cbind(period3, DIST1[, 3], DIST2[, 3]))
colnames(b.div1) <- c("p", "s1", "s2", "n", "nsp1", "m", "nsp2", "nsh", "fm1", "fm2", "f1m", "f2m", "u_pt1", "v_pt1", "Junad", "Lunad", "U", "V", "Jabd", "Labd", "ipcc", "igeoe")
head(b.div1)
b.div2 <- rbind(cbind(period4, DIST1[, 3], DIST2[, 3]), cbind(period5, DIST1[, 3], DIST2[, 3]), cbind(period6, DIST1[, 3], DIST2[, 3]))
colnames(b.div2) <- c("p", "s1", "s2", "n", "nsp1", "m", "nsp2", "nsh", "fm1", "fm2", "f1m", "f2m", "u_pt1", "v_pt1", "Junad", "Lunad", "U", "V", "Jabd", "Labd", "ipcc", "igeoe")
head(b.div2)

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

#FOR ALL YEARS####     
wf.wf <- landsc_within(b.div, wf, wf); comment(wf.wf) <- "wf.wf"
wm.wm <- landsc_within(b.div, wm, wm); comment(wm.wm) <- "wm.wm"
wa.wa <- landsc_within(b.div, wa, wa); comment(wa.wa) <- "wa.wa"
tf.tf <- landsc_within(b.div, tf, tf); comment(tf.tf) <- "tf.tf"
tm.tm <- landsc_within(b.div, tm, tm); comment(tm.tm) <- "tm.tm"
ta.ta <- landsc_within(b.div, ta, ta); comment(ta.ta) <- "ta.ta"
sf.sf <- landsc_within(b.div, sf, sf); comment(sf.sf) <- "sf.sf"
sm.sm <- landsc_within(b.div, sm, sm); comment(sm.sm) <- "sm.sm"
sa.sa <- landsc_within(b.div, sa, sa); comment(sa.sa) <- "sa.sa"
mf.mf <- landsc_within(b.div, mf, mf); comment(mf.mf) <- "mf.mf"
mm.mm <- landsc_within(b.div, mm, mm); comment(mm.mm) <- "mm.mm"
ma.ma <- landsc_within(b.div, ma, ma); comment(ma.ma) <- "ma.ma"

##within landscapes - interval
figure.div(wf.wf)
figure.div(wm.wm)
figure.div(wa.wa)
figure.div(tf.tf)
figure.div(tm.tm)
figure.div(ta.ta)
figure.div(sf.sf)
figure.div(sm.sm)
figure.div(sa.sa)
figure.div(mf.mf)
figure.div(mm.mm)
figure.div(ma.ma)

#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 21], wf.wf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm[, 21], wm.wm[, 19], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa[, 21], wa.wa[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf[, 21], tf.tf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm[, 21], tm.tm[, 19], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta[, 21], ta.ta[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf[, 21], sf.sf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm[, 21], sm.sm[, 19], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa[, 21], sa.sa[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf[, 21], mf.mf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm[, 21], mm.mm[, 19], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma[, 21], ma.ma[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 21], wf.wf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm[, 21], wm.wm[, 20], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa[, 21], wa.wa[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf[, 21], tf.tf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm[, 21], tm.tm[, 20], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta[, 21], ta.ta[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf[, 21], sf.sf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm[, 21], sm.sm[, 20], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa[, 21], sa.sa[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf[, 21], mf.mf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm[, 21], mm.mm[, 20], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma[, 21], ma.ma[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 21], 1 - wf.wf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm[, 21], 1 - wm.wm[, 19], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa[, 21], 1 - wa.wa[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf[, 21], 1 - tf.tf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm[, 21], 1 - tm.tm[, 19], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta[, 21], 1 - ta.ta[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf[, 21], 1 - sf.sf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm[, 21], 1 - sm.sm[, 19], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa[, 21], 1 - sa.sa[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf[, 21], 1 - mf.mf[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm[, 21], 1 - mm.mm[, 19], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma[, 21], 1 - ma.ma[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 21], 1 - wf.wf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm[, 21], 1 - wm.wm[, 20], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa[, 21], 1 - wa.wa[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf[, 21], 1 - tf.tf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm[, 21], 1 - tm.tm[, 20], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta[, 21], 1 - ta.ta[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf[, 21], 1 - sf.sf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm[, 21], 1 - sm.sm[, 20], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa[, 21], 1 - sa.sa[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf[, 21], 1 - mf.mf[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm[, 21], 1 - mm.mm[, 20], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma[, 21], 1 - ma.ma[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#FOR FIRST YEAR####  
wf.wf1 <- landsc_within(b.div1, wf, wf); comment(wf.wf1) <- "wf.wf 1"
wm.wm1 <- landsc_within(b.div1, wm, wm); comment(wm.wm1) <- "wm.wm 1"
wa.wa1 <- landsc_within(b.div1, wa, wa); comment(wa.wa1) <- "wa.wa 1"
tf.tf1 <- landsc_within(b.div1, tf, tf); comment(tf.tf1) <- "tf.tf 1"
tm.tm1 <- landsc_within(b.div1, tm, tm); comment(tm.tm1) <- "tm.tm 1"
ta.ta1 <- landsc_within(b.div1, ta, ta); comment(ta.ta1) <- "ta.ta 1"
sf.sf1 <- landsc_within(b.div1, sf, sf); comment(sf.sf1) <- "sf.sf 1"
sm.sm1 <- landsc_within(b.div1, sm, sm); comment(sm.sm1) <- "sm.sm 1"
sa.sa1 <- landsc_within(b.div1, sa, sa); comment(sa.sa1) <- "sa.sa 1"
mf.mf1 <- landsc_within(b.div1, mf, mf); comment(mf.mf1) <- "mf.mf 1"
mm.mm1 <- landsc_within(b.div1, mm, mm); comment(mm.mm1) <- "mm.mm 1"
ma.ma1 <- landsc_within(b.div1, ma, ma); comment(ma.ma1) <- "ma.ma 1"

##within landscapes - interval
figure.div(wf.wf1)
figure.div(wm.wm1)
figure.div(wa.wa1)
figure.div(tf.tf1)
figure.div(tm.tm1)
figure.div(ta.ta1)
figure.div(sf.sf1)
figure.div(sm.sm1)
figure.div(sa.sa1)
figure.div(mf.mf1)
figure.div(mm.mm1)
figure.div(ma.ma1)

#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf1[, 21], wf.wf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm1[, 21], wm.wm1[, 19], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa1[, 21], wa.wa1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf1[, 21], tf.tf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm1[, 21], tm.tm1[, 19], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta1[, 21], ta.ta1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf1[, 21], sf.sf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm1[, 21], sm.sm1[, 19], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa1[, 21], sa.sa1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf1[, 21], mf.mf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm1[, 21], mm.mm1[, 19], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma1[, 21], ma.ma1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf1[, 21], wf.wf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm1[, 21], wm.wm1[, 20], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa1[, 21], wa.wa1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf1[, 21], tf.tf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm1[, 21], tm.tm1[, 20], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta1[, 21], ta.ta1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf1[, 21], sf.sf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm1[, 21], sm.sm1[, 20], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa1[, 21], sa.sa1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf1[, 21], mf.mf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm1[, 21], mm.mm1[, 20], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma1[, 21], ma.ma1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf1[, 21], 1 - wf.wf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm1[, 21], 1 - wm.wm1[, 19], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa1[, 21], 1 - wa.wa1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf1[, 21], 1 - tf.tf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm1[, 21], 1 - tm.tm1[, 19], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta1[, 21], 1 - ta.ta1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf1[, 21], 1 - sf.sf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm1[, 21], 1 - sm.sm1[, 19], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa1[, 21], 1 - sa.sa1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf1[, 21], 1 - mf.mf1[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm1[, 21], 1 - mm.mm1[, 19], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma1[, 21], 1 - ma.ma1[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf1[, 21], 1 - wf.wf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm1[, 21], 1 - wm.wm1[, 20], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa1[, 21], 1 - wa.wa1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf1[, 21], 1 - tf.tf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm1[, 21], 1 - tm.tm1[, 20], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta1[, 21], 1 - ta.ta1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf1[, 21], 1 - sf.sf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm1[, 21], 1 - sm.sm1[, 20], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa1[, 21], 1 - sa.sa1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf1[, 21], 1 - mf.mf1[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm1[, 21], 1 - mm.mm1[, 20], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma1[, 21], 1 - ma.ma1[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#FOR SECOND YEAR#### 
wf.wf2 <- landsc_within(b.div2, wf, wf); comment(wf.wf2) <- "wf.wf 2"
wm.wm2 <- landsc_within(b.div2, wm, wm); comment(wm.wm2) <- "wm.wm 2"
wa.wa2 <- landsc_within(b.div2, wa, wa); comment(wa.wa2) <- "wa.wa 2"
tf.tf2 <- landsc_within(b.div2, tf, tf); comment(tf.tf2) <- "tf.tf 2"
tm.tm2 <- landsc_within(b.div2, tm, tm); comment(tm.tm2) <- "tm.tm 2"
ta.ta2 <- landsc_within(b.div2, ta, ta); comment(ta.ta2) <- "ta.ta 2"
sf.sf2 <- landsc_within(b.div2, sf, sf); comment(sf.sf2) <- "sf.sf 2"
sm.sm2 <- landsc_within(b.div2, sm, sm); comment(sm.sm2) <- "sm.sm 2" 
sa.sa2 <- landsc_within(b.div2, sa, sa); comment(sa.sa2) <- "sa.sa 2"
mf.mf2 <- landsc_within(b.div2, mf, mf); comment(mf.mf2) <- "mf.mf 2"
mm.mm2 <- landsc_within(b.div2, mm, mm); comment(mm.mm2) <- "mm.mm 2"
ma.ma2 <- landsc_within(b.div2, ma, ma); comment(ma.ma2) <- "ma.ma 2"

##within landscapes - interval
figure.div(wf.wf2)
figure.div(wm.wm2)
figure.div(wa.wa2)
figure.div(tf.tf2)
figure.div(tm.tm2)
figure.div(ta.ta2)
figure.div(sf.sf2)
figure.div(sm.sm2)
figure.div(sa.sa2)
figure.div(mf.mf2)
figure.div(mm.mm2)
figure.div(ma.ma2)

#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf2[, 21], wf.wf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm2[, 21], wm.wm2[, 19], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa2[, 21], wa.wa2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf2[, 21], tf.tf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm2[, 21], tm.tm2[, 19], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta2[, 21], ta.ta2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf2[, 21], sf.sf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm2[, 21], sm.sm2[, 19], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa2[, 21], sa.sa2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf2[, 21], mf.mf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm2[, 21], mm.mm2[, 19], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma2[, 21], ma.ma2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf2[, 21], wf.wf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm2[, 21], wm.wm2[, 20], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa2[, 21], wa.wa2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf2[, 21], tf.tf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm2[, 21], tm.tm2[, 20], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta2[, 21], ta.ta2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf2[, 21], sf.sf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm2[, 21], sm.sm2[, 20], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa2[, 21], sa.sa2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf2[, 21], mf.mf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm2[, 21], mm.mm2[, 20], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma2[, 21], ma.ma2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf2[, 21], 1 - wf.wf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm2[, 21], 1 - wm.wm2[, 19], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa2[, 21], 1 - wa.wa2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf2[, 21], 1 - tf.tf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm2[, 21], 1 - tm.tm2[, 19], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta2[, 21], 1 - ta.ta2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf2[, 21], 1 - sf.sf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm2[, 21], 1 - sm.sm2[, 19], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa2[, 21], 1 - sa.sa2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf2[, 21], 1 - mf.mf2[, 19], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm2[, 21], 1 - mm.mm2[, 19], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma2[, 21], 1 - ma.ma2[, 19], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf2[, 21], 1 - wf.wf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wm2[, 21], 1 - wm.wm2[, 20], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wa.wa2[, 21], 1 - wa.wa2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tf2[, 21], 1 - tf.tf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.tm2[, 21], 1 - tm.tm2[, 20], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ta.ta2[, 21], 1 - ta.ta2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sf2[, 21], 1 - sf.sf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sm2[, 21], 1 - sm.sm2[, 20], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sa.sa2[, 21], 1 - sa.sa2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mf2[, 21], 1 - mf.mf2[, 20], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.mm2[, 21], 1 - mm.mm2[, 20], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(ma.ma2[, 21], 1 - ma.ma2[, 20], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))




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


#FOR ALL YEARS####
#woodland
wf.wm <- landsc_between(b.div, wf, wm); comment(wf.wm) <- "wf.wm"
wf.wa <- landsc_between(b.div, wf, wa); comment(wf.wa) <- "wf.wa"
wm.wa <- landsc_between(b.div, wm, wa); comment(wm.wa) <- "wm.wa"

#tall-shrub
tf.tm <- landsc_between(b.div, tf, tm); comment(tf.tm) <- "tf.tm"
tf.ta <- landsc_between(b.div, tf, ta); comment(tf.ta) <- "tf.ta"
tm.ta <- landsc_between(b.div, tm, ta); comment(tm.ta) <- "tm.ta"

#short-shrub
sf.sm <- landsc_between(b.div, sf, sm); comment(sf.sm) <- "sf.sm"
sf.sa <- landsc_between(b.div, sf, sa); comment(sf.sa) <- "sf.sa"
sm.sa <- landsc_between(b.div, sm, sa); comment(sm.sa) <- "sm.sa"

#meadow
mf.mm <- landsc_between(b.div, mf, mm); comment(mf.mm) <- "mf.mm"
mf.ma <- landsc_between(b.div, mf, ma); comment(mf.ma) <- "mf.ma"
mm.ma <- landsc_between(b.div, mm, ma); comment(mm.ma) <- "mm.ma"

figure.div(wf.wm)
figure.div(wf.wa)
figure.div(wm.wa)
figure.div(tf.tm)
figure.div(tf.ta)
figure.div(tm.ta)
figure.div(sf.sm)
figure.div(sf.sa)
figure.div(sm.sa)
figure.div(mf.mm)
figure.div(mf.ma)
figure.div(mm.ma)

#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 21], wf.wm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa[, 21], wf.wa[, 19], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa[, 21], wm.wa[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm[, 21], tf.tm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta[, 21], tf.ta[, 19], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta[, 21], tm.ta[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm[, 21], sf.sm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa[, 21], sf.sa[, 19], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa[, 21], sm.sa[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm[, 21], mf.mm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma[, 21], mf.ma[, 19], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma[, 21], mm.ma[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 21], wf.wm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa[, 21], wf.wa[, 20], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa[, 21], wm.wa[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm[, 21], tf.tm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta[, 21], tf.ta[, 20], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta[, 21], tm.ta[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm[, 21], sf.sm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa[, 21], sf.sa[, 20], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa[, 21], sm.sa[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm[, 21], mf.mm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma[, 21], mf.ma[, 20], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma[, 21], mm.ma[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 21], 1 - wf.wm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa[, 21], 1 - wf.wa[, 19], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa[, 21], 1 - wm.wa[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm[, 21], 1 - tf.tm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta[, 21], 1 - tf.ta[, 19], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta[, 21], 1 - tm.ta[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm[, 21], 1 - sf.sm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa[, 21], 1 - sf.sa[, 19], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa[, 21], 1 - sm.sa[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm[, 21], 1 - mf.mm[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma[, 21], 1 - mf.ma[, 19], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma[, 21], 1 - mm.ma[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 21], 1 - wf.wm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa[, 21], 1 - wf.wa[, 20], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa[, 21], 1 - wm.wa[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm[, 21], 1 - tf.tm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta[, 21], 1 - tf.ta[, 20], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta[, 21], 1 - tm.ta[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm[, 21], 1 - sf.sm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa[, 21], 1 - sf.sa[, 20], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa[, 21], 1 - sm.sa[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm[, 21], 1 - mf.mm[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma[, 21], 1 - mf.ma[, 20], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma[, 21], 1 - mm.ma[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#FOR FIRST YEAR####
#woodland
wf.wm1 <- landsc_between(b.div1, wf, wm); comment(wf.wm1) <- "wf.wm 1"
wf.wa1 <- landsc_between(b.div1, wf, wa); comment(wf.wa1) <- "wf.wa 1"
wm.wa1 <- landsc_between(b.div1, wm, wa); comment(wm.wa1) <- "wm.wa 1"

#tall-shrub
tf.tm1 <- landsc_between(b.div1, tf, tm); comment(tf.tm1) <- "tf.tm 1"
tf.ta1 <- landsc_between(b.div1, tf, ta); comment(tf.ta1) <- "tf.ta 1"
tm.ta1 <- landsc_between(b.div1, tm, ta); comment(tm.ta1) <- "tm.ta 1"

#short-shrub
sf.sm1 <- landsc_between(b.div1, sf, sm); comment(sf.sm1) <- "sf.sm 1"
sf.sa1 <- landsc_between(b.div1, sf, sa); comment(sf.sa1) <- "sf.sa 1"
sm.sa1 <- landsc_between(b.div1, sm, sa); comment(sm.sa1) <- "sm.sa 1"

#meadow
mf.mm1 <- landsc_between(b.div1, mf, mm); comment(mf.mm1) <- "mf.mm 1"
mf.ma1 <- landsc_between(b.div1, mf, ma); comment(mf.ma1) <- "mf.ma 1"
mm.ma1 <- landsc_between(b.div1, mm, ma); comment(mm.ma1) <- "mm.ma 1"

figure.div(wf.wm1)
figure.div(wf.wa1)
figure.div(wm.wa1)
figure.div(tf.tm1)
figure.div(tf.ta1)
figure.div(tm.ta1)
figure.div(sf.sm1)
figure.div(sf.sa1)
figure.div(sm.sa1)
figure.div(mf.mm1)
figure.div(mf.ma1)
figure.div(mm.ma1)

#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm1[, 21], wf.wm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa1[, 21], wf.wa1[, 19], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa1[, 21], wm.wa1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm1[, 21], tf.tm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta1[, 21], tf.ta1[, 19], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta1[, 21], tm.ta1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm1[, 21], sf.sm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa1[, 21], sf.sa1[, 19], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa1[, 21], sm.sa1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm1[, 21], mf.mm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma1[, 21], mf.ma1[, 19], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma1[, 21], mm.ma1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm1[, 21], wf.wm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa1[, 21], wf.wa1[, 20], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa1[, 21], wm.wa1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm1[, 21], tf.tm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta1[, 21], tf.ta1[, 20], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta1[, 21], tm.ta1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm1[, 21], sf.sm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa1[, 21], sf.sa1[, 20], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa1[, 21], sm.sa1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm1[, 21], mf.mm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma1[, 21], mf.ma1[, 20], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma1[, 21], mm.ma1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#####DISSIMILARITY#####
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm1[, 21], 1 - wf.wm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa1[, 21], 1 - wf.wa1[, 19], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa1[, 21], 1 - wm.wa1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm1[, 21], 1 - tf.tm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta1[, 21], 1 - tf.ta1[, 19], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta1[, 21], 1 - tm.ta1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm1[, 21], 1 - sf.sm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa1[, 21], 1 - sf.sa1[, 19], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa1[, 21], 1 - sm.sa1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm1[, 21], 1 - mf.mm1[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma1[, 21], 1 - mf.ma1[, 19], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma1[, 21], 1 - mm.ma1[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm1[, 21], 1 - wf.wm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa1[, 21], 1 - wf.wa1[, 20], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa1[, 21], 1 - wm.wa1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm1[, 21], 1 - tf.tm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta1[, 21], 1 - tf.ta1[, 20], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta1[, 21], 1 - tm.ta1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm1[, 21], 1 - sf.sm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa1[, 21], 1 - sf.sa1[, 20], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa1[, 21], 1 - sm.sa1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm1[, 21], 1 - mf.mm1[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma1[, 21], 1 - mf.ma1[, 20], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma1[, 21], 1 - mm.ma1[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#SECOND YEAR####
#woodland
wf.wm2 <- landsc_between(b.div2, wf, wm); comment(wf.wm2) <- "wf.wm 2"
wf.wa2 <- landsc_between(b.div2, wf, wa); comment(wf.wa2) <- "wf.wa 2"
wm.wa2 <- landsc_between(b.div2, wm, wa); comment(wm.wa2) <- "wm.wa 2"

#tall-shrub
tf.tm2 <- landsc_between(b.div2, tf, tm); comment(tf.tm2) <- "tf.tm 2"
tf.ta2 <- landsc_between(b.div2, tf, ta); comment(tf.ta2) <- "tf.ta 2"
tm.ta2 <- landsc_between(b.div2, tm, ta); comment(tm.ta2) <- "tm.ta 2"

#short-shrub
sf.sm2 <- landsc_between(b.div2, sf, sm); comment(sf.sm2) <- "sf.sm 2"
sf.sa2 <- landsc_between(b.div2, sf, sa); comment(sf.sa2) <- "sf.sa 2"
sm.sa2 <- landsc_between(b.div2, sm, sa); comment(sm.sa2) <- "sm.sa 2"

#meadow
mf.mm2 <- landsc_between(b.div2, mf, mm); comment(mf.mm2) <- "mf.mm 2"
mf.ma2 <- landsc_between(b.div2, mf, ma); comment(mf.ma2) <- "mf.ma 2"
mm.ma2 <- landsc_between(b.div2, mm, ma); comment(mm.ma2) <- "mm.ma 2"

figure.div(wf.wm2)
figure.div(wf.wa2)
figure.div(wm.wa2)
figure.div(tf.tm2)
figure.div(tf.ta2)
figure.div(tm.ta2)
figure.div(sf.sm2)
figure.div(sf.sa2)
figure.div(sm.sa2)
figure.div(mf.mm2)
figure.div(mf.ma2)
figure.div(mm.ma2)

#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm2[, 21], wf.wm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa2[, 21], wf.wa2[, 19], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa2[, 21], wm.wa2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm2[, 21], tf.tm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta2[, 21], tf.ta2[, 19], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta2[, 21], tm.ta2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm2[, 21], sf.sm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa2[, 21], sf.sa2[, 19], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa2[, 21], sm.sa2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm2[, 21], mf.mm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma2[, 21], mf.ma2[, 19], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma2[, 21], mm.ma2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm2[, 21], wf.wm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa2[, 21], wf.wa2[, 20], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa2[, 21], wm.wa2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm2[, 21], tf.tm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta2[, 21], tf.ta2[, 20], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta2[, 21], tm.ta2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm2[, 21], sf.sm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa2[, 21], sf.sa2[, 20], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa2[, 21], sm.sa2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm2[, 21], mf.mm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma2[, 21], mf.ma2[, 20], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma2[, 21], mm.ma2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm2[, 21], 1 - wf.wm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa2[, 21], 1 - wf.wa2[, 19], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa2[, 21], 1 - wm.wa2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm2[, 21], 1 - tf.tm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta2[, 21], 1 - tf.ta2[, 19], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta2[, 21], 1 - tm.ta2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm2[, 21], 1 - sf.sm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa2[, 21], 1 - sf.sa2[, 19], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa2[, 21], 1 - sm.sa2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm2[, 21], 1 - mf.mm2[, 19], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma2[, 21], 1 - mf.ma2[, 19], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma2[, 21], 1 - mm.ma2[, 19], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm2[, 21], 1 - wf.wm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wf.wa2[, 21], 1 - wf.wa2[, 20], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(wm.wa2[, 21], 1 - wm.wa2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.tm2[, 21], 1 - tf.tm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tf.ta2[, 21], 1 - tf.ta2[, 20], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(tm.ta2[, 21], 1 - tm.ta2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sm2[, 21], 1 - sf.sm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sf.sa2[, 21], 1 - sf.sa2[, 20], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(sm.sa2[, 21], 1 - sm.sa2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.mm2[, 21], 1 - mf.mm2[, 20], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mf.ma2[, 21], 1 - mf.ma2[, 20], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))
plot(mm.ma2[, 21], 1 - mm.ma2[, 20], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 7400))

#between biotopes####



#REGRESSION####
model1 <- lm((1 - b.div1[, 19]) ~ b.div1[, 21])
summary(model1)
model2 <- lm((1 - b.div2[, 19]) ~ b.div2[, 21])
summary(model2)
par(mfrow=c(2,1))

plot(b.div1[, 21], 1 - b.div1[, 19])
abline(coef(model1[1]), coef(model1[2]), col = 2)
plot(b.div2[, 21], 1 - b.div2[, 19])
abline(coef(model2[1]), coef(model2[2]), col = 2)

#EACH LANDSCAPE
##ALL YEARS
par(mfrow = c(3, 1), oma = c(0, 0, 3, 0))
forest <- c(wf, tf, sf, mf) 
datafor <- landsc_within(b.div, forest, forest); comment(datafor) <- "DATAFOR"
modfor <- lm((1 - datafor[, 19]) ~ datafor[, 21])
summary(modfor)
plot((1 - datafor[, 19]) ~ datafor[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modfor)[1], coef(modfor)[2], col = 2)
mtext("ALL", outer = TRUE, cex = 1.5)

mixed <- c(wm, tm, sm, mm)
datamix <- landsc_within(b.div, mixed, mixed); comment(datamix) <- "DATAMIX"
modmix <- lm((1 - datamix[, 19]) ~ datamix[, 21])
summary(modmix)
plot((1 - datamix[, 19]) ~ datamix[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modmix)[1], coef(modmix)[2], col = 2)

meadow <- c(wa, ta, sa, ma)
datamea <- landsc_within(b.div, meadow, meadow); comment(datamea) <- "DATAMEA"
modmea <- lm((1 - datamea[, 19]) ~ datamea[, 21])
summary(modmea)
plot((1 - datamea[, 19]) ~ datamea[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modmea)[1], coef(modmea)[2], col = 2)

intfor <- figure.div(datafor)
intmix <- figure.div(datamix)
intmea <- figure.div(datamea)

##FIRST YEAR
par(mfrow = c(3, 1), oma = c(0, 0, 3, 0))
forest <- c(wf, tf, sf, mf) 
datafor1 <- landsc_within(b.div1, forest, forest); comment(datafor1) <- "DATAFOR 1"
modfor1 <- lm((1 - datafor1[, 19]) ~ datafor1[, 21])
summary(modfor1)
plot((1 - datafor1[, 19]) ~ datafor1[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modfor1)[1], coef(modfor1)[2], col = 2)
mtext("FIRST YEAR", outer = TRUE, cex = 1.5)

mixed <- c(wm, tm, sm, mm)
datamix1 <- landsc_within(b.div1, mixed, mixed); comment(datamix1) <- "DATAMIX 1"
modmix1 <- lm((1 - datamix1[, 19]) ~ datamix1[, 21])
summary(modmix1)
plot((1 - datamix1[, 19]) ~ datamix1[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modmix1)[1], coef(modmix1)[2], col = 2)

meadow <- c(wa, ta, sa, ma)
datamea1 <- landsc_within(b.div1, meadow, meadow); comment(datamea1) <- "DATAMEA 1"
modmea1 <- lm((1 - datamea1[, 19]) ~ datamea1[, 21])
summary(modmea1)
plot((1 - datamea1[, 19]) ~ datamea1[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modmea1)[1], coef(modmea1)[2], col = 2)

intfor1 <- figure.div(datafor1)
intmix1 <- figure.div(datamix1)
intmea1 <- figure.div(datamea1)

##SECOND YEAR
par(mfrow = c(3, 1), oma = c(0, 0, 3, 0))
forest <- c(wf, tf, sf, mf) 
datafor2 <- landsc_within(b.div2, forest, forest); comment(datafor2) <- "DATAFOR 2"
modfor2 <- lm((1 - datafor2[, 19]) ~ datafor2[, 21])
summary(modfor2)
plot((1 - datafor2[, 19]) ~ datafor2[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modfor2)[1], coef(modfor2)[2], col = 2)
mtext("ALL", outer = TRUE, cex = 1.5)

mixed <- c(wm, tm, sm, mm)
datamix2 <- landsc_within(b.div2, mixed, mixed); comment(datamix2) <- "DATAMIX 2"
modmix2 <- lm((1 - datamix2[, 19]) ~ datamix2[, 21])
summary(modmix2)
plot((1 - datamix2[, 19]) ~ datamix2[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modmix2)[1], coef(modmix2)[2], col = 2)

meadow <- c(wa, ta, sa, ma)
datamea2 <- landsc_within(b.div2, meadow, meadow); comment(datamea2) <- "DATAMEA 2"
modmea2 <- lm((1 - datamea2[, 19]) ~ datamea2[, 21])
summary(modmea)
plot((1 - datamea2[, 19]) ~ datamea2[, 21], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1), xlim=c(0, 2000))
abline(coef(modmea2)[1], coef(modmea2)[2], col = 2)

intfor2 <- figure.div(datafor2)
intmix2 <- figure.div(datamix2)
intmea2 <- figure.div(datamea2)

period <- rep(c("All","Year1","Year2"), each = 4)

intf <- rbind(intfor[1:4, ], intfor1[1:4, ], intfor2[1:4, ])
intf <- cbind(period, intf)

intm <- rbind(intmix[1:4, ], intmix1[1:4, ], intmix2[1:4, ])
intm <- cbind(period, intm)

inta <- rbind(intmea[1:4, ], intmea1[1:4, ], intmea2[1:4, ])
inta <- cbind(period, inta)

par(mfrow=c(3, 2), oma = c(0, 0, 3, 0))
plot(1 - intf[, 6]/intf[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
title("Forest, Mixed and Agriculture", outer = TRUE, cex = 1.5)
segments(x0 = 1:12, y0 = 1 - (intf[, 6]/intf[, 1]) - sqrt((intf[, 7]/(intf[, 1]) - (intf[, 6]/intf[, 1])^2)), y1 = 1 - (intf[, 6]/intf[, 1]) + sqrt((intf[, 7]/(intf[, 1]) - (intf[, 6]/intf[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
plot(1 - intf[, 8]/intf[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:12, y0 = 1 - (intf[, 8]/intf[, 1]) - sqrt((intf[, 9]/(intf[, 1]) - (intf[, 8]/intf[, 1])^2)), y1 = 1 - (intf[, 8]/intf[, 1]) + sqrt((intf[, 9]/(intf[, 1]) - (intf[, 8]/intf[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)

plot(1 - intm[, 6]/intm[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:12, y0 = 1 - (intm[, 6]/intm[, 1]) - sqrt((intm[, 7]/(intm[, 1]) - (intm[, 6]/intm[, 1])^2)), y1 = 1 - (intm[, 6]/intm[, 1]) + sqrt((intm[, 7]/(intm[, 1]) - (intm[, 6]/intm[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
plot(1 - intm[, 8]/intf[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:12, y0 = 1 - (intm[, 8]/intm[, 1]) - sqrt((intm[, 9]/(intm[, 1]) - (intm[, 8]/intm[, 1])^2)), y1 = 1 - (intm[, 8]/intm[, 1]) + sqrt((intm[, 9]/(intm[, 1]) - (intm[, 8]/intm[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)

plot(1 - inta[, 6]/inta[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
segments(x0 = 1:12, y0 = 1 - (inta[, 6]/inta[, 1]) - sqrt((inta[, 7]/(inta[, 1]) - (inta[, 6]/inta[, 1])^2)), y1 = 1 - (inta[, 6]/inta[, 1]) + sqrt((inta[, 7]/(inta[, 1]) - (inta[, 6]/inta[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)
plot(1 - inta[, 8]/inta[, 1], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
segments(x0 = 1:12, y0 = 1 - (inta[, 8]/inta[, 1]) - sqrt((inta[, 9]/(inta[, 1]) - (inta[, 8]/inta[, 1])^2)), y1 = 1 - (inta[, 8]/inta[, 1]) + sqrt((inta[, 9]/(inta[, 1]) - (inta[, 8]/inta[, 1])^2)), col = "black")
abline(h = 0.5, col = 2)

#BOOTSTRAPPING REGRESSION MODELS#####
boot.huber <- function(data, indices, maxit = 20){
  data <- data[indices, ] #select obs. in bootstrap sample
  mod <- rlm(Jabd ~ ipcc, data = data, maxit = maxit)
  coefficients(mod) #return coefficient vector
}

library(MASS)
library(boot)
head(as.data.frame(b.div))
b.div.boot <- boot(as.data.frame(b.div), boot.huber, 2000, maxit = 100)
b.div.boot

plot(b.div.boot, index = 1)
plot(b.div.boot, index = 2)

#BOOTSTRAP CONFIDENCE INTERVALS####
boot.ci(b.div.boot, index = 1, type = c("norm", "perc", "bca"))
boot.ci(b.div.boot, index = 2, type = c("norm", "perc", "bca"))

#SENSITIVITY OF THE STATISTIC
par(mfcol = c(2, 1))
jack.after.boot(b.div.boot, index = 1, main = "(a) Intercept")
jack.after.boot(b.div.boot, index = 2, main = "(b) Geographic Distance (m)")

###### TESTES ##########
species <- union(species_s1[, 1], species_s2[, 1])
species <- cbind(species, matrix("0", nrow = length(species)), matrix("0", nrow = length(species)))
colnames(species) <- c("spec", "abun1", "abun2")

for(sp in 1:dim(species)[1]){
  value <- species_s1[species_s1[, 1] == species[sp], ][2]
  if(!is.na(value)) species[sp, 2] <- value
  value <- species_s2[species_s2[, 1] == species[sp], ][2]
  if(!is.na(value)) species[sp, 3] <- value
}

species_s1 <- cbind(as.vector(data$species[data$Site == 3 & data$Period == 1]), data$Nr[data$Site == 3 & data$Period == 1])
species_s2 <- cbind(as.vector(data$species[data$Site == 17 & data$Period == 1]), data$Nr[data$Site == 17 & data$Period == 1])
species_s1
species_s2

sum(as.numeric(species_s2[, 2]))

shared <- intersect(species_s1[, 1], species_s2[, 1])
shared
shared <- cbind(shared, matrix("0", nrow = length(shared)), matrix("0", nrow = length(shared)))
colnames(shared) <- c("spec", "abun1", "abun2")
shared
sum(as.numeric(shared[, 3]))
for(sp in 1:dim(shared)[1]){
  shared[sp, 2] <- species_s1[species_s1[, 1] == shared[sp], ][2] 
  shared[sp, 3] <- species_s2[species_s2[, 1] == shared[sp], ][2] 
}

shared
species_s1[species_s1[, 1] == shared[9], ][2] 
species_s2[species_s2[, 1] == shared[9], ][2] 


jper1boot <- read.csv("period1J.csv", h = T, sep = " ")
test <- as.vector(jper1boot[1, 1:1000])

hist(as.vector(as.numeric(jper1boot[jper1boot != "NA"])))
hist(as.vector(as.numeric(jper1boot[1, ])), xlim=c(0, 1))
hist(as.vector(as.numeric(jper1boot[2, ])), xlim=c(0, 1))
hist(as.vector(as.numeric(jper1boot[3, ])), xlim=c(0, 1))

sum_per1 <- apply(jper1boot, 1, mean)
hist(sum_per1)
