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
source('chaomb.R')

period1m <- chaom(1, 1, B = 1000, "period1")
period2m <- chaom(2, 2, B = 1000, "period2")
period3m <- chaom(3, 3, B = 1000, "period3")
period4m <- chaom(4, 4, B = 1000, "period4")
period5m <- chaom(5, 5, B = 1000, "period5")
period6m <- chaom(6, 6, B = 1000, "period6")

period1 <- chao(1, 1, "tperiod1")
period2 <- chao(2, 2, "tperiod2")
period3 <- chao(3, 3, "tperiod3")
period4 <- chao(4, 4, "tperiod4")
period5 <- chao(5, 5, "tperiod5")
period6 <- chao(6, 6, "tperiod6")

## For first year
period123m <- matrix(0, nrow = 3486)
for(i in 1:3486){
  period123m[i] <- mean(period1m[i, 4], period2m[i, 4], period3m[i, 4])
}

## For second year
period456m <- matrix(0, nrow = 3486)
for(i in 1:3486){
  period456m[i] <- mean(period4m[i, 4], period5m[i, 4], period6m[i, 4])
}

## For all traps
periodm <- matrix(0, nrow = 3486)
for(i in 1:3486){
  periodm[i] <- mean(period123m[i], period456m[i])
}

#SIMILARITY####
par(mfrow=c(3, 3))
plot(DIST1[, 3], period1m[, 4], main = "Period 1", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period2m[, 4], main = "Period 2", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period3m[, 4], main = "Period 3", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period4m[, 4], main = "Period 4", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period5m[, 4], main = "Period 5", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period6m[, 4], main = "Period 6", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period123m, main = "First Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period456m, main = "Second Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], periodm, main = "ALL", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

plot(DIST1[, 3], period1[, 4], main = "Period 1", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period2[, 4], main = "Period 2", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period3[, 4], main = "Period 3", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period4[, 4], main = "Period 4", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period5[, 4], main = "Period 5", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], period6[, 4], main = "Period 6", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#DISSIMILARITY####
par(mfrow=c(3, 3))
plot(DIST1[, 3], 1 - period1m[, 4], main = "Period 1", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period2m[, 4], main = "Period 2", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period3m[, 4], main = "Period 3", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period4m[, 4], main = "Period 4", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period5m[, 4], main = "Period 5", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period6m[, 4], main = "Period 6", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period123m, main = "First Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - period456m, main = "Second Year", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(DIST1[, 3], 1 - periodm, main = "ALL", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#BIG-DATASET####
b.div <- rbind(cbind(period1m, DIST1[, 3], DIST2[, 3]), cbind(period2m, DIST1[, 3], DIST2[, 3]), 
               cbind(period3m, DIST1[, 3], DIST2[, 3]), cbind(period4m, DIST1[, 3], DIST2[, 3]), 
               cbind(period5m, DIST1[, 3], DIST2[, 3]), cbind(period6m, DIST1[, 3], DIST2[, 3]))
colnames(b.div) <- c("p", "s1", "s2", "Jabd", "Labd", "MeanJ", "VarJ", "MeanL", "VarL", "ipcc", "igeoe")
head(b.div)

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

wf.wf <- landsc_within(b.div, wf, wf)
wm.wm <- landsc_within(b.div, wm, wm)
wa.wa <- landsc_within(b.div, wa, wa)
tf.tf <- landsc_within(b.div, tf, tf)
tm.tm <- landsc_within(b.div, tm, tm)
ta.ta <- landsc_within(b.div, ta, ta)
sf.sf <- landsc_within(b.div, sf, sf)
sm.sm <- landsc_within(b.div, sm, sm)
sa.sa <- landsc_within(b.div, sa, sa)
mf.mf <- landsc_within(b.div, mf, mf)
mm.mm <- landsc_within(b.div, mm, mm)
ma.ma <- landsc_within(b.div, ma, ma)

#FOR ALL YEARS####                       
#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 10], wf.wf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[, 10], wm.wm[, 4], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[, 10], wa.wa[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[, 10], tf.tf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[, 10], tm.tm[, 4], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[, 10], ta.ta[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[, 10], sf.sf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[, 10], sm.sm[, 4], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[, 10], sa.sa[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[, 10], mf.mf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[, 10], mm.mm[, 4], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[, 10], ma.ma[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 8], wf.wf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[, 8], wm.wm[, 5], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[, 8], wa.wa[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[, 8], tf.tf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[, 8], tm.tm[, 5], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[, 8], ta.ta[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[, 8], sf.sf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[, 8], sm.sm[, 5], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[, 8], sa.sa[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[, 8], mf.mf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[, 8], mm.mm[, 5], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[, 8], ma.ma[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 8], 1 - wf.wf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[, 8], 1 - wm.wm[, 4], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[, 8], 1 - wa.wa[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[, 8], 1 - tf.tf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[, 8], 1 - tm.tm[, 4], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[, 8], 1 - ta.ta[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[, 8], 1 - sf.sf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[, 8], 1 - sm.sm[, 4], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[, 8], 1 - sa.sa[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[, 8], 1 - mf.mf[, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[, 8], 1 - mm.mm[, 4], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[, 8], 1 - ma.ma[, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[, 8], 1 - wf.wf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[, 8], 1 - wm.wm[, 5], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[, 8], 1 - wa.wa[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[, 8], 1 - tf.tf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[, 8], 1 - tm.tm[, 5], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[, 8], 1 - ta.ta[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[, 8], 1 - sf.sf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[, 8], 1 - sm.sm[, 5], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[, 8], 1 - sa.sa[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[, 8], 1 - mf.mf[, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[, 8], 1 - mm.mm[, 5], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[, 8], 1 - ma.ma[, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#FOR FIRST YEAR####                       
#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 1:3, 8], wf.wf[wf.wf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 1:3, 8], wm.wm[wm.wm[, 1] == 1:3, 4], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 1:3, 8], wa.wa[wa.wa[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 1:3, 8], tf.tf[tf.tf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 1:3, 8], tm.tm[tm.tm[, 1] == 1:3, 4], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 1:3, 8], ta.ta[ta.ta[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 1:3, 8], sf.sf[sf.sf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 1:3, 8], sm.sm[sm.sm[, 1] == 1:3, 4], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 1:3, 8], sa.sa[sa.sa[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 1:3, 8], mf.mf[mf.mf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 1:3, 8], mm.mm[mm.mm[, 1] == 1:3, 4], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 1:3, 8], ma.ma[ma.ma[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 1:3, 8], wf.wf[wf.wf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 1:3, 8], wm.wm[wm.wm[, 1] == 1:3, 5], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 1:3, 8], wa.wa[wa.wa[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 1:3, 8], tf.tf[tf.tf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 1:3, 8], tm.tm[tm.tm[, 1] == 1:3, 5], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 1:3, 8], ta.ta[ta.ta[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 1:3, 8], sf.sf[sf.sf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 1:3, 8], sm.sm[sm.sm[, 1] == 1:3, 5], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 1:3, 8], sa.sa[sa.sa[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 1:3, 8], mf.mf[mf.mf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 1:3, 8], mm.mm[mm.mm[, 1] == 1:3, 5], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 1:3, 8], ma.ma[ma.ma[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 1:3, 8], 1 - wf.wf[wf.wf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 1:3, 8], 1 - wm.wm[wm.wm[, 1] == 1:3, 4], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 1:3, 8], 1 - wa.wa[wa.wa[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 1:3, 8], 1 - tf.tf[tf.tf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 1:3, 8], 1 - tm.tm[tm.tm[, 1] == 1:3, 4], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 1:3, 8], 1 - ta.ta[ta.ta[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 1:3, 8], 1 - sf.sf[sf.sf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 1:3, 8], 1 - sm.sm[sm.sm[, 1] == 1:3, 4], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 1:3, 8], 1 - sa.sa[sa.sa[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 1:3, 8], 1 - mf.mf[mf.mf[, 1] == 1:3, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 1:3, 8], 1 - mm.mm[mm.mm[, 1] == 1:3, 4], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 1:3, 8], 1 - ma.ma[ma.ma[, 1] == 1:3, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 1:3, 8], 1 - wf.wf[wf.wf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 1:3, 8], 1 - wm.wm[wm.wm[, 1] == 1:3, 5], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 1:3, 8], 1 - wa.wa[wa.wa[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 1:3, 8], 1 - tf.tf[tf.tf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 1:3, 8], 1 - tm.tm[tm.tm[, 1] == 1:3, 5], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 1:3, 8], 1 - ta.ta[ta.ta[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 1:3, 8], 1 - sf.sf[sf.sf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 1:3, 8], 1 - sm.sm[sm.sm[, 1] == 1:3, 5], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 1:3, 8], 1 - sa.sa[sa.sa[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 1:3, 8], 1 - mf.mf[mf.mf[, 1] == 1:3, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 1:3, 8], 1 - mm.mm[mm.mm[, 1] == 1:3, 5], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 1:3, 8], 1 - ma.ma[ma.ma[, 1] == 1:3, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#FOR SECOND YEAR####                       
#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 4:6, 8], wf.wf[wf.wf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 4:6, 8], wm.wm[wm.wm[, 1] == 4:6, 4], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 4:6, 8], wa.wa[wa.wa[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 4:6, 8], tf.tf[tf.tf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 4:6, 8], tm.tm[tm.tm[, 1] == 4:6, 4], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 4:6, 8], ta.ta[ta.ta[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 4:6, 8], sf.sf[sf.sf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 4:6, 8], sm.sm[sm.sm[, 1] == 4:6, 4], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 4:6, 8], sa.sa[sa.sa[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 4:6, 8], mf.mf[mf.mf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 4:6, 8], mm.mm[mm.mm[, 1] == 4:6, 4], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 4:6, 8], ma.ma[ma.ma[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 4:6, 8], wf.wf[wf.wf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 4:6, 8], wm.wm[wm.wm[, 1] == 4:6, 5], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 4:6, 8], wa.wa[wa.wa[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 4:6, 8], tf.tf[tf.tf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 4:6, 8], tm.tm[tm.tm[, 1] == 4:6, 5], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 4:6, 8], ta.ta[ta.ta[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 4:6, 8], sf.sf[sf.sf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 4:6, 8], sm.sm[sm.sm[, 1] == 4:6, 5], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 4:6, 8], sa.sa[sa.sa[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 4:6, 8], mf.mf[mf.mf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 4:6, 8], mm.mm[mm.mm[, 1] == 4:6, 5], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 4:6, 8], ma.ma[ma.ma[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#DISSIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 4:6, 8], 1 - wf.wf[wf.wf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 4:6, 8], 1 - wm.wm[wm.wm[, 1] == 4:6, 4], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 4:6, 8], 1 - wa.wa[wa.wa[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 4:6, 8], 1 - tf.tf[tf.tf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 4:6, 8], 1 - tm.tm[tm.tm[, 1] == 4:6, 4], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 4:6, 8], 1 - ta.ta[ta.ta[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 4:6, 8], 1 - sf.sf[sf.sf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 4:6, 8], 1 - sm.sm[sm.sm[, 1] == 4:6, 4], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 4:6, 8], 1 - sa.sa[sa.sa[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 4:6, 8], 1 - mf.mf[mf.mf[, 1] == 4:6, 4], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 4:6, 8], 1 - mm.mm[mm.mm[, 1] == 4:6, 4], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 4:6, 8], 1 - ma.ma[ma.ma[, 1] == 4:6, 4], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wf[wf.wf[, 1] == 4:6, 8], 1 - wf.wf[wf.wf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wm[wm.wm[, 1] == 4:6, 8], 1 - wm.wm[wm.wm[, 1] == 4:6, 5], main = "Woodland \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wa.wa[wa.wa[, 1] == 4:6, 8], 1 - wa.wa[wa.wa[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tf[tf.tf[, 1] == 4:6, 8], 1 - tf.tf[tf.tf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.tm[tm.tm[, 1] == 4:6, 8], 1 - tm.tm[tm.tm[, 1] == 4:6, 5], main = "Tall-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ta.ta[ta.ta[, 1] == 4:6, 8], 1 - ta.ta[ta.ta[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sf[sf.sf[, 1] == 4:6, 8], 1 - sf.sf[sf.sf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sm[sm.sm[, 1] == 4:6, 8], 1 - sm.sm[sm.sm[, 1] == 4:6, 5], main = "Short-shurb \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sa.sa[sa.sa[, 1] == 4:6, 8], 1 - sa.sa[sa.sa[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mf[mf.mf[, 1] == 4:6, 8], 1 - mf.mf[mf.mf[, 1] == 4:6, 5], main = "Forest", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.mm[mm.mm[, 1] == 4:6, 8], 1 - mm.mm[mm.mm[, 1] == 4:6, 5], main = "Meadow \n Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(ma.ma[ma.ma[, 1] == 4:6, 8], 1 - ma.ma[ma.ma[, 1] == 4:6, 5], main = "Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

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

#woodland
wf.wm <- landsc_between(b.div, wf, wm)
wf.wa <- landsc_between(b.div, wf, wa)
wm.wa <- landsc_between(b.div, wm, wa)

#tall-shrub
tf.tm <- landsc_between(b.div, tf, tm)
tf.ta <- landsc_between(b.div, tf, ta)
tm.ta <- landsc_between(b.div, tm, ta)

#short-shrub
sf.sm <- landsc_between(b.div, sf, sm)
sf.sa <- landsc_between(b.div, sf, sa)
sm.sa <- landsc_between(b.div, sm, sa)

#meadow
mf.mm <- landsc_between(b.div, mf, mm)
mf.ma <- landsc_between(b.div, mf, ma)
mm.ma <- landsc_between(b.div, mm, ma)

#FOR ALL YEARS####
#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], wf.wm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], wf.wa[, 4], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], wm.wa[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], tf.tm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], tf.ta[, 4], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], tm.ta[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], sf.sm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], sf.sa[, 4], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], sm.sa[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], mf.mm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], mf.ma[, 4], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], mm.ma[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], wf.wm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], wf.wa[, 5], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], wm.wa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], tf.tm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], tf.ta[, 5], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], tm.ta[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], sf.sm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], sf.sa[, 5], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], sm.sa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], mf.mm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], mf.ma[, 5], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], mm.ma[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#FOR FIRST YEAR####
#SIMILARITY###
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm[wf.wm[, 1] == 1:3, 8], wf.wm[wf.wm[, 1] == 1:3, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[wf.wa[, 1] == 1:3, 8], wf.wa[wf.wa[, 1] == 1:3, 4], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[wm.wa[, 1] == 1:3, 8], wm.wa[wm.wa[, 1] == 1:3, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[tf.tm[, 1] == 1:3, 8], tf.tm[tf.tm[, 1] == 1:3, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[tf.ta[, 1] == 1:3, 8], tf.ta[tf.ta[, 1] == 1:3, 4], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[tm.ta[, 1] == 1:3, 8], tm.ta[tm.ta[, 1] == 1:3, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[sf.sm[, 1] == 1:3, 8], sf.sm[sf.sm[, 1] == 1:3, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[sf.sa[, 1] == 1:3, 8], sf.sa[sf.sa[, 1] == 1:3, 4], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[sm.sa[, 1] == 1:3, 8], sm.sa[sm.sa[, 1] == 1:3, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[mf.mm[, 1] == 1:3, 8], mf.mm[mf.mm[, 1] == 1:3, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[mf.ma[, 1] == 1:3, 8], mf.ma[mf.ma[, 1] == 1:3, 4], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[mm.ma[, 1] == 1:3, 8], mm.ma[mm.ma[, 1] == 1:3, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], wf.wm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], wf.wa[, 5], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], wm.wa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], tf.tm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], tf.ta[, 5], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], tm.ta[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], sf.sm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], sf.sa[, 5], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], sm.sa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], mf.mm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], mf.ma[, 5], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], mm.ma[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#####DISSIMILARITY#####
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], 1 - wf.wm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], 1 - wf.wa[, 4], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], 1 - wm.wa[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], 1 - tf.tm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], 1 - tf.ta[, 4], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], 1 - tm.ta[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], 1 - sf.sm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], 1 - sf.sa[, 4], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], 1 - sm.sa[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], 1 - mf.mm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], 1 - mf.ma[, 4], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], 1 - mm.ma[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], 1 - wf.wm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], 1 - wf.wa[, 5], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], 1 - wm.wa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], 1 - tf.tm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], 1 - tf.ta[, 5], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], 1 - tm.ta[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], 1 - sf.sm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], 1 - sf.sa[, 5], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], 1 - sm.sa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], 1 - mf.mm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], 1 - mf.ma[, 5], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], 1 - mm.ma[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#####DISSIMILARITY#####
##Jaccard estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], 1 - wf.wm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], 1 - wf.wa[, 4], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], 1 - wm.wa[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], 1 - tf.tm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], 1 - tf.ta[, 4], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], 1 - tm.ta[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], 1 - sf.sm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], 1 - sf.sa[, 4], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], 1 - sm.sa[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], 1 - mf.mm[, 4], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], 1 - mf.ma[, 4], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], 1 - mm.ma[, 4], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

##Sorensen estimate    
par(mfrow=c(4,3))
plot(wf.wm[, 8], 1 - wf.wm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wf.wa[, 8], 1 - wf.wa[, 5], main = "Woodland \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(wm.wa[, 8], 1 - wm.wa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.tm[, 8], 1 - tf.tm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tf.ta[, 8], 1 - tf.ta[, 5], main = "Tall-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(tm.ta[, 8], 1 - tm.ta[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sm[, 8], 1 - sf.sm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sf.sa[, 8], 1 - sf.sa[, 5], main = "Short-shurb \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(sm.sa[, 8], 1 - sm.sa[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.mm[, 8], 1 - mf.mm[, 5], main = "Forest - Mixed", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mf.ma[, 8], 1 - mf.ma[, 5], main = "Meadow \n Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
plot(mm.ma[, 8], 1 - mm.ma[, 5], main = "Mixed - Agriculture", xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))

#between biotopes####



#REGRESSION####
model1 <- lm((1 - b.div[, 4]) ~ b.div[, 8])
summary(model1)

par(mfrow=c(1,1))
plot(b.div[, 8], 1 - b.div[, 4])
abline(coef(model1[1]), coef(model1[2]), col = 2)
coef(model1)

model2 <- lm(b.div[, 4] ~ b.div[, 8])
summary(model2)

par(mfrow=c(1,1))
plot(b.div[, 8], b.div[, 4])
abline(coef(model2[1]), coef(model2[2]), col = 2)

#EACH LANDSCAPE
par(mfrow = c(3, 1))
forest <- c(wf, tf, sf, mf) 
datafor <- landsc_within(b.div, forest, forest)
modfor <- lm((1 - datafor[, 4]) ~ datafor[, 8])
summary(modfor)
plot((1 - datafor[, 4]) ~ datafor[, 8], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
abline(coef(modfor)[1], coef(modfor)[2], col = 2)

mixed <- c(wm, tm, sm, mm)
datamix <- landsc_within(b.div, mixed, mixed)
modmix <- lm((1 - datamix[, 4]) ~ datamix[, 8])
summary(modmix)
plot((1 - datamix[, 4]) ~ datamix[, 8], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
abline(coef(modmix)[1], coef(modmix)[2], col = 2)

meadow <- c(wa, ta, sa, ma)
datamea <- landsc_within(b.div, meadow, meadow)
modmea <- lm((1 - datamea[, 4]) ~ datamea[, 8])
summary(modmea)
plot((1 - datamea[, 4]) ~ datamea[, 8], xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), ylim = c(0, 1))
abline(coef(modmea)[1], coef(modmea)[2], col = 2)

#BOOTSTRAPPING REGRESSION MODELS
boot.huber <- function(data, indices, maxit = 20){
  data <- data[indices, ] #select obs. in bootstrap sample
  mod <- rlm(Jabd ~ ipcc, data = data, maxit = maxit)
  coefficients(mod) #return coefficient vector
}

library(boot)
head(as.data.frame(b.div))
duncan.boot <- boot(as.data.frame(b.div), boot.huber, 2000, maxit = 100)
duncan.boot

plot(duncan.boot, index = 1)
plot(duncan.boot, index = 2)

#BOOTSTRAP CONFIDENCE INTERVALS####
boot.ci(duncan.boot, index = 1, type = c("norm", "perc", "bca"))
boot.ci(duncan.boot, index = 2, type = c("norm", "perc", "bca"))

#SENSITIVITY OF THE STATISTIC
par(mfcol = c(2, 1))
jack.after.boot(duncan.boot, index = 1, main = "(a) income coefficient")
jack.after.boot(duncan.boot, index = 2, main = "(b) education coefficient")

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
