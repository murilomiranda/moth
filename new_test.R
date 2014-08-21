##test site1 = 3, site2 = 17, period = 1
(species_s1 <- cbind(as.vector(data[data[, 2] == 3 & data[, 3] == 1, 4]), data[data[, 2] == 3 & data[, 3] == 1, 5]))
(species_s2 <- cbind(as.vector(data[data[, 2] == 17 & data[, 3] == 1, 4]), data[data[, 2] == 17 & data[, 3] == 1, 5]))
attributes(species_s1); is.matrix(species_s1)
attributes(species_s2); is.matrix(species_s2)

(shared <- intersect(ONESP(species_s1), ONESP(species_s2)))
(shared <- cbind(shared, matrix("0", nrow = length(shared)), matrix("0", nrow = length(shared))))
colnames(shared) <- c("spec", "abund1", "abund2")
shared

for(sp in 1:dim(shared)[1]){
  shared[sp, 2] <- species_s1[species_s1[, 1] == shared[sp], ][2]
  shared[sp, 3] <- species_s2[species_s2[, 1] == shared[sp], ][2]
}
shared

chaom <- function(pmin = 1, pmax = 6, B = 10, file){
  results <- NULL
  temp_result <- NULL
  Jabd <- NULL
  Sabd <- NULL
  u_pt1 <- NULL
  v_pt1 <- NULL
  U <- NULL
  V <- NULL
  
  leng_test <- function(base){
    if(is.matrix(base)){
      return(dim(base)[1])
    }else if(is.vector(base)){
      return(length(base) - 1)
    }
  }
  
  ONESP <- function(base){
    if(is.vector(base))
      return(base[1])
    else return(base[, 1])
  }
  
  for(p in pmin:pmax){
    for(s1 in 1:83){ #change to calculate lower triangule
      for(s2 in (s1+1):84){
        n <- 0
        m <- 0
        u_pt1 <- 0
        v_pt1 <- 0
        u_pt2 <- 0
        v_pt2 <- 0
        U <- 0
        V <- 0
        species_s1 <- cbind(as.vector(data[data[, 2] == s1 & data[, 3] == p, 4]), data[data[, 2] == s1 & data[, 3] == p, 5])
        species_s2 <- cbind(as.vector(data[data[, 2] == s2 & data[, 3] == p, 4]), data[data[, 2] == s2 & data[, 3] == p, 5])
        
        if(leng_test(species_s1) > 0){
          if(leng_test(species_s2) > 0){
            shared <- intersect(ONESP(species_s1), ONESP(species_s2))
            if(length(shared) >= 1){
              shared <- cbind(shared, matrix("0", nrow = length(shared)), matrix("0", nrow = length(shared)))
              colnames(shared) <- c("spec", "abund1", "abund2")
              
              for(sp in 1:dim(shared)[1]){
                shared[sp, 2] <- species_s1[species_s1[, 1] == shared[sp], ][2]
                shared[sp, 3] <- species_s2[species_s2[, 1] == shared[sp], ][2]
              }
              
              temp_shar <- NULL
              temp_shar <- cbind(as.numeric(shared[, 2]), as.numeric(shared[, 3]))
              
              if(!is.null(shared)){
                n <- if(is.vector(species_s1)){as.numeric(species_s1[2])}else{sum(as.numeric(species_s1[, 2]))}
                m <- if(is.vector(species_s2)){as.numeric(species_s2[2])}else{sum(as.numeric(species_s2[, 2]))}
                
                u_pt1 <- sum(temp_shar[, 1]/n)
                v_pt1 <- sum(temp_shar[, 2]/m)
                
                sum_u <- 0
                sum_v <- 0
                f1m <- 0
                fm1 <- 0
                f2m <- 0
                fm2 <- 0
                
                for(i in 1:dim(temp_shar)[1]){
                  if(temp_shar[i, 2] == 1){ #species_s1
                    sum_u <- sum_u + temp_shar[i, 1]/n
                    if(temp_shar[i, 1] >= 1) fm1 <- fm1 + 1
                  }
                  if(temp_shar[i, 1] == 1){ #species_s2
                    sum_v <- sum_v + temp_shar[i, 2]/m
                    if(temp_shar[i, 1] >= 1) f1m <- f1m + 1
                  }
                  if(temp_shar[i, 2] == 2){ #species_s1
                    if(temp_shar[i, 1] >= 1) fm2 <- fm2 + 1
                  }
                  if(temp_shar[i, 1] == 2){ #species_s2
                    if(temp_shar[i, 2] >= 1) f2m <- f2m + 1
                  }
                }
                
                if(fm2 == 0) fm2 <- 1
                if(f2m == 0) f2m <- 1
                
                u_pt2 <- fm1*sum_u/(2*fm2)
                v_pt2 <- f1m*sum_v/(2*f2m)
                
                U <- u_pt1 + (m - 1)*u_pt2/m
                V <- v_pt1 + (n - 1)*v_pt2/n
                
                if(U > 1) U <- 1
                if(V > 1) V <- 1
                
                Jabd <- U*V/(U + V - U*V)
                Sabd <- 2*U*V/(U + V)
                temp_result <- cbind(p, s1, s2, Jabd, Sabd)
              }else{
                Jabd <- 0
                Sabd <- 0
                temp_result <- cbind(p, s1, s2, Jabd, Sabd)
              }
            }else{
              Jabd <- 0
              Sabd <- 0
              temp_result <- cbind(p, s1, s2, Jabd, Sabd)
            }
          }else{
            Jabd <- 0
            Sabd <- 0
            temp_result <- cbind(p, s1, s2, Jabd, Sabd)
          }
        }
        results <- rbind(results, temp_result)
      }
    }
  }
  write.table(results, file = paste(file, "R.csv", sep = ""), row.names = FALSE)
  results
}

tab <- table(data[data[, 3] == 1, 4], data[data[, 3]  == 1, 2], data[data[, 3] == 1, 5])
sp <- attributes(tab)$dimnames[[1]]
site <- as.numeric(attributes(tab)$dimnames[[2]])
abun <- as.numeric(attributes(tab)$dimnames[[3]])
year1 <- cbind(matrix(0, nrow = length(sp), ncol = 84))

for(ab in 1:length(abun)){
  for(s1 in 1:length(site)){
    for(row in 1:length(sp)){
      if(tab[row, s1, ab] >= 1){
        year1[row, site[s1]] <- abun[ab]
      }
    }
  }
}

year1
apply(year1, 2, sum)
plot(table(year1)[-1])
rownames(year1) <- sp
year1[which(year1[, 1] != 0)]

s1 <- 1
s2 <- 5
shared1 <- NULL
nrow <- NULL
for(i in 1:length(sp)){
  if(as.numeric(year1[i, s1]) != 0){
    if(as.numeric(year1[i, s2]) != 0){
      nrow <- c(as.numeric(year1[i, s1]), as.numeric(year1[i, s2]))
      shared1 <- rbind(shared1, nrow)
    }
  }  
}

shared1

species_s1 <- year1[which(year1[, s1] != 0), s1]
sp1 <- attributes(species_s1)$names
n <- sum(species_s1)
nsp1 <- length(species_s1)
species_s2 <- year1[which(year1[, s2] != 0), s2]
sp2 <- attributes(species_s2)$names
m <- sum(species_s2)
nsp2 <- length(species_s2)
shared <- intersect(sp1, sp2)
nsh <- length(shared)

data.shared <- matrix(0, nrow = length(shared), ncol = 2)
rownames(data.shared) <- shared
data.shared

for(i in 1:length(shared)){
  data.shared[i, 1] <- shared1[i, 1]
  data.shared[i, 2] <- shared1[i, 2]
}

data.shared

(f11 <- dim(shared1[which((shared1[, 1] == 1) & (shared1[, 2] == 1)), ])[1])
(fm1 <- dim(shared1[which((shared1[, 1] >= 1) & (shared1[, 2] == 1)), ])[1])
(fm2 <- dim(shared1[which((shared1[, 1] >= 1) & (shared1[, 2] == 2)), ])[1])
(f1m <- dim(shared1[which((shared1[, 1] == 1) & (shared1[, 2] >= 1)), ])[1])
(f2m <- dim(shared1[which((shared1[, 1] == 2) & (shared1[, 2] >= 1)), ])[1])

if(fm2 == 0) fm2 <- 1
if(f2m == 0) f2m <- 1

sum_u1 <- sum(shared1[, 1])/n
sum_v1 <- sum(shared1[, 2])/m

sum_u2 <- sum(shared1[which(shared1[, 2] == 1), 1])/n
sum_v2 <- sum(shared1[which(shared1[, 1] == 1), 2])/m

U <- sum_u1 + ((m - 1)/m)*(fm1/(2*fm2))*sum_u2
V <- sum_v1 + ((n - 1)/n)*(f1m/(2*f2m))*sum_v2

if(U > 1) U <- 1
if(V > 1) V <- 1

Junad <- sum_u1*sum_v1/(sum_u1 + sum_v1 - sum_u1*sum_v1)
Lunad <- 2*sum_u1*sum_v1/(sum_u1 + sum_v1)

Jabd <- U*V/(U + V - U*V)
Labd <- 2*U*V/(U + V)

n; m; nsp1; nsp2; nsh
f11; f1m; fm1; f2m; fm2
sum_u1; sum_v1
U; V
Junad; Lunad
Jabd; Labd

######TESTE WITH DATA SPARE####
spare <- read.table("Data5a.txt", h = F)
head(spare)

nobs1 <- sum(spare[, 1])
nobs2 <- sum(spare[, 3])
nsp1 <- length(spare[which(spare[, 1] != 0), 1])
nsp2 <- length(spare[which(spare[, 3] != 0), 3])

shared1 <- NULL
nrow <- NULL
for(i in 1:dim(spare)[1]){
  if(spare[i, 1] != 0){
    if(spare[i, 3] != 0){
      nrow <- c(spare[i, 1], spare[i, 3])
      shared1 <- rbind(shared1, nrow)
    }
  }  
}
shared1
nsh <- dim(shared1)[1]

f11 <- dim(shared1[which((shared1[, 1] == 1) & (shared1[, 2] == 1)), ])[1]
fm1 <- dim(shared1[which((shared1[, 1] >= 1) & (shared1[, 2] == 1)), ])[1]
fm2 <- dim(shared1[which((shared1[, 1] >= 1) & (shared1[, 2] == 2)), ])[1]
f1m <- dim(shared1[which((shared1[, 1] == 1) & (shared1[, 2] >= 1)), ])[1]
f2m <- dim(shared1[which((shared1[, 1] == 2) & (shared1[, 2] >= 1)), ])[1]
sum_u1 <- sum(shared1[, 1])/nobs1
sum_v1 <- sum(shared1[, 2])/nobs2
sum_u2 <- sum(shared1[which(shared1[, 2] == 1), 1])/nobs1
sum_v2 <- sum(shared1[which(shared1[, 1] == 1), 2])/nobs2

if(fm2 == 0) fm2 <- 1
if(f2m == 0) f2m <- 1

U <- sum_u1 + ((nobs2 - 1)/nobs2)*(fm1/(2*fm2))*sum_u2
V <- sum_v1 + ((nobs1 - 1)/nobs1)*(f1m/(2*f2m))*sum_v2

if(U > 1) U <- 1
if(V > 1) V <- 1

Junad <- sum_u1*sum_v1/(sum_u1 + sum_v1 - sum_u1*sum_v1)
Lunad <- 2*sum_u1*sum_v1/(sum_u1 + sum_v1)

Jabd <- U*V/(U + V - U*V)
Labd <- 2*U*V/(U + V)

nobs1; nobs2; nsp1; nsp2; nsh
f11; f1m; fm1; f2m; fm2
sum_u1; sum_v1
U; V
Junad; Lunad
Jabd; Labd

year1.res <- chao2(period = 1, file = "test_chao2")
year2.res <- chao2(period = 2, file = "test_chao2")
year3.res <- chao2(period = 3, file = "test_chao2")
year4.res <- chao2(period = 4, file = "test_chao2")
year5.res <- chao2(period = 5, file = "test_chao2")
year6.res <- chao2(period = 6, file = "test_chao2")

bisamples <- function(base1, base2){
  period <- matrix(0, nrow = dim(base1)[1], ncol = dim(base1)[2])
  for(i in 1:dim(base1)[1]){
    period[i, 1] <- mean(base1[i, 1], base2[i, 1])
    period[i, 2] <- mean(base1[i, 2], base2[i, 2])
    period[i, 3] <- mean(base1[i, 3], base2[i, 3])
    period[i, 4] <- mean(base1[i, 4], base2[i, 4])
    period[i, 5] <- mean(base1[i, 5], base2[i, 5])
    period[i, 6] <- mean(base1[i, 6], base2[i, 6])
    period[i, 7] <- mean(base1[i, 7], base2[i, 7])
    period[i, 8] <- mean(base1[i, 8], base2[i, 8])
    period[i, 9] <- mean(base1[i, 9], base2[i, 9])
    period[i, 10] <- mean(base1[i, 10], base2[i, 10])
    period[i, 11] <- mean(base1[i, 11], base2[i, 11])
    period[i, 12] <- mean(base1[i, 12], base2[i, 12])
    period[i, 13] <- mean(base1[i, 13], base2[i, 13])
    period[i, 14] <- mean(base1[i, 14], base2[i, 14])
    period[i, 15] <- mean(base1[i, 15], base2[i, 15])
    period[i, 16] <- mean(base1[i, 16], base2[i, 16])
    period[i, 17] <- mean(base1[i, 17], base2[i, 17])
    period[i, 18] <- mean(base1[i, 18], base2[i, 18])
    period[i, 19] <- mean(base1[i, 19], base2[i, 19])
    period[i, 20] <- mean(base1[i, 20], base2[i, 20])
  }
  period
}

trisamples <- function(base1, base2, base3){
  period <- matrix(0, nrow = dim(base1)[1], ncol = dim(base1)[2])
  for(i in 1:dim(base1)[1]){
    period[i, 1] <- mean(base1[i, 1], base2[i, 1], base3[i, 1])
    period[i, 2] <- mean(base1[i, 2], base2[i, 2], base3[i, 2])
    period[i, 3] <- mean(base1[i, 3], base2[i, 3], base3[i, 3])
    period[i, 4] <- mean(base1[i, 4], base2[i, 4], base3[i, 4])
    period[i, 5] <- mean(base1[i, 5], base2[i, 5], base3[i, 5])
    period[i, 6] <- mean(base1[i, 6], base2[i, 6], base3[i, 6])
    period[i, 7] <- mean(base1[i, 7], base2[i, 7], base3[i, 7])
    period[i, 8] <- mean(base1[i, 8], base2[i, 8], base3[i, 8])
    period[i, 9] <- mean(base1[i, 9], base2[i, 9], base3[i, 9])
    period[i, 10] <- mean(base1[i, 10], base2[i, 10], base3[i, 10])
    period[i, 11] <- mean(base1[i, 11], base2[i, 11], base3[i, 11])
    period[i, 12] <- mean(base1[i, 12], base2[i, 12], base3[i, 12])
    period[i, 13] <- mean(base1[i, 13], base2[i, 13], base3[i, 13])
    period[i, 14] <- mean(base1[i, 14], base2[i, 14], base3[i, 14])
    period[i, 15] <- mean(base1[i, 15], base2[i, 15], base3[i, 15])
    period[i, 16] <- mean(base1[i, 16], base2[i, 16], base3[i, 16])
    period[i, 17] <- mean(base1[i, 17], base2[i, 17], base3[i, 17])
    period[i, 18] <- mean(base1[i, 18], base2[i, 18], base3[i, 18])
    period[i, 19] <- mean(base1[i, 19], base2[i, 19], base3[i, 19])
    period[i, 20] <- mean(base1[i, 20], base2[i, 20], base3[i, 20])
  }
  period
}

## For first year
year1 <- trisamples(year1.res, year2.res, year3.res)

## For second year
year2 <- trisamples(year4.res, year5.res, year6.res)

## For all traps
year.all <- bisamples(year1, year2)


head(year1.res)
par(mfrow=c(3, 2))
plot(DIST1[, 3], year1.res[, 15], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard unadjusted")
plot(DIST1[, 3], year1.res[, 16], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen unadjusted")
plot(DIST1[, 3], year1.res[, 19], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
plot(DIST1[, 3], year1.res[, 20], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
hist(DIST1[, 3])
hist(DIST2[, 3])

plot(DIST1[, 3], 1 - year1.res[, 15], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard unadjusted")
plot(DIST1[, 3], 1 - year1.res[, 16], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen unadjusted")
plot(DIST1[, 3], 1 - year1.res[, 19], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
plot(DIST1[, 3], 1 - year1.res[, 20], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
hist(DIST1[, 3])
hist(DIST2[, 3])

dist <- seq(0, 7500, 500)
length(dist)

figure.year <- function(base){
  #Jun, Sun, Jad, Sad
  Jun <- matrix(0, nrow = 15, ncol = 2)
  Sun <- matrix(0, nrow = 15, ncol = 2)
  Jad <- matrix(0, nrow = 15, ncol = 2)
  Sad <- matrix(0, nrow = 15, ncol = 2)
  freq <- matrix(0, nrow = 15)
  for(i in 1:(dim(DIST1)[1])){
    if(DIST1[i, 3] < 500){
      Jun[1, 1] <- Jun[1, 1] + base[i, 15]
      Sun[1, 1] <- Sun[1, 1] + base[i, 16]
      Jad[1, 1] <- Jad[1, 1] + base[i, 19]
      Sad[1, 1] <- Sad[1, 1] + base[i, 20]
      freq[1] <- freq[1] + 1
      Jun[1, 2] <- Jun[1, 2] + base[i, 15]^2
      Sun[1, 2] <- Sun[1, 2] + base[i, 16]^2
      Jad[1, 2] <- Jad[1, 2] + base[i, 19]^2
      Sad[1, 2] <- Sad[1, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 500) & (DIST1[i, 3] < 1000)){
      Jun[2, 1] <- Jun[2, 1] + base[i, 15]
      Sun[2, 1] <- Sun[2, 1] + base[i, 16]
      Jad[2, 1] <- Jad[2, 1] + base[i, 19]
      Sad[2, 1] <- Sad[2, 1] + base[i, 20]
      freq[2] <- freq[2] + 1
      Jun[2, 2] <- Jun[2, 2] + base[i, 15]^2
      Sun[2, 2] <- Sun[2, 2] + base[i, 16]^2
      Jad[2, 2] <- Jad[2, 2] + base[i, 19]^2
      Sad[2, 2] <- Sad[2, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 1000) & (DIST1[i, 3] < 1500)){
      Jun[3, 1] <- Jun[3, 1] + base[i, 15]
      Sun[3, 1] <- Sun[3, 1] + base[i, 16]
      Jad[3, 1] <- Jad[3, 1] + base[i, 19]
      Sad[3, 1] <- Sad[3, 1] + base[i, 20]
      freq[3] <- freq[3] + 1
      Jun[3, 2] <- Jun[3, 2] + base[i, 15]^2
      Sun[3, 2] <- Sun[3, 2] + base[i, 16]^2
      Jad[3, 2] <- Jad[3, 2] + base[i, 19]^2
      Sad[3, 2] <- Sad[3, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 1500) & (DIST1[i, 3] < 2000)){
      Jun[4, 1] <- Jun[4, 1] + base[i, 15]
      Sun[4, 1] <- Sun[4, 1] + base[i, 16]
      Jad[4, 1] <- Jad[4, 1] + base[i, 19]
      Sad[4, 1] <- Sad[4, 1] + base[i, 20]
      freq[4] <- freq[4] + 1
      Jun[4, 2] <- Jun[4, 2] + base[i, 15]^2
      Sun[4, 2] <- Sun[4, 2] + base[i, 16]^2
      Jad[4, 2] <- Jad[4, 2] + base[i, 19]^2
      Sad[4, 2] <- Sad[4, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 2000) & (DIST1[i, 3] < 2500)){
      Jun[5, 1] <- Jun[5, 1] + base[i, 15]
      Sun[5, 1] <- Sun[5, 1] + base[i, 16]
      Jad[5, 1] <- Jad[5, 1] + base[i, 19]
      Sad[5, 1] <- Sad[5, 1] + base[i, 20]
      freq[5] <- freq[5] + 1
      Jun[5, 2] <- Jun[5, 2] + base[i, 15]^2
      Sun[5, 2] <- Sun[5, 2] + base[i, 16]^2
      Jad[5, 2] <- Jad[5, 2] + base[i, 19]^2
      Sad[5, 2] <- Sad[5, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 2500) & (DIST1[i, 3] < 3000)){
      Jun[6, 1] <- Jun[6, 1] + base[i, 15]
      Sun[6, 1] <- Sun[6, 1] + base[i, 16]
      Jad[6, 1] <- Jad[6, 1] + base[i, 19]
      Sad[6, 1] <- Sad[6, 1] + base[i, 20]
      freq[6] <- freq[6] + 1
      Jun[6, 2] <- Jun[6, 2] + base[i, 15]^2
      Sun[6, 2] <- Sun[6, 2] + base[i, 16]^2
      Jad[6, 2] <- Jad[6, 2] + base[i, 19]^2
      Sad[6, 2] <- Sad[6, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 3000) & (DIST1[i, 3] < 3500)){
      Jun[7, 1] <- Jun[7, 1] + base[i, 15]
      Sun[7, 1] <- Sun[7, 1] + base[i, 16]
      Jad[7, 1] <- Jad[7, 1] + base[i, 19]
      Sad[7, 1] <- Sad[7, 1] + base[i, 20]
      freq[7] <- freq[7] + 1
      Jun[7, 2] <- Jun[7, 2] + base[i, 15]^2
      Sun[7, 2] <- Sun[7, 2] + base[i, 16]^2
      Jad[7, 2] <- Jad[7, 2] + base[i, 19]^2
      Sad[7, 2] <- Sad[7, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 3500) & (DIST1[i, 3] < 4000)){
      Jun[8, 1] <- Jun[8, 1] + base[i, 15]
      Sun[8, 1] <- Sun[8, 1] + base[i, 16]
      Jad[8, 1] <- Jad[8, 1] + base[i, 19]
      Sad[8, 1] <- Sad[8, 1] + base[i, 20]
      freq[8] <- freq[8] + 1
      Jun[8, 2] <- Jun[8, 2] + base[i, 15]^2
      Sun[8, 2] <- Sun[8, 2] + base[i, 16]^2
      Jad[8, 2] <- Jad[8, 2] + base[i, 19]^2
      Sad[8, 2] <- Sad[8, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 4000) & (DIST1[i, 3] < 4500)){
      Jun[9, 1] <- Jun[9, 1] + base[i, 15]
      Sun[9, 1] <- Sun[9, 1] + base[i, 16]
      Jad[9, 1] <- Jad[9, 1] + base[i, 19]
      Sad[9, 1] <- Sad[9, 1] + base[i, 20]
      freq[9] <- freq[9] + 1
      Jun[9, 2] <- Jun[9, 2] + base[i, 15]^2
      Sun[9, 2] <- Sun[9, 2] + base[i, 16]^2
      Jad[9, 2] <- Jad[9, 2] + base[i, 19]^2
      Sad[9, 2] <- Sad[9, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 4500) & (DIST1[i, 3] < 5000)){
      Jun[10, 1] <- Jun[10, 1] + base[i, 15]
      Sun[10, 1] <- Sun[10, 1] + base[i, 16]
      Jad[10, 1] <- Jad[10, 1] + base[i, 19]
      Sad[10, 1] <- Sad[10, 1] + base[i, 20]
      freq[10] <- freq[10] + 1
      Jun[10, 2] <- Jun[10, 2] + base[i, 15]^2
      Sun[10, 2] <- Sun[10, 2] + base[i, 16]^2
      Jad[10, 2] <- Jad[10, 2] + base[i, 19]^2
      Sad[10, 2] <- Sad[10, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 5000) & (DIST1[i, 3] < 5500)){
      Jun[11, 1] <- Jun[11, 1] + base[i, 15]
      Sun[11, 1] <- Sun[11, 1] + base[i, 16]
      Jad[11, 1] <- Jad[11, 1] + base[i, 19]
      Sad[11, 1] <- Sad[11, 1] + base[i, 20]
      freq[11] <- freq[11] + 1
      Jun[11, 2] <- Jun[11, 2] + base[i, 15]^2
      Sun[11, 2] <- Sun[11, 2] + base[i, 16]^2
      Jad[11, 2] <- Jad[11, 2] + base[i, 19]^2
      Sad[11, 2] <- Sad[11, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 5500) & (DIST1[i, 3] < 6000)){
      Jun[12, 1] <- Jun[12, 1] + base[i, 15]
      Sun[12, 1] <- Sun[12, 1] + base[i, 16]
      Jad[12, 1] <- Jad[12, 1] + base[i, 19]
      Sad[12, 1] <- Sad[12, 1] + base[i, 20]
      freq[12] <- freq[12] + 1
      Jun[12, 2] <- Jun[12, 2] + base[i, 15]^2
      Sun[12, 2] <- Sun[12, 2] + base[i, 16]^2
      Jad[12, 2] <- Jad[12, 2] + base[i, 19]^2
      Sad[12, 2] <- Sad[12, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 6000) & (DIST1[i, 3] < 6500)){
      Jun[13, 1] <- Jun[13, 1] + base[i, 15]
      Sun[13, 1] <- Sun[13, 1] + base[i, 16]
      Jad[13, 1] <- Jad[13, 1] + base[i, 19]
      Sad[13, 1] <- Sad[13, 1] + base[i, 20]
      freq[13] <- freq[13] + 1
      Jun[13, 2] <- Jun[13, 2] + base[i, 15]^2
      Sun[13, 2] <- Sun[13, 2] + base[i, 16]^2
      Jad[13, 2] <- Jad[13, 2] + base[i, 19]^2
      Sad[13, 2] <- Sad[13, 2] + base[i, 20]^2
    }else if((DIST1[i, 3] >= 6500) & (DIST1[i, 3] < 7000)){
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
}

figure.year(year1.res)
figure.year(year2.res)
figure.year(year3.res)
figure.year(year4.res)
figure.year(year5.res)
figure.year(year6.res)
figure.year(year1)
figure.year(year2)
figure.year(year.all)

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

figure.div(wf.wf)
figure.div(wm.wm)
figure.div(wa.wa)
