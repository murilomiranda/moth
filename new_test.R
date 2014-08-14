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
head(year1.res)
par(mfrow=c(3, 2))
plot(DIST1[, 3], year1.res[, 15], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard unadjusted")
plot(DIST1[, 3], year1.res[, 16], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen unadjusted")
plot(DIST1[, 3], year1.res[, 19], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Jaccard adjusted")
plot(DIST1[, 3], year1.res[, 20], ylim = c(0, 1), xlab = "Geographic Distance (m)", ylab = expression(~beta*"-diversity"), main = "Sorensen adjusted")
hist(DIST1[, 3])
hist(DIST2[, 3])
