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
tab
attributes(tab)
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

shared1 <- NULL
nrow <- NULL
for(i in 1:length(sp)){
  if(as.numeric(year1[i, 3]) != 0){
    if(as.numeric(year1[i, 17]) != 0){
      nrow <- c(as.numeric(year1[i, 3]), as.numeric(year1[i, 17]))
      shared1 <- rbind(shared1, nrow)
    }
  }  
}

shared1

species_s1 <- year1[which(year1[, 3] != 0), 3]
sp1 <- attributes(species_s1)$names
species_s2 <- year1[which(year1[, 17] != 0), 17]
sp2 <- attributes(species_s2)$names
shared <- intersect(sp1, sp2)
length(shared)
data.shared <- matrix(0, nrow = length(shared), ncol = 2)
rownames(data.shared) <- shared
data.shared

for(i in 1:length(shared)){
  data.shared[i, 1] <- shared1[i, 1]
  data.shared[i, 2] <- shared1[i, 2]
}

data.shared
