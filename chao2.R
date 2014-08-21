chao2 <- function(period = c(1:6), file = "test"){
  for(p in period){
    #create the 3D data.set
    tab <- table(data[data[, 3] == p, 4], data[data[, 3]  == p, 2], data[data[, 3] == p, 5])
    species <- attributes(tab)$dimnames[[1]]
    site <- as.numeric(attributes(tab)$dimnames[[2]])
    abun <- as.numeric(attributes(tab)$dimnames[[3]])
    year <- cbind(matrix(0, nrow = length(species), ncol = 84))

    #recreate in 2D data.set
    for(ab in 1:length(abun)){
      for(s1 in 1:length(site)){
        for(row in 1:length(species)){
          if(tab[row, s1, ab] >= 1){
            year[row, site[s1]] <- abun[ab]
          }
        }
      }
    }
    
    #cat("\nYEAR\n")
    #print(year)
    #print(apply(year, 2, sum))
    #plot(table(year)[-1])
    rownames(year) <- species
    #year[which(year[, 1] != 0)]

    #Calculate each pair of beta diversity using Chao et al (2005)
    RESULTS <- NULL
    
    for(s1 in 1:83){
      for(s2 in (s1+1):84){
        #Create the data.set for shared species
        shared <- NULL
        nrow <- NULL
        for(i in 1:length(species)){
          if(as.numeric(year[i, s1]) != 0){
            if(as.numeric(year[i, s2]) != 0){
              nrow <- c(as.numeric(year[i, s1]), as.numeric(year[i, s2]))
              shared <- rbind(shared, nrow)
            }
          }  
        }
        
        #Species in site 1
        species_s1 <- year[which(year[, s1] != 0), s1]
        n <- sum(species_s1)
        nsp1 <- length(species_s1)
        
        #Species in site 2
        species_s2 <- year[which(year[, s2] != 0), s2]
        m <- sum(species_s2)
        nsp2 <- length(species_s2)
        
        #Verify if there is shared species
        if(!is.null(shared)){
          #cat("\n SHARED 1 \n")
          #print(shared)
         
          #Species in site 1
          sp1 <- attributes(species_s1)$names
          #cat("\n SPECIES 1 \n")
          #print(sp1)
          #Species in site 2
          sp2 <- attributes(species_s2)$names
          #cat("\n SPECIES 2 \n")
          #print(sp2)  
          #Shared species in site 1 and 2
          sp.shared <- intersect(sp1, sp2)
          nsh <- length(sp.shared)
          #cat("\n SHARED SPECIES \n")
          #print(sp.shared)
          #print(nsh)
          
          data.shared <- matrix(0, nrow = length(sp.shared), ncol = 2)
          rownames(data.shared) <- sp.shared
        
          #cat("\n SHARED 2 \n")
          #print(data.shared)

          for(i in 1:length(sp.shared)){
            data.shared[i, 1] <- shared[i, 1]
            data.shared[i, 2] <- shared[i, 2]
          }
        
          #cat("\n SHARED 3 \n")
          #print(data.shared)
          
         
          f11 <- dim(shared[which((shared[, 1] == 1) & (shared[, 2] == 1)), ])[1]
          fm1 <- dim(shared[which((shared[, 1] >= 1) & (shared[, 2] == 1)), ])[1]
          fm2 <- dim(shared[which((shared[, 1] >= 1) & (shared[, 2] == 2)), ])[1]
          f1m <- dim(shared[which((shared[, 1] == 1) & (shared[, 2] >= 1)), ])[1]
          f2m <- dim(shared[which((shared[, 1] == 2) & (shared[, 2] >= 1)), ])[1]

          #cat(" SHARED VALUES \n")
          #cat("f11 ", f11, " fm1 ", fm1, " fm2 ", fm2, " f1m ", f1m, " f2m ", f2m, "\n")
          
          if(is.null(fm2)) fm2 <- 0
          if(fm2 == 0) fm2 <- 1
          if(is.null(f2m)) f2m <- 0
          if(f2m == 0) f2m <- 1
          if(is.null(fm1)) fm1 <- 0
          if(is.null(f1m)) f1m <- 0
                  
          #cat("f11 ", f11, " fm1 ", fm1, " fm2 ", fm2, " f1m ", f1m, " f2m ", f2m, "\n")
          
          sum_u1 <- sum(shared[, 1])/n
          sum_v1 <- sum(shared[, 2])/m
          #cat("sum_u1 ", sum_u1, " sum_v1 ", sum_v1)

          sum_u2 <- sum(shared[which(shared[, 2] == 1), 1])/n
          sum_v2 <- sum(shared[which(shared[, 1] == 1), 2])/m

          U <- sum_u1 + ((m - 1)/m)*(fm1/(2*fm2))*sum_u2
          V <- sum_v1 + ((n - 1)/n)*(f1m/(2*f2m))*sum_v2

          #cat("Ub ", U, " Vb ", V)
          
          if(U > 1) U <- 1
          if(V > 1) V <- 1

          #cat("Ua ", U, " Va ", V)
          
          Junad <- sum_u1*sum_v1/(sum_u1 + sum_v1 - sum_u1*sum_v1)
          Lunad <- 2*sum_u1*sum_v1/(sum_u1 + sum_v1)

          #cat("Junad ", Junad, " Lunad ", Lunad)
          
          Jabd <- U*V/(U + V - U*V)
          Labd <- 2*U*V/(U + V)
        
          #cat("Jabd ", Jabd, " Labd ", Labd)
          
          results <- cbind(p, s1, s2, n, nsp1, m, nsp2, nsh, fm1, fm2, f1m, f2m, sum_u1, sum_v1, Junad, Lunad, U, V, Jabd, Labd)
        }else{ #if(!is.null(shared))
          results <- cbind(p, s1, s2, n, nsp1, m, nsp2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        }
        RESULTS <- rbind(RESULTS, results)
      } #for(s2 in (s1+1):84)
    }#for(s1 in 1:83)
    write.table(RESULTS, file = paste(file, p, "R.csv", sep = ""), row.names = FALSE)
  } #for(p in period)
  RESULTS
}