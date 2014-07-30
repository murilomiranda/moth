chao <- function(pmin = 1, pmax = 6, file){
  results <- NULL
  temp_resul <- NULL
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
    for(s1 in 1:3){ #change to calculate lower triangle
      for(s2 in (s1+1):4){
        cat(p, s1, s2, "\n")
        u_pt1 <- 0
        v_pt1 <- 0
        u_pt2 <- 0
        v_pt2 <- 0
        U <- 0
        V <- 0
        species_s1 <- cbind(as.vector(data[data[, 2] == s1 & data[, 3] == p, 4]), data[data[, 2] == s1 & data[, 3] == p, 5])
        species_s2 <- cbind(as.vector(data[data[, 2] == s2 & data[, 3] == p, 4]), data[data[, 2] == s2 & data[, 3] == p, 5])
        
        cat("\nSPECIES_S1\n")
        print(species_s1)
        cat("\nSPECIES_S2\n")
        print(species_s2)
        
        if(leng_test(species_s1) > 0){
          if(leng_test(species_s2) > 0){
            #JACCARD AND SORENSEN ESTIMATORS
            
            shared <- intersect(ONESP(species_s1), ONESP(species_s2))
            cat("\nFIRST SHARED\n")
            print(shared)
            
            if(length(shared) >= 1){
              shared <- cbind(shared, matrix("0", nrow = length(shared)), matrix("0", nrow = length(shared)))
              colnames(shared) <- c("spec", "abun1", "abun2")
              
              cat("\nS1", s1, "S2", s2, "\n")
              cat("\nFIRST SHARED\n")
              print(shared)
              
              for(sp in 1:dim(shared)[1]){
                shared[sp, 2] <- species_s1[species_s1[, 1] == shared[sp], ][2] 
                shared[sp, 3] <- species_s2[species_s2[, 1] == shared[sp], ][2]
              }
              
              cat("\nSECOND SHARED\n")
              print(shared)
              
              temp_shar <- NULL
              temp_shar <- cbind(as.numeric(shared[, 2]), as.numeric(shared[, 3]))
              
              if(any(is.na(temp_shar))) cat("\nNANNNNNNNNNNN\n")
              if(!is.null(shared)){
                n <- if(is.vector(species_s1)){as.numeric(species_s1[2])}else{sum(as.numeric(species_s1[, 2]))}
                m <- if(is.vector(species_s2)){as.numeric(species_s2[2])}else{sum(as.numeric(species_s2[, 2]))}
                
                u_pt1 <- sum(temp_shar[, 1]/n)
                v_pt1 <- sum(temp_shar[, 2]/m)
                print(temp_shar)
                cat("u_pt1", u_pt1, "\n")
                cat("v_pt1", v_pt1, "\n")
                
                sum_u <- 0
                sum_v <- 0
                f1m <- 0
                fm1 <- 0
                f2m <- 0
                fm2 <- 0
                
                
                for(i in 1:dim(temp_shar)[1]){
                  if(temp_shar[i, 2] == 1){ #species_s1
                    sum_u <- sum_u + temp_shar[i, 1]/n
                    if(as.numeric(temp_shar[i, 1]) >= 1) fm1 <- fm1 + 1
                  }
                  if(temp_shar[i, 1] == 1){ #species_s2
                    sum_v <- sum_v + temp_shar[i, 2]/m
                    if(temp_shar[i, 2] >= 1) f1m <- f1m + 1 
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
                1
                u_pt2 <- fm1*sum_u/(2*fm2)
                v_pt2 <- f1m*sum_v/(2*f2m)
                
                cat("sum_u", sum_u, "\n")
                cat("fm1", fm1, "\n")
                cat("sum_v", sum_v, "\n")
                cat("f1m", f1m, "\n")
                cat("fm2", fm2, "\n")
                cat("f2m", f2m, "\n")
                cat("u_pt2", u_pt2, "\n")
                cat("v_pt2", v_pt2, "\n")
                cat("m-1/m", (m-1)/m, "n-1/n", (n-1)/n, "\n")
                
                U <- u_pt1 + (m - 1)*u_pt2/m
                V <- v_pt1 + (n - 1)*v_pt2/n
                
                cat("U", U, "\n")
                cat("V", V, "\n")
                
                if(U > 1) U <- 1
                if(V > 1) V <- 1
                
                Jabd <- U*V/(U + V - U*V)
                Labd <- 2*U*V/(U + V)
                
                cat("Jabd", Jabd, "\n")
                cat("Labd", Labd, "\n")
                
                temp_resul <- cbind(p, s1, s2, Jabd, Labd)
              
              }else{
                Jabd <- 0
                Labd <- 0
                temp_resul <- cbind(p, s1, s2, Jabd, Labd)
              }
            
            }else{
              Jabd <- 0
              Labd <- 0
              temp_resul <- cbind(p, s1, s2, Jabd, Labd)
            }
          }else{
            Jabd <- 0
            Labd <- 0
            temp_resul <- cbind(p, s1, s2, Jabd, Labd)
          }
        }else{
          Jabd <- 0
          Labd <- 0
          temp_resul <- cbind(p, s1, s2, Jabd, Labd)
        }
        results <- rbind(results, temp_resul)  
        print(temp_resul)
      }
    }
  }
  write.table(results, file = paste(file, "R.csv", sep = ""), row.names = FALSE)
  results
}