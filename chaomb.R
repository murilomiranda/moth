chaom <- function(pmin = 1, pmax = 6, B = 10, file){
  results <- NULL
  temp_resul <- NULL
  Jabd <- NULL
  Labd <- NULL
  VarJ <- NULL
  VarL <- NULL
  VARJ <- NULL
  VARL <- NULL
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
    for(s1 in 1:83){ #change to calculate lower triangle
      for(s2 in (s1+1):84){
        #cat(p, s1, s2, "\n")
        u_pt1 <- 0
        v_pt1 <- 0
        u_pt2 <- 0
        v_pt2 <- 0
        U <- 0
        V <- 0
        species_s1 <- cbind(as.vector(data[data[, 2] == s1 & data[, 3] == p, 4]), data[data[, 2] == s1 & data[, 3] == p, 5])
        species_s2 <- cbind(as.vector(data[data[, 2] == s2 & data[, 3] == p, 4]), data[data[, 2] == s2 & data[, 3] == p, 5])
        

          #cat("\nSPECIES_S1\n")
          #print(species_s1)
          #cat("\nSPECIES_S2\n")
          #print(species_s2)
          #print(leng_test(species_s2))
        cat("\nS1", s1, "S2", s2, "\n")
        
        if(leng_test(species_s1) > 0){
          if(leng_test(species_s2) > 0){
            #JACCARD AND SORENSEN ESTIMATORS
            shared <- intersect(ONESP(species_s1), ONESP(species_s2))
            #cat("\nFIRST SHARED\n")
            #print(shared)
            
            if(length(shared) >= 1){
              shared <- cbind(shared, matrix("0", nrow = length(shared)), matrix("0", nrow = length(shared)))
              colnames(shared) <- c("spec", "abun1", "abun2")
              
              
              #cat("\nFIRST SHARED\n")
              #print(shared)
              
              for(sp in 1:dim(shared)[1]){
                shared[sp, 2] <- species_s1[species_s1[, 1] == shared[sp], ][2] 
                shared[sp, 3] <- species_s2[species_s2[, 1] == shared[sp], ][2]
              }
              
              #cat("\nSECOND SHARED\n")
              #print(shared)
              
              temp_shar <- NULL
              temp_shar <- cbind(as.numeric(shared[, 2]), as.numeric(shared[, 3]))
              
              #if(any(is.na(temp_shar))) cat("\nNANNNNNNNNNNN\n")
              if(!is.null(shared)){
                n <- if(is.vector(species_s1)){as.numeric(species_s1[2])}else{sum(as.numeric(species_s1[, 2]))}
                m <- if(is.vector(species_s2)){as.numeric(species_s2[2])}else{sum(as.numeric(species_s2[, 2]))}
                
                u_pt1 <- sum(temp_shar[, 1]/n)
                v_pt1 <- sum(temp_shar[, 2]/m)
                #print(temp_shar)
                #cat("u_pt1", u_pt1, "\n")
                #cat("v_pt1", v_pt1, "\n")
                
                sum_u <- 0
                sum_v <- 0
                f1m <- 0
                fm1 <- 0
                f2m <- 0
                fm2 <- 0
                
                #cat("\nTEMPORARY SHARED\n")
                #print(temp_shar)
                
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
                u_pt2 <- fm1*sum_u/(2*fm2)
                v_pt2 <- f1m*sum_v/(2*f2m)
                
                #cat("sum_u", sum_u, "\n")
                #cat("fm1", fm1, "\n")
                #cat("sum_v", sum_v, "\n")
                #cat("f1m", f1m, "\n")
                #cat("fm2", fm2, "\n")
                #cat("f2m", f2m, "\n")
                #cat("u_pt2", u_pt2, "\n")
                #cat("v_pt2", v_pt2, "\n")
                
                U <- u_pt1 + (m - 1)*u_pt2/m
                V <- v_pt1 + (n - 1)*v_pt2/n
                
                #cat("U", U, "\n")
                #cat("V", V, "\n")
                
                if(U > 1) U <- 1
                if(V > 1) V <- 1
                
                Jabd <- U*V/(U + V - U*V)
                Labd <- 2*U*V/(U + V)
                
                #cat("Jabd", Jabd, "\n")
                #cat("Labd", Labd, "\n")
                
                #BOOTSTRAP METHOD - VARIANCE ESTIMATORS
                if(B > 0){
                  species <- union(species_s1[, 1], species_s2[, 1])
                  species <- cbind(species, matrix("0", nrow = length(species)), matrix("0", nrow = length(species)))
                  colnames(species) <- c("spec", "abun1", "abun2")
                  
                  for(sp in 1:dim(species)[1]){
                    value <- species_s1[species_s1[, 1] == species[sp], ][2]
                    if(!is.na(value)) species[sp, 2] <- value
                    value <- species_s2[species_s2[, 1] == species[sp], ][2]
                    if(!is.na(value)) species[sp, 3] <- value
                  }
                  
                  VAREST <- matrix(0, nrow = B, ncol = 2)
                  colnames(VAREST) <- c("Jac", "Sor")
                  for(b in 1:B){
                    randnum <- trunc(runif(dim(species)[1], 1, dim(species)[1]))
                    randata <- NULL
                    for(sp in 1:dim(species)[1]){
                      randata <- rbind(randata, species[randnum[sp], ])
                    }
                    randata[, 1] <- paste(randata[, 1], seq(1, dim(species)[1]), "")
                    
                    #cat("RANDATA")
                    #print(randata)
                    
                    species_s1 <- randata[randata[, 2] > 0, 1:2]
                    species_s2 <- randata[randata[, 3] > 0, -2]
                    
                    #cat("SPECIES_S1")
                    #print(species_s1)
                    #cat("SPECIES_S2")
                    #print(species_s2)
                    
                    #cat("TESTES", b, length(species_s2), is.vector(species_s2), is.matrix(species_s2), is.data.frame(species_s2))
                    #cat("\n LS1", dim(species_s1)[1], "LS2", dim(species_s2)[1], "\n")
                    
                    
                    if(leng_test(species_s1) > 0){
                      if(leng_test(species_s2) > 0){
                        #JACCARD AND SORENSEN ESTIMATORS                          
                        shared <- intersect(ONESP(species_s1), ONESP(species_s2))
                        
                        #cat("FIRST SHARED \n")
                        #print(shared)
                        
                        if(length(shared) >= 1){
                          shared <- cbind(shared, matrix("0", nrow = length(shared)), matrix("0", nrow = length(shared)))
                          colnames(shared) <- c("spec", "abun1", "abun2")
                          
                          #cat("FIRST SHARED")
                          #print(shared)
                          #print(dim(shared)[1])
                          
                          for(sp in 1:dim(shared)[1]){
                            shared[sp, 2] <- if(is.vector(species_s1)){ species_s1[2]}else{species_s1[ONESP(species_s1) == shared[sp], ][2]} 
                            shared[sp, 3] <- if(is.vector(species_s2)){ species_s2[2]}else{species_s2[ONESP(species_s2) == shared[sp], ][2]} 
                          }
                          
                          #cat("SECOND SHARED")
                          #print(shared)
                          #print(dim(shared)[1])
                          
                          temp_shar <- NULL
                          temp_shar <- cbind(as.numeric(shared[, 2]), as.numeric(shared[, 3]))
                          
                          #cat("\nTEMPORARY SHARED\n")
                          #print(temp_shar)
                          
                          if(!is.null(shared)){
                            n <- if(is.vector(species_s1)){as.numeric(species_s1[2])}else{sum(as.numeric(species_s1[, 2]))}
                            m <- if(is.vector(species_s2)){as.numeric(species_s2[2])}else{sum(as.numeric(species_s2[, 2]))}
                            
                            u_pt1 <- sum(temp_shar[, 1]/n)
                            v_pt1 <- sum(temp_shar[, 2]/m)
                            #print(temp_shar)
                            #cat("u_pt1", u_pt1, "\n")
                            #cat("v_pt1", v_pt1, "\n")
                            
                            sum_u <- 0
                            sum_v <- 0
                            f1m <- 0
                            fm1 <- 0
                            f2m <- 0
                            fm2 <- 0
                            
                            #cat("DIM", dim(temp_shar), "\n")
                            
                            for(i in 1:dim(temp_shar)[1]){
                              if(temp_shar[i, 2] == 1){ #species_s1
                                sum_u <- sum_u + temp_shar[i, 1]/n
                                if(temp_shar[i, 1] >= 1) fm1 <- fm1 + 1
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
                            u_pt2 <- fm1*sum_u/(2*fm2)
                            v_pt2 <- f1m*sum_v/(2*f2m)
                            
                            #cat("sum_u", sum_u, "\n")
                            #cat("fm1", fm1, "\n")
                            #cat("sum_v", sum_v, "\n")
                            #cat("f1m", f1m, "\n")
                            #cat("fm2", fm2, "\n")
                            #cat("f2m", f2m, "\n")
                            #cat("u_pt2", u_pt2, "\n")
                            #cat("v_pt2", v_pt2, "\n")
                            
                            U <- u_pt1 + (m - 1)*u_pt2/m
                            V <- v_pt1 + (n - 1)*v_pt2/n
                            
                            if(U > 1) U <- 1
                            if(V > 1) V <- 1
                            
                            #cat("U", U, "\n")
                            #cat("V", V, "\n")
                            
                            VAREST[b, 1] <- U*V/(U + V - U*V)
                            VAREST[b, 2] <- 2*U*V/(U + V)
                          }                   
                        }else{
                          b <- b - 1
                        }                  
                      }else{
                        VAREST[b, 1] <- 0
                        VAREST[b, 2] <- 0
                      }  
                    }           
                  }
                  
                  VarJ <- var(VAREST[, 1])              
                  VarL <- var(VAREST[, 2])
                  MeanJ <- mean(VAREST[, 1])
                  MeanL <- mean(VAREST[, 2])
                  VARJ <- rbind(VARJ, VAREST[, 1])
                  VARL <- rbind(VARL, VAREST[, 2])
                  temp_resul <- cbind(p, s1, s2, Jabd, Labd, MeanJ, VarJ, MeanL, VarL)
                  
                }else{
                  VarJ <- "NA"
                  VarL <- "NA"
                  MeanJ <- "NA"
                  MeanL <- "NA"
                  temp_resul <- cbind(p, s1, s2, Jabd, Labd, MeanJ, VarJ, MeanL, VarL)
                }
                
              }else{
                Jabd <- 0
                Labd <- 0
                VarJ <- 0
                VarL <- 0
                MeanJ <- 0
                MeanL <- 0
                temp_resul <- cbind(p, s1, s2, Jabd, Labd, MeanJ, VarJ, MeanL, VarL)
              }
            }else{
              Jabd <- 0
              Labd <- 0
              VarJ <- 0
              VarL <- 0
              MeanJ <- 0
              MeanL <- 0
              temp_resul <- cbind(p, s1, s2, Jabd, Labd, MeanJ, VarJ, MeanL, VarL)
            }
          
          }else{
            Jabd <- 0
            Labd <- 0
            VarJ <- 0
            VarL <- 0
            MeanJ <- 0
            MeanL <- 0
            temp_resul <- cbind(p, s1, s2, Jabd, Labd, MeanJ, VarJ, MeanL, VarL)
          }
        }else{
          Jabd <- 0
          Labd <- 0
          VarJ <- 0
          VarL <- 0
          MeanJ <- 0
          MeanL <- 0
          temp_resul <- cbind(p, s1, s2, Jabd, Labd, MeanJ, VarJ, MeanL, VarL)
        }
        results <- rbind(results, temp_resul)  
        #print(temp_resul)
      }
    }
  }
  if(B > 0){
    write.table(VARJ, file = paste(file, "J.csv", sep = ""), row.names = FALSE)
    write.table(VARL, file = paste(file, "S.csv", sep = ""), row.names = FALSE)
  }
  write.table(results, file = paste(file, "R.csv", sep = ""), row.names = FALSE)
  results
}
