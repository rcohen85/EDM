# Calculate CCM btwn Clicks & all other vars across bin sizes (& lags?):
library(rEDM)
library(tictoc)
library(parallel)

bin_list = c("1 hour","3 hour","6 hour","12 hour","1 day","2 day","3 day")
c = c(3,4,6:10) # columns of vars you want predict using library var
c = c(3,4)

par_ccm_explo = function(x, site, bin_list, c, tp) {
  tic('Run time:')
  load(paste("Bin_Explo_Output_", site, ".Rdata", sep = "")) # Load file containing best E info
  ccm_means_multibin = list()
  num_cores = detectCores() - 1 #avoid using all cores, or machine will come to a standstill
  clus = makeCluster(num_cores) # create clusters
  
  for (i in 5:length(bin_list)) {
    best_E = Simplex_out[[1]][i, 4] # Best E value for library var (clicks)
    lib = c(1, nrow(x[[i]]))
    pred = lib
    
    ts_mat = data.frame(matrix(NA, (nrow(x[[i]]) * 2), length(c)))
    colnames(ts_mat) = colnames(x[[i]][, c])
    for (j in 1:length(c)) {
      ts_vec = unname(c(unlist(x[[i]][2]), unlist(x[[i]][j])))
      ts_mat[, j] = ts_vec
    }
    
    run_ccm = function(q) {
      ts = cbind(q[1:(NROW(q) / 2)], q[((NROW(q) / 2) + 1):NROW(q)])
      out = ccm(
        block = ts,
        lib = lib,
        pred = pred,
        target_column = 2,
        lib_column = 1,
        E = best_E,
        lib_sizes = c(seq(10, nrow(ts), by = 50), nrow(ts)),
        num_samples = 100
      )
      out$rho[which(out$rho < 0)] = as.numeric("NA") # remove meaningless negative rho values
      ccm_means = aggregate(
        out,
        by = list(out$lib_size),
        FUN = mean,
        na.rm = TRUE
      )
    }
    
    # ccm_out2 = list()  #Code to test speed of running serial CCM without parallelization
    # tic()
    # for (i in 1:ncol(ts_mat)){
    #   cm = run_ccm(ts_mat[,i])
    #   ccm_out2[[i]] = cm
    # }
    # toc()
    
    # Run CCM on different var combinations in parallel:
    clusterExport(clus, c('ts_mat', 'lib', 'pred', 'best_E', 'run_ccm')) # export vars/funs to clusters
    clusterEvalQ(clus, expr = c(library(rEDM), library(tictoc), library(parallel))) # equip clusters w packages
    print('Running CCM with TP = 0...')
    tic()
    ccm_means_out = parApply(cl = clus, X = ts_mat, 2, FUN = run_ccm) # run CCM in parallel
    toc()
    
    ccm_means_multibin[[i]] = ccm_means_out
    
    save(ccm_means_multibin,
         file = paste('CCM_Output_TP0_', site, sep = ""))
    
    #
    #   for (k in 1:length(c)){
    #   print('Plotting rho per library size...')
    #   par(mfrow = c(1, 1))
    #   plot(
    #     ccm_means$lib_size,
    #     ccm_means$rho,
    #     type = "l",
    #     main = paste(
    #       "Clicks xmap",
    #       colnames(ts[2]),
    #       "\nSite",
    #       site,
    #       bin_list[i],
    #       "Bin",
    #       sep = " "
    #     )
    #   )
    #   }
    #
    #
    #   if (!missing(tp)) {
    #     run_ccm_tp = function(q) {
    #       ts = cbind(q[1:(NROW(q) / 2)], q[((NROW(q) / 2) + 1):NROW(q)])
    #       out = ccm(
    #         block = ts,
    #         lib = lib,
    #         pred = pred,
    #         target_column = 2,
    #         lib_column = 1,
    #         E = best_E,
    #         tp = tp,
    #         lib_sizes = c(seq(10, nrow(ts), by = 50), nrow(ts)),
    #         num_samples = 100
    #       )
    #     }
    # }
  }
  
  stopCluster(clus) # stop clusters
  
}

  
  
  
  
  
  
  # for (i in 2:length(bin_list)) { # Index over each bin-size dataframe
  #   for (j in c(3,4,6:10)) { # Index over desired columns of dataframe containing all TSs
  #     ts = cbind(x[[i]][2],x[[i]][j])
  #     best_E = Simplex_out[[1]][i,4] # Best E value for library var (clicks)
  #     lib = c(1, nrow(x[[i]][j]))
  #     pred = lib
  #     
  #     
  #     # Run regular CCM:
  #     print('Running regular CCM...')
  #     tic()
  #     ccm_out = ccm(ts,lib,pred,target_column = 2,lib_column = 1,E = best_E,
  #                   lib_sizes=c(seq(10, nrow(ts), by = 50),nrow(ts)),num_samples=10)
  #     
  #     ccm_out2$rho[which(ccm_out$rho<0)] = as.numeric("NA")
  #     
  #     # Calculate mean rho per lib size:
  #     ccm_means = aggregate(ccm_out,by=list(ccm_out$lib_size),FUN=mean,na.rm=TRUE)
  #     toc()
  #     
  #     # Plot mean rho per lib size:
  #     print('Plotting rho per library size...')
  #     par(mfrow=c(1,1))
  #     plot(ccm_means$lib_size,ccm_means$rho,type="l",
  #          main=paste("Clicks xmap",colnames(ts[2]), "\nSite", site,bin_list[i],"Bin", sep=" "))
  #     
  #     # # Run CCM on longest contiguous stretch of data:
  #     #   print('Running CCM on longest contiguous stretch of data...')
  #     #   tic()
  #     #   if (j!=10){
  #     #   # ind_a = seq(attributes(na.contiguous(ts[,1]))$tsp[1],attributes(na.contiguous(ts[,1]))$tsp[2],by=1)
  #     #   # ind_b = seq(attributes(na.contiguous(ts[,2]))$tsp[1],attributes(na.contiguous(ts[,2]))$tsp[2],by=1)
  #     #   # ind = intersect(ind_a,ind_b)
  #     #     ind = seq(attributes(na.contiguous(ts[,1]))$tsp[1],attributes(na.contiguous(ts[,1]))$tsp[2],by=1)
  #     #   } else {ind = seq(attributes(na.contiguous(ts[,1]))$tsp[1],attributes(na.contiguous(ts[,1]))$tsp[2],by=1)}
  #     # 
  #     #   ccm_out_contig_ts = ccm(ts[ind,],lib,pred,target_column = 2,lib_column = 1,E = best_E,
  #     #                 lib_sizes=c(seq(10, length(ind), by = 50),length(ind)),num_samples=10)
  #     #   ccm_out_contig_ts$rho[which(ccm_out_contig_ts$rho<0)] = as.numeric("NA")
  #     # 
  #     #   # Calculate mean rho per lib size for contiguous TS:
  #     #   ccm_means_contig_ts = aggregate(ccm_out_contig_ts,by=list(ccm_out_contig_ts$lib_size),FUN=mean,na.rm=TRUE)
  #     #   toc()
  #     # 
  #     #   # Plot mean rho per lib size for contiguous TS:
  #     #   print('Plotting rho per library size for longest contiguous stretch of data...')
  #     #   par(mfrow=c(1,1))
  #     #   plot(ccm_means_contig_ts$lib_size,ccm_means_contig_ts$rho,type="l",
  #     #        main=paste("Clicks xmap",colnames(ts[2]), "\nSite", site, bin_list[i],"Bin Contiguous TS", sep=" "))
  #     
  #     
  #     # Run CCM with tp != 1:
  #     if (!missing(tp)){
  #       print(paste('Running CCM with TP = ',tp,sep = ""))
  #       tic()
  #       #for (k in 1:length(tp)){
  #       ccm_out = ccm(ts,lib,pred,target_column = 2,lib_column = 1,E = best_E,tp=tp,
  #                     lib_sizes=c(seq(10, nrow(ts), by = 50),nrow(ts)),num_samples=10)
  #       ccm_out$rho[which(ccm_out$rho<0)] = as.numeric("NA")
  #       
  #       # Calculate mean rho per lib size:
  #       ccm_means = aggregate(ccm_out,by=list(ccm_out$lib_size),FUN=mean,na.rm=TRUE)
  #       toc()
  #       
  #       # Plot mean rho per lib size:
  #       print(paste('Plotting rho per library size for TP = ',tp,sep = ""))
  #       par(mfrow=c(1,1))
  #       plot(ccm_means$lib_size,ccm_means$rho,type="l",
  #            main=paste("Clicks xmap",colnames(ts[2]), tp, "\nSite", site, bin_list[i],"Bin", sep=" "))
  #       # }
  #     }
  #     
  #     # # Create surrogate data:
  #     # surr_dat = make_surrogate_data(unlist(x[[i]][j]), num_surr = 100, method =
  #     #                                  "ebisuzaki")
  #     # x_surr = cbind(x[[i]][2], surr_dat)
  #     # 
  #     # # Run CCM on surrogate data:
  #     # ccm_surr = ccm(x_surr,lib,pred,target_column = j,lib_column = 2,E = best_E)
  #     # 
  #     # # Calculate p-values:
  #     # ccm_sig =
  #     #   
  #     # # Remove non-significant results:
  #     # ccm_mat[ccm_mat < 0] = 0
  #     # ccm_mat[ccm_sig > 0.05] = 0
  #     
  #     
  #   }
  #   
  # }
  # toc() 

