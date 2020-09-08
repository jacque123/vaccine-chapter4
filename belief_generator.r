# Initial belief distribution generator
# Referred to the vaccination coverage in USA during 2017-2018, the overall acceptance for 
# adults over 18 is around 45.3%

# Note: er_approach.r needs to be called in advance

# Input
#   wgt_sub: 5*1 matrix | criterion weight for sub-criterion
#   wgt_main: 2*1 matrix | criterion weight for two main criteria
#   N_nodes: number of nodes in the network
#   ini_cov: initial vaccination coverage in the network
#   bel_bound: belief boundary for each individual

belief_generator <- function(N_nodes, wgt_sub, wgt_main, bel_bound, ref, ini_vac){
  
  # 1. Generate the initial belief for each individual
  bel_dist <- rdirichlet(5*N_nodes, rep(1,4))
  bel_dist <- bel_dist[,-4]

  # 2. Belief aggregation
  bel_tran <- matrix(0, nrow = N_nodes, ncol = 3)
  for (i in 1:N_nodes) {
    bel_i <- bel_dist[(5*i-4):(5*i),]
    w_main <- wgt_main[,i]
    w_sub <- wgt_sub[,i]
    bel_tran[i,] <- bel_to_dec_tran(bel_i,ref, w_sub, w_main)
  }
  
  ini_vac_r <- runif(1, min = ini_vac*0.8, max = ini_vac*1.2)
  
  #ini_vac_r <- 0.45
  if (sum(bel_tran[,1] >= bel_bound)/N_nodes >= ini_vac_r){
    return(bel_dist)
  } else {
    
    while (sum(bel_tran[,1] >= bel_bound)/N_nodes < ini_vac_r) {
      
      vac_list <- which(bel_tran[,1] >= bel_bound)
      not_vac_list <- seq(1, N_nodes)
      not_vac_list <- not_vac_list[!not_vac_list %in% vac_list]
      
      for (j in not_vac_list) {
        b_rep <- rdirichlet(5, rep(1,4))
        b_rep <- b_rep[,-4]
        bel_dist[(5*j-4):(5*j),] <- b_rep
      }
      
      bel_tran <- matrix(0, nrow = N_nodes, ncol = 3)
      for (i in 1:N_nodes) {
        bel_i <- bel_dist[(5*i-4):(5*i),]
        w_main <- wgt_main[,i]
        w_sub <- wgt_sub[,i]
        bel_tran[i,] <- bel_to_dec_tran(bel_i,ref, w_sub, w_main)
      }
    }
    return(bel_dist)
  }
}
