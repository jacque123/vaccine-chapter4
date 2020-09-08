# Aggregate the belief distribution from bottom to top
# Transform the belief distribution to accept/reject decision
# Note: er_approach.r needs to called in advance
# Input
#   bdist: belief distribution 5*3
#   ref
#   wgt_sub: 5*1 matrix | criterion weight for sub-criterion
#   wgt_main: 2*1 matrix | criterion weight for two main criteria
# Output
#


bel_to_dec_tran <- function(bdist, ref, wgt_sub, wgt_main){
  
  # Step1: Criterion C1 & C2 
  bel_c1 <- er_approach(bdist[1,], bdist[2,], wgt_sub[1], wgt_sub[2])
  
  bel_temp <- er_approach(bdist[3,], bdist[4,], wgt_sub[3], wgt_sub[4])
  bel_c2 <- er_approach(bel_temp,bdist[5,], wgt_sub[3]+wgt_sub[4], wgt_sub[5])
  
  # Step2: Top-level
  bel_int <- er_approach(bel_c1, bel_c2, wgt_main[1], wgt_main[2])
  
  # Step3: Transform belief distributions to {accept, reject, wait}
  
  bel_int[4] <- 1 - sum(bel_int[1:3])
  
  dec_mass <- bel_int %*% ref
  
  return(dec_mass)
  
}




