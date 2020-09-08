# Scenario 3
# Difference in the weights of main criteria
# 


# 0. Load packages and data

library(igraph)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(MCMCpack)

source('er_approach.r')
source('bel_to_dec_tran.r')
source('belief_generator.r')
source('node_colour.r')

test <- fread('./out.moreno_oz_oz', header = FALSE)
setnames(test, colnames(test),c('source','target','weight'))

# 0.1 Overview of the network
table(test$weight) # [1]-38, [2]-110, [3]-1624, [4]-602, [5]-298
test$weight <- test$weight / max(test$weight)
test[test$weight == 1,'weight'] <- 0.99

test_sna <- graph.data.frame(d = test, directed = T)
N_nodes <- vcount(test_sna) # Number of nodes in a social network

# 1. Initialisation (Fixed parameters)

# 1.1 Reference matrix for transforming aggregated MCDA results to Vaccination decisions
# For simplicity, the transformation matrix is assumed as follows.
#       Accept    Reject     Wait
# H1       0        1         0
# H2      1/2      1/2        0
# H3       1        0         0
# Theta    0        0         1
refmatrix <- matrix(c(0,1,0,1/2,1/2,0,1,0,0,0,0,1),
                    nrow = 4,ncol = 3,byrow = TRUE)

# 1.2 Fix the initial acceptance level in the network
ini_vac <- 0.45

# 2. Simulation runs (100)

iteration <- 10

dec_table_simul <- c()
vac_summary_simul <- c()

for (simul in 1:100){
  
  w_i_whole <- rbind(t(rdirichlet(N_nodes, rep(1,2))), t(rdirichlet(N_nodes, rep(1,3)))) 
  
  # 2.1 Initialise the parameters involved in the model
  # 2.1.1 Weights (for sub-criterion and main-criterion)
  
  scenario_s1 <- matrix(0, nrow = 2, ncol = N_nodes) %>% as.data.frame()
  scenario_s1[1,] <- runif(N_nodes, min = 0.6, max = 1)
  scenario_s1[2,] <- 1 - scenario_s1[1,]
  
  scenario_s2 <- matrix(0, nrow = 2, ncol = N_nodes) %>% as.data.frame()
  scenario_s2[1,] <- runif(N_nodes, min = 0.8, max = 1)
  scenario_s2[2,] <- 1 - scenario_s2[1,]
  
  scenario_s3 <- matrix(0, nrow = 2, ncol = N_nodes) %>% as.data.frame()
  scenario_s3[1,] <- scenario_s1[2,]
  scenario_s3[2,] <- scenario_s1[1,]
  
  scenario_s4 <- matrix(0, nrow = 2, ncol = N_nodes) %>% as.data.frame()
  scenario_s4[1,] <- scenario_s2[2,]
  scenario_s4[2,] <- scenario_s2[1,]
  
  scenario_s5 <- matrix(0.5, nrow = 2, ncol = N_nodes) %>% as.data.frame()
  
  s_W_whole <- matrix(0, nrow = 10, ncol = N_nodes + 1) %>% as.data.frame()
  s_W_whole[,2:(N_nodes+1)] <- rbind(scenario_s1,scenario_s2,scenario_s3,scenario_s4,scenario_s5)
  names(s_W_whole)[1] <- 'scenario'
  s_W_whole$scenario <- rep(1:5, each = 2)
  
  # 2.1.2 Initialise the unique belief bounds for each individual in the network
  bel_bound <- runif(N_nodes, min = 0.5, max = 1)
  
  # 2.2 Tables for storing all the results
  s3_record <- data.frame(scenario = rep(1:5,each = 4),
                          info = rep(1:4,5),
                          time = rep(1:4,5), 
                          sourcenode = rep(sample(1:N_nodes,4,replace = FALSE),5),
                          posneg = rep(1,20),
                          original = rep(1,20)) # True --> put back
  s3_record$from <- s3_record$sourcenode
  
  dec_s3_table <- c()
  vac_s3_summary <- c()
  
  for (sce in 1:5) {
    
    Wi_whole <- s_W_whole[s_W_whole$scenario == sce,2:(1 + N_nodes)]
    
    # 2.1.3 Initialise the belief distribution
    ini_assess <- belief_generator(N_nodes, w_i_whole, Wi_whole, bel_bound, refmatrix, ini_vac)
    ini_status <- matrix(0, nrow = N_nodes, ncol = 3)
    
    for (si in 1:N_nodes) {
      bel_i <- ini_assess[(5*si-4):(5*si),]
      w_main <- Wi_whole[,si]
      w_sub <- w_i_whole[,si]
      ini_status[si,] <- bel_to_dec_tran(bel_i,refmatrix, w_sub, w_main)
    }
    
    b_belief <- ini_assess
    e_belief <- ini_assess
    
    vac_summary <- matrix(0, nrow = N_nodes, ncol = 6)
    vac_summary[,1:3] <- ini_status
    vac_summary[,4] <- sce
    vac_summary[,5] <- 0
    vac_summary[,6] <- simul
    
    # +3 means 'node','scenario', 'iteration'
    dec_table <- matrix(0, nrow = 1, ncol = N_nodes +3)  %>% as.data.frame()
    for (p in 1:N_nodes) {
      if (ini_status[p,1] >= bel_bound[p]){dec_table[1,p] <- 1}
      else {next()}
    }
    dec_table[1,N_nodes+1] <- sce
    dec_table[1,N_nodes+3] <- simul
    
    s_record <-  subset(s3_record, scenario == sce, select = c("info","time","sourcenode","posneg","original","from"))
    t_record <- s_record[,c('info','time','sourcenode','posneg','from')]
    names(t_record)[3] <- 'targetnode'
    
    for (i in 1:iteration) {
      
      # 1. start spreadings
      b_group <- subset(s_record, time == i, select = c('info','sourcenode','original','posneg','from'))  
      if (nrow(b_group) == 0){next()}
      else {
        for (t in 1:nrow(b_group)) {
          
          # 1.0 Update the belief distributions of originals
          if (b_group$original[t] == 1){
            early_ad <- b_group$sourcenode[t]
            early_info <- b_group$info[t]
            cri_index <- c((5*early_ad-4):(5*early_ad))[early_info]
            
            if (b_group$posneg[t] == 1){spr_new <- c(0,0,1)}
            else {spr_new <- c(1,0,0)}
            b_belief[cri_index,] <- er_approach(b_belief[cri_index,],spr_new,1,1)
            e_belief[cri_index,] <- b_belief[cri_index,]
          }
          
          # 1.1 who is the source
          b_source <- b_group$sourcenode[t]
          
          # 1.1.1 who is the 'very' source
          b_from <- b_group$from[t]
          # 1.2 which criterion is affected
          b_criterion <- b_group$info[t]
          
          # 1.3 who is the target
          e_target <- test[test$source == b_source,'target']
          
          # 1.4 Good news or bad news
          if (b_group$posneg[t] == 1){
            b_news <- c(0,0,1)
            b_posneg <- 1}
          else {
            b_news <- c(1,0,0)
            b_posneg <- 0}
          
          # 1.5 check whether there is any target for the source or not
          if (nrow(e_target) == 0){next()}
          else {
            
            for (tt in 1:nrow(e_target)) {
              
              tar.node <- e_target$target[tt] %>% as.numeric()
              
              # Rule 1: if I have received the same info before, I will not be affected anymore
              pre.tar <- subset(t_record, info == b_criterion & time == i-1, select = c('targetnode'))
              rule1 <- tar.node %in% pre.tar$targetnode
              
              # Rule 2: If A and B spreads the same information, C will only accept once
              now.tar <- subset(t_record, info == b_criterion & time == i, select = c('targetnode'))
              rule2 <- tar.node %in% now.tar$targetnode
              
              ## Rule 2: if A and B is inter-connected, A will not affect B if A obtains the influence from B in the beginning
              #tar.source <- test[test$target == tar.node, 'source']
              #pre.source <- subset(s_record, info == b_criterion & time == i-1, select = c('sourcenode'))
              
              #judge1 <- tar.node %in% pre.source$sourcenode   # is the target who spread the info in the previous round
              #judge2 <- tar.node %in% tar.source
              
              if (rule1 | rule2){next()}
              else {
                
                # 2.5 pick up the highest openness
                #openness <- test[test$source == b_source & test$target == tar.node,'weight'] %>% as.numeric()
                
                tar.source <- test[test$target == tar.node, 'source']
                b.cri.source <- b_group[b_group$info == b_criterion,'sourcenode']
                tar.source <- tar.source[tar.source$source %in% b.cri.source]
                
                openness <- test[test$source %in% tar.source$source & test$target == tar.node,'weight'] %>% max()
                
                #s_index <- c((5*b_source-4):(5*b_source))[b_criterion]
                t_index <- c((5*tar.node-4):(5*tar.node))[b_criterion]
                
                if (all(e_belief[t_index,] == b_belief[t_index,])){
                  e_belief[t_index,] <- er_approach(b_news,b_belief[t_index,],openness,1)
                } else {
                  e_belief[t_index,] <- er_approach(b_news,e_belief[t_index,],openness,1)
                }
                
                s_record <- rbind(s_record,c(b_criterion, i+1, tar.node, b_posneg,0,b_from))  # 'info','sourcenode','original','posneg','from'
                du.s.rec <- s_record[,c('info','sourcenode','posneg','from')] %>% duplicated() %>% which()
                if (length(du.s.rec) == 0){s_record <- s_record}
                else {s_record <- s_record[-du.s.rec,]}
                
                t_record <- rbind(t_record,c(b_criterion, i, tar.node,b_posneg,b_from))
                du.t.rec <- t_record[,c('info','targetnode','posneg','from')] %>% duplicated() %>% which()
                if (length(du.t.rec) == 0){t_record <- t_record}
                else {t_record <- t_record[-du.t.rec,]}
                
              }
            }
          }
        }
      }
      
      mid_status <- matrix(0, nrow = N_nodes, ncol = 6)
      for (s in 1:N_nodes) {
        bel_i <- e_belief[(5*s-4):(5*s),]
        w_main <- Wi_whole[,s]
        w_sub <- w_i_whole[,s]
        mid_status[s,1:3] <- bel_to_dec_tran(bel_i,refmatrix, w_sub, w_main)
      }
      mid_status[,4] <- sce
      mid_status[,5] <- i
      mid_status[,6] <- simul
      
      vac_summary <- rbind(vac_summary,mid_status)
      
      mid_dec <- matrix(0, nrow = 1, ncol = N_nodes + 3) %>% as.data.frame()
      mid_dec[,1:N_nodes] <- dec_table[i,1:N_nodes]
      for (st in which(mid_dec[1,1:N_nodes] != 1)) {
        if (mid_status[st,1] >= bel_bound[st]){mid_dec[st] <- 1}
      }
      mid_dec[1,N_nodes + 1] <- sce
      mid_dec[1,N_nodes + 2] <- i
      mid_dec[1,N_nodes + 3] <- simul
      
      dec_table <- rbind(dec_table, mid_dec)
      
      b_belief <- e_belief
      
      cat(paste0('iteration:',i,'\n'))
    }
    
    
    dec_s3_table <- rbind(dec_s3_table,dec_table) # decision | >= belief bound
    vac_s3_summary <- rbind(vac_s3_summary,vac_summary)  # status | accept, reject
    
    cat(paste0('scenario:',sce,'\n'))
  }
  
  dec_table_simul <- rbind(dec_table_simul, dec_s3_table)
  vac_summary_simul <- rbind(vac_summary_simul, vac_s3_summary)
  
  cat(paste0('simulation:',simul,'\n'))
  
}

saveRDS(vac_summary_simul,'s3_vac_status.rds')
saveRDS(dec_table_simul,'s3_estimated_vaxer.rds')

