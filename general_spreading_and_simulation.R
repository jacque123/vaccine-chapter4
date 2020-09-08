# Revised integrated model (social influence +  MCDA)
# Small social network: 'out.moreno_oz_oz'
#     Nodes: 217; Edges: 2672
# Social influence is reflected on the changes of belief assessments for the criterion at the bottom-level
#
#
#
#
# 0. Loading packages and data

setwd('/Volumes/Xongkoro/EJOR-Aug 2020/Codes and Data/')

library(igraph)
library(dplyr)
library(data.table)
library(RColorBrewer)
library(MCMCpack)
library(ggplot2)

source('er_approach.r')
source('bel_to_dec_tran.r')
source('belief_generator.r')
source('node_colour.r')


test <- fread('./out.moreno_oz_oz', header = FALSE)
setnames(test, colnames(test),c('source','target','weight'))

# 0.1 Overview of the network
wgt.freq <- table(test$weight) %>% as.data.frame()  # [1]-38, [2]-110, [3]-1624, [4]-602, [5]-298
g.wgt <- ggplot(aes(x = Var1, y = Freq), data = wgt.freq) + 
  geom_bar(aes(fill = Var1),stat = 'identity',color = 'black') +
  geom_text(aes(label= Freq), vjust= -1, size = 5) + 
  scale_fill_manual(values = c("#FEE5D9","#FCAE91","#FB6A4A","#DE2D26","#A50F15")) +
  ylim(0,2000) +
  xlab('Friendship Level') + ylab('Frequency') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'))
ggsave('friendship distribution.png', g.wgt, dpi = 600, height = 4, width = 6)

test$weight <- test$weight / max(test$weight)
test[test$weight == 1,'weight'] <- 0.99

test_sna <- graph.data.frame(d = test, directed = T)
N_nodes <- vcount(test_sna) # Number of nodes in a social network

V(test_sna)$name[!V(test_sna)$name %in% as.character(test$source)]  # '189' not on source list
V(test_sna)$name[!V(test_sna)$name %in% as.character(test$target)]  # '191' and '197' not on target list

# 1. Initialisation (Randomisation)

# For simplicity, the transformation matrix is assumed as follows.
#       Accept    Reject     Wait
# H1       0        1         0
# H2      1/2      1/2        0
# H3       1        0         0
# Theta    0        0         1
refmatrix <- matrix(c(0,1,0,1/2,1/2,0,1,0,0,0,0,1),
                    nrow = 4,ncol = 3,byrow = TRUE)

# 1.1  Initial belief distributions

w_i_whole <- rbind(t(rdirichlet(N_nodes, rep(1,2))), t(rdirichlet(N_nodes, rep(1,3)))) 
Wi_whole <- t(rdirichlet(N_nodes, rep(1,2)))

# https://www.cdc.gov/flu/fluvaxview/coverage-1819estimates.htm
# For adults over 18, the (mean) vaccination coverage is around 45.3% (33.9%–56.3% | +- 25%)

# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/895233/Surveillance_Influenza_and_other_respiratory_viruses_in_the_UK_2019_to_2020_FINAL.pdf
# Table 5: 
ini_vac <- 0.45
bel_bound <- runif(N_nodes, min = 0.5, max = 1)

ini_assess <- belief_generator(N_nodes, w_i_whole, Wi_whole, bel_bound, refmatrix, ini_vac)
ini_status <- matrix(0, nrow = N_nodes, ncol = 3)

for (i in 1:N_nodes) {
  bel_i <- ini_assess[(5*i-4):(5*i),]
  w_main <- Wi_whole[,i]
  w_sub <- w_i_whole[,i]
  ini_status[i,] <- bel_to_dec_tran(bel_i,refmatrix, w_sub, w_main)
}

# 1.2  Visualise the initial status

nodelist <- V(test_sna)$name
outdegree <- degree(test_sna, mode = c('out'))

V(test_sna)$color <- heat.colors(10)[6]
V(test_sna)$label <- V(test_sna)$name
V(test_sna)$label.color <- rgb(0, 0, .2, .8)
V(test_sna)$label.cex <- outdegree/max(outdegree)*4
V(test_sna)$size <- mean(outdegree)*1.1*outdegree/max(outdegree)

# 1.2.1 Visualise the network structure
png('network_structure.png',width = 2000,height = 2000)
plot(test_sna,layout=layout.kamada.kawai,
     vertex.label=V(test_sna)$label,
     vertex.label.color=V(test_sna)$label.color,
     vertex.label.cex = V(test_sna)$label.cex,
     vertex.color=V(test_sna)$color,
     vertex.size=V(test_sna)$size,
     edge.width=E(test_sna)$width,
     edge.color=grey(0.5),edge.arrow.size=0.3)
dev.off()

set.seed(31378)
# 1.2.2 Visualise the initial belief distributions
V(test_sna)$color <- node_colour(N_nodes, ini_status, nodelist)
png('initial_estimated_vaccination_attitudes.png',width = 2000,height = 2000)
plot(test_sna, 
     #layout = layout_with_graphopt,
     layout=layout.kamada.kawai,
     vertex.label=V(test_sna)$label,
     vertex.label.color=V(test_sna)$label.color,
     vertex.label.cex = V(test_sna)$label.cex,
     vertex.color=V(test_sna)$color,
     vertex.size=V(test_sna)$size,
     edge.width=E(test_sna)$width,
     edge.color=grey(0.5),edge.arrow.size=0.3)
legend(x = -1.05, y = -0.6,
       #x = 0.73,y = 1.02,
       legend = c('[1/3,1/2]','(1/2,2/3]','(2/3,  1 ]'),
       title = 'Belief degree (Accept)',fill=brewer.pal(9,'Greens')[c(3,5,7)],
       cex = 2.5)
legend(x =-1.05, y = -0.83,
       #x = 0.73,y = 0.78,
       legend = c('[1/3,1/2]','(1/2,2/3]','(2/3,  1 ]'),
       title = 'Belief degree (Reject) ',fill=brewer.pal(9,'Reds')[c(3,5,7)],
       cex = 2.5)
legend(x = -0.65, y = -0.83,
       #x = 0.73,y = 0.54,
       legend = c('[1/3,1/2]','(1/2,2/3]','(2/3,  1 ]'),
       title = 'Belief degree (Wait)',fill=heat.colors(10)[c(10,8,6)],
       cex = 2.5)
dev.off()

# 1.3 Verdict who can take actions at this stage 
# Assume each individual has a lower bound of uncertainty to certainty

V(test_sna)$color <- rgb(1,1,1)   # colour of white
for (i in 1:N_nodes){
  k <- nodelist[i] %>% as.numeric()
  if (ini_status[k,1] >= bel_bound[k]){V(test_sna)$color[i] <- 'dodgerblue'}  #253494 dark dark blue #004529 dark dark green
}

png(filename = 'initial estimated vaccination decisions.png',width = 2000,height = 2000)
plot(test_sna,layout=layout.kamada.kawai,
     vertex.label=V(test_sna)$label,
     vertex.label.color=V(test_sna)$label.color,
     vertex.color=V(test_sna)$color,
     vertex.size=V(test_sna)$size,
     edge.width=E(test_sna)$width,
     edge.color=grey(0.5),edge.arrow.size=0.3)
dev.off()

# 2. Information diffusion and belief updating

bel_summary <- ini_assess   # nrow = 5*217 = 1085, ncol = 3 
vac_summary <- ini_status

# 2.1 Pre-defined the stream of information diffusion
# t1: m1 (good)  C11 (0,0,1)
# t2: m2 (good)  C12 (0,0,1)
# t3: m3 (good)  C21 (0,0,1)
# t4: m4 (good)  C22 (0,0,1)
# t5: m5 (good)  C23 (0,0,1)

s_record <- data.frame(info = 1:5, time = 1:5, 
                       sourcenode = c(70,184,169,120,113),
                       posneg = c(1,0,1,1,0),
                       original = rep(1,5))
t_record <- s_record[,c('info','time','sourcenode','posneg')]
names(t_record)[3] <- 'targetnode'

# 2.2 Pre-set some data tables to store the information during iteration

b_belief <- ini_assess
e_belief <- ini_assess

dec_table <- matrix(0, nrow = 1, ncol = N_nodes)  %>% as.data.frame()
for (p in 1:N_nodes) {
  if (ini_status[p,1] >= bel_bound[p]){dec_table[1,p] <- 1}
  else {next()}
}

# 2.3 Belief updating

i <- 1
iteration <- 10 

for (i in 1:iteration) {
 
  # 1. start spreadings
  b_group <- subset(s_record, time == i, select = c('info','sourcenode','original','posneg'))  
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
            
            #if (all(e_belief[t_index,] == b_belief[t_index,])){
            #  e_belief[t_index,] <- er_approach(c(0,0,1),b_belief[t_index,],openness,1)
            #} else {
            #  e_belief[t_index,] <- er_approach(c(0,0,1),e_belief[t_index,],openness,1)
            #}
            
            s_record <- rbind(s_record,c(b_criterion, i+1, tar.node, b_posneg,0))  # info, time, sourcenode, posneg, original
            du.s.rec <- s_record[,c('info','sourcenode','posneg')] %>% duplicated() %>% which()
            if (length(du.s.rec) == 0){s_record <- s_record}
            else {s_record <- s_record[-du.s.rec,]}
            
            t_record <- rbind(t_record,c(b_criterion, i, tar.node,b_posneg))
            du.t.rec <- t_record[,c('info','targetnode','posneg')] %>% duplicated() %>% which()
            if (length(du.t.rec) == 0){t_record <- t_record}
            else {t_record <- t_record[-du.t.rec,]}
            
          }
        }
      }
    }
  }
  
   
  
  bel_summary <- rbind(bel_summary,e_belief)  # record the belief distributions for all nodes
  
  mid_status <- matrix(0, nrow = N_nodes, ncol = 3)
  for (s in 1:N_nodes) {
    bel_i <- e_belief[(5*s-4):(5*s),]
    w_main <- Wi_whole[,s]
    w_sub <- w_i_whole[,s]
    mid_status[s,] <- bel_to_dec_tran(bel_i,refmatrix, w_sub, w_main)
  }
  
  vac_summary <- rbind(vac_summary,mid_status)
  
  mid_dec <- dec_table[i,]
  
  for (st in which(mid_dec != 1)) {
    if (mid_status[st,1] >= bel_bound[st]){mid_dec[st] <- 1}
  }
  
  dec_table <- rbind(dec_table, mid_dec)
  
  b_belief <- e_belief
  
  # 3. Visualisations
  # 3.1 Vaccination attitudes -  three colours
  V(test_sna)$color <- node_colour(N_nodes, mid_status, nodelist)
  png(filename = paste('./T',i,'-estimated vaccination attitudes.png'), height = 2000, width = 2000)
  plot(test_sna,layout=layout.kamada.kawai,
       vertex.label=V(test_sna)$label,
       vertex.label.color=V(test_sna)$label.color,
       vertex.color=V(test_sna)$color,
       vertex.size=V(test_sna)$size,
       edge.width=E(test_sna)$width,
       edge.color=grey(0.5),edge.arrow.size=0.3)
  dev.off()
  
  # 3.2 Estimated decisions - yellow and white
  V(test_sna)$color <- rgb(1,1,1)
  for (ki in 1:N_nodes) {
    k <- nodelist[ki] %>% as.numeric()
    if(mid_dec[k] == 1){V(test_sna)$color[ki] <- 'dodgerblue'}
  }
  
  png(filename = paste('T',i,'-estimated vaccination decisions.png'), height = 2000, width = 2000)
  plot(test_sna,layout=layout.kamada.kawai,
       vertex.label=V(test_sna)$label,
       vertex.label.color=V(test_sna)$label.color,
       vertex.color=V(test_sna)$color,
       vertex.size=V(test_sna)$size,
       edge.width=E(test_sna)$width,
       edge.color=grey(0.5),edge.arrow.size=0.3)
  dev.off()
  
  cat(i,'\n')
}

saveRDS(vac_summary,'2508vac_status.rds')
saveRDS(bel_summary,'2508belief_updates_summary.rds')
saveRDS(dec_table,'2508estimated_vaxer.rds')




























