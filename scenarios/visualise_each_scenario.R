# Codes for visualising the results of each scenario

setwd('/Volumes/Xongkoro/EJOR-Aug 2020/Codes and Data/')

library(dplyr)
library(ggplot2)

N_nodes <- 217

s1_vaxed <- readRDS('./s1_estimated_vaxer.rds')
names(s1_vaxed)[218:220] <- c('scenario','iteration','simulation')
N_nodes <- 217

s2_vaxed <- readRDS('./s2_estimated_vaxer.rds')
names(s2_vaxed)[218:220] <- c('scenario','iteration','simulation')

s3_vaxed <- readRDS('./s3_estimated_vaxer.rds')
names(s3_vaxed)[218:220] <- c('scenario','iteration','simulation')

s4_vaxed <- readRDS('./s4_estimated_vaxer.rds')
names(s4_vaxed)[218:220] <- c('scenario','iteration','simulation')

##################################
# I. Data integration
# Note:The process below can be applied to all four scenarios.
#      Pay attention to the number of sub-scenarios within each scenarios
#      The dimension of the dataframe (s1/2/3/4_ne) needs to be updated correspondingly

s1_ne <- data.frame(simulation = rep(1:100, each = 44),
                   scenario = rep(rep(1:4, each = 11),100),
                   step = rep(0:10,400),
                   vaxed = 0)

for (ds in 1:nrow(s1_ne)) {
  dec.scen <- s1_ne$scenario[ds]
  dec.simu <- s1_ne$simulation[ds]
  dec.step <- s1_ne$step[ds]
  
  id <- which(s1_vaxed$scenario == dec.scen & s1_vaxed$iteration == dec.step & s1_vaxed$simulation == dec.simu)
  if (length(id) == 0){next()}
  else {s1_ne$vaxed[ds] <- sum(s1_vaxed[id,1:N_nodes])}
}

for (dsd in 2:nrow(s1_ne)) {
  if (s1_ne$vaxed[dsd] != 0){next()}
  else {s1_ne$vaxed[dsd] <- s1_ne$vaxed[dsd-1]}
}

s1_ne$vaxed <- s1_ne$vaxed/N_nodes
s1_ne$scenario <- s1_ne$scenario %>% as.character()

s1_avg <- data.frame(scenario = rep(1:4, each = 11),
                     step = rep(0:10,4),
                     vaxed = 0)
for (i in 1:nrow(s1_avg)) {
  st <- s1_avg$step[i]
  sc <- s1_avg$scenario[i]
  
  s1_avg$vaxed[i] <- s1_ne[s1_ne$scenario == sc & s1_ne$step == st,'vaxed'] %>% mean()
}

s1_avg$scenario <- s1_avg$scenario %>% as.character()

###########################################
# II. Visualisation

# Scenario 1: the content of information

g.s1 <- ggplot(s1_avg) +
  geom_line(aes(x = as.factor(step), y = vaxed,color = scenario, 
                group = scenario), 
            #color = factor(simulation), group = simulation), 
            size = 1) +
  xlab('Time (step)') + ylab('Estimated vaccination coverage') +
  ylim(0.445,0.55) +
  scale_color_manual(name="Scenarios",
                     values = c('royalblue','green','gold','red'),
                     breaks=c("1", "2", "3","4"),
                     labels=c(expression(s[1.1]), expression(s[1.2]), expression(s[1.3]),expression(s[1.4]))) +
  geom_point(aes(x = as.factor(step), y = vaxed, shape = scenario,color = scenario),
             size = 4) +
  scale_shape_manual(values = 15:18,
                     name="Scenarios",
                     breaks=c("1", "2", "3","4"),
                     labels=c(expression(s[1.1]), expression(s[1.2]), expression(s[1.3]),expression(s[1.4]))) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.background = element_rect(color = 'black'),
        legend.position=c(0.12,0.8)) + 
  annotate('segment', x = 2, xend = 2, y = 0.47, yend = 0.464,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('text', x = 1.8, y = 0.476,size = 5, label = 'positive',color = 'red') +
  annotate('text', x = 1.8, y = 0.472,size = 5, label = 'positive',color = 'gold') +
  annotate('segment', x = 3, xend = 3, y = 0.475, yend = 0.469,size = 1,arrow = arrow(length = unit(0.3, "cm"),type = 'closed')) +
  annotate('text', x = 3, y = 0.482,size = 5, label = 'positive',color = 'red') +
  annotate('text', x = 3.06, y = 0.478,size = 5, label = 'negative',color = 'gold') +
  annotate('segment', x = 2, xend = 2, y = 0.451, yend = 0.457,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('text', x = 1.7, y = 0.449,size = 5, label = 'negative',color = 'green') +
  annotate('text', x = 1.7, y = 0.445,size = 5, label = 'negative',color = 'royalblue') +
  annotate('segment', x = 3, xend = 3, y = 0.451, yend = 0.457,size = 1,arrow = arrow(length = unit(0.3, "cm"),type = 'closed')) +
  annotate('text', x = 3.3, y = 0.449,size = 5, label = 'positive',color = 'green') +
  annotate('text', x = 3.37, y = 0.445,size = 5, label = 'negative',color = 'royalblue') 
  
ggsave('./s1_avg_plot.png', g.s1, dpi = 600, height = 6, width = 8)


# Scenario 2: the increasing amount of information

g.s2 <- ggplot(s2_avg) +
  geom_line(aes(x = as.factor(step), y = vaxed,color = scenario, 
                group = scenario), 
            #color = factor(simulation), group = simulation), 
            size = 1) +
  xlab('Time (step)') + ylab('Estimated vaccination coverage') +
  ylim(0.4,1) + 
  scale_color_manual(name="Scenarios",
                     values = c('purple','royalblue','green','gold','red'),
                     breaks=c('1',"2","3","4",'5'),
                     labels=c(expression(s[2.1]), expression(s[2.2]), expression(s[2.3]),expression(s[2.4]),expression(s[2.5]))) + 
  geom_point(aes(x = as.factor(step), y = vaxed, shape = scenario,color = scenario),
             size = 4) +
  scale_shape_manual(values = 15:19,
                     name="Scenarios",
                     breaks=c('1',"2","3","4",'5'),
                     labels=c(expression(s[2.1]), expression(s[2.2]), expression(s[2.3]),expression(s[2.4]),expression(s[2.5]))) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.background = element_rect(color = 'black'),
        legend.position=c(0.12,0.8)) +  
  annotate('segment', x = 2, xend = 2, y = 0.52, yend = 0.47,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('text', x = 1.7, y = 0.64,size = 5, label = 'positive',color = 'red') +
  annotate('text', x = 1.7, y = 0.615,size = 5, label = 'positive',color = 'gold') +
  annotate('text', x = 1.7, y = 0.59,size = 5, label = 'positive',color = 'green') +
  annotate('text', x = 1.7, y = 0.565,size = 5, label = 'positive',color = 'royalblue') +
  annotate('text', x = 1.7, y = 0.54,size = 5, label = 'positive',color = 'purple') +
  annotate('segment', x = 3, xend = 3, y = 0.545, yend = 0.475,size = 1,arrow = arrow(length = unit(0.3, "cm"),type = 'closed')) +
  annotate('text', x = 2.9, y = 0.65,size = 5, label = 'positive',color = 'red') +
  annotate('text', x = 2.9, y = 0.625,size = 5, label = 'positive',color = 'gold') +
  annotate('text', x = 2.9, y = 0.60,size = 5, label = 'positive',color = 'green') +
  annotate('text', x = 2.9, y = 0.575,size = 5, label = 'positive',color = 'royalblue') +
  annotate('segment', x = 4, xend = 4, y = 0.58, yend = 0.51,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('text', x = 4, y = 0.66,size = 5, label = 'positive',color = 'red') +
  annotate('text', x = 4, y = 0.635,size = 5, label = 'positive',color = 'gold') +
  annotate('text', x = 4, y = 0.61,size = 5, label = 'positive',color = 'green') +
  annotate('segment', x = 5, xend = 5, y = 0.63, yend = 0.57,size = 1,arrow = arrow(length = unit(0.3, "cm"),type = 'closed')) +
  annotate('text', x = 5.1, y = 0.68,size = 5, label = 'positive',color = 'red') +
  annotate('text', x = 5.1, y = 0.655,size = 5, label = 'positive',color = 'gold') +
  annotate('segment', x = 6, xend = 6, y = 0.72, yend = 0.66,size = 1,arrow = arrow(length = unit(0.3, "cm"),type = 'closed')) +
  annotate('text', x = 6.2, y = 0.74,size = 5, label = 'positive',color = 'red') 
  
ggsave('./s2_avg_plot.png', g.s2, dpi = 600, height = 6, width = 8)  

# Scenario 3: the weights of main criteria

g.s3 <- ggplot(s3_avg) +
  geom_line(aes(x = as.factor(step), y = vaxed,color = scenario, 
                group = scenario), 
            #color = factor(simulation), group = simulation), 
            size = 1) +
  xlab('Time (step)') + ylab('Estimated vaccination coverage') +
  ylim(0.4,1) + 
  scale_color_manual(name="Scenarios",
                     values = c('purple','royalblue','green','gold','red'),
                     breaks=c('1',"2","3","4",'5'),
                     labels=c(expression(s[3.1]), expression(s[3.2]), expression(s[3.3]),expression(s[3.4]),expression(s[3.5]))) + 
  geom_point(aes(x = as.factor(step), y = vaxed, shape = scenario,color = scenario),
             size = 4) +
  scale_shape_manual(values = 15:19,
                     name="Scenarios",
                     breaks=c('1',"2","3","4",'5'),
                     labels=c(expression(s[3.1]), expression(s[3.2]), expression(s[3.3]),expression(s[3.4]),expression(s[3.5]))) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.background = element_rect(color = 'black'),
        legend.position=c(0.12,0.8)) +
  annotate('text', x = 3.3, y =0.775, label = 'positive information regarding', size = 5) +
  # annotate('text', x = 1.665, y =0.75, label = 'information', size = 5) +
  # annotate('text', x = 1.585, y =0.725, label = 'regarding', size = 5) +
  annotate('segment', x = 2, xend = 2, y = 0.727, yend = 0.5,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('segment', x = 3, xend = 3, y = 0.727, yend = 0.55,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('segment', x = 4, xend = 4, y = 0.727, yend = 0.6,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('segment', x = 5, xend = 5, y = 0.727, yend = 0.7,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('text', x = 1.9, y = 0.745,size = 5, label = expression(C[list(1,1)])) +
  annotate('text', x = 2.9, y = 0.745,size = 5, label = expression(C[list(1,2)])) +
  annotate('text', x = 3.9, y = 0.745,size = 5, label = expression(C[list(2,1)])) +
  annotate('text', x = 4.9, y = 0.745,size = 5, label = expression(C[list(2,2)])) 

ggsave('./s3_avg_plot.png', g.s3, dpi = 600, height = 6, width = 8)  

# Scenario 4: the weights of sub-criteria

g.s4 <- ggplot(s4_avg) +
  geom_line(aes(x = as.factor(step), y = vaxed,color = scenario, 
                group = scenario), 
            #color = factor(simulation), group = simulation), 
            size = 1) +
  xlab('Time (step)') + ylab('Estimated vaccination coverage') +
  ylim(0.45,0.8) + 
  scale_color_manual(name="Scenarios",
                     values = c('purple','royalblue','green','gold','red'),
                     breaks=c('1',"2","3","4",'5'),
                     labels=c(expression(s[4.1]), expression(s[4.2]), expression(s[4.3]),expression(s[4.4]),expression(s[4.5]))) + 
  geom_point(aes(x = as.factor(step), y = vaxed, shape = scenario,color = scenario),
             size = 4) +
  scale_shape_manual(values = 15:19,
                     name="Scenarios",
                     breaks=c('1',"2","3","4",'5'),
                     labels=c(expression(s[4.1]), expression(s[4.2]), expression(s[4.3]),expression(s[4.4]),expression(s[4.5]))) + 
  theme_classic() +
  theme(axis.text = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 15, face = 'bold'),
        legend.text = element_text(size = 15, face = 'bold'),
        legend.title = element_text(size = 15, face = 'bold'),
        legend.background = element_rect(color = 'black'),
        legend.position=c(0.12,0.8)) +
  annotate('text', x = 3.35, y =0.67, label = 'positive information regarding', size = 5) +
  # annotate('text', x = 1.665, y =0.75, label = 'information', size = 5) +
  # annotate('text', x = 1.585, y =0.725, label = 'regarding', size = 5) +
  annotate('segment', x = 2, xend = 2, y = 0.60, yend = 0.48,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('segment', x = 3, xend = 3, y = 0.60, yend = 0.50,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('segment', x = 4, xend = 4, y = 0.60, yend = 0.53,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('segment', x = 5, xend = 5, y = 0.60, yend = 0.585,size = 1,arrow = arrow(length = unit(0.3, "cm"), type = 'closed')) +
  annotate('text', x = 2.2, y = 0.645,size = 5, label = expression(C[list(1,1)])) +
  annotate('text', x = 3.05, y = 0.645,size = 5, label = expression(C[list(1,2)])) +
  annotate('text', x = 3.95, y = 0.645,size = 5, label = expression(C[list(2,1)])) +
  annotate('text', x = 4.9, y = 0.645,size = 5, label = expression(C[list(2,2)])) +
  annotate('text', x = 2.2, y = 0.615,size = 5, label = expression(C[list(2,1)])) +
  annotate('text', x = 3.05, y = 0.615,size = 5, label = expression(C[list(2,2)])) +
  annotate('text', x = 3.95, y = 0.615,size = 5, label = expression(C[list(2,1)])) +
  annotate('text', x = 4.9, y = 0.615,size = 5, label = expression(C[list(2,2)])) +
  annotate('text', x = 0.70, y = 0.645,size = 5, label = expression(s[4.1]), color = 'purple') +
  annotate('text', x = 1.15, y = 0.645,size = 5, label = expression(s[4.2]), color = 'royalblue') +
  annotate('text', x = 1.60, y = 0.645,size = 5, label = expression(s[4.3]), color = 'green') +
  annotate('rect', xmin = 0.5, ymin = 0.635, xmax = 5.3, ymax = 0.655, alpha = 0.1) +
  annotate('text', x = 0.70, y = 0.617,size = 5, label = expression(s[4.4]), color = 'gold') +
  annotate('text', x = 1.15, y = 0.617,size = 5, label = expression(s[4.5]), color = 'red') +
  annotate('rect', xmin = 0.5, ymin = 0.605, xmax = 5.3, ymax = 0.625, alpha = 0.1) 

ggsave('./s4_avg_plot.png', g.s4, dpi = 600, height = 6, width = 8)



