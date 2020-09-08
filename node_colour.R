# Node Colour Function
# Input 
#   N: number of individuals
#   ebd: belief assessments for each individual in the context of {y,n,ytd} (1*3)
#   idlist: index list 鉴于节点非按顺序排列，需要一个列表来索引其具体位置
# Output 
#   V(test_sna)$color: color setting for graph

node_colour <- function(N,ebd,idlist) {
  y_color <- brewer.pal(9,'Greens')
  n_color <- brewer.pal(9,'Reds')
  ytd_color <- heat.colors(10)
  #ytd_color <- brewer.pal(9,'Blues')
  for (k in 1:N) {
    t <- idlist[k] %>% as.numeric()
    if (max(ebd[t,]) == ebd[t,1]) {
      if (ebd[t,1] >2/3 & round(ebd[t,1],4) <=1) {V(test_sna)$color[k] = y_color[7]}
      if (ebd[t,1] >1/2 & ebd[t,1]<=2/3) {V(test_sna)$color[k] = y_color[5]}
      if (ebd[t,1]>=1/3 & ebd[t,1]<=1/2) {V(test_sna)$color[k] = y_color[3]}
    } else if (max(ebd[t,]) == ebd[t,2]) {
      if (ebd[t,2] >2/3 & round(ebd[t,2],4) <=1) {V(test_sna)$color[k] = n_color[7]}
      if (ebd[t,2] >1/2 & ebd[t,2]<=2/3) {V(test_sna)$color[k] = n_color[5]}
      if (ebd[t,2]>=1/3 & ebd[t,2]<=1/2) {V(test_sna)$color[k] = n_color[3]}
    } else if (max(ebd[t,]) == ebd[t,3]) {
      if (ebd[t,3] >2/3 & round(ebd[t,3],4) <=1) {V(test_sna)$color[k] = ytd_color[6]}
      if (ebd[t,3] >1/2 & ebd[t,3]<=2/3) {V(test_sna)$color[k] = ytd_color[8]}
      if (ebd[t,3]>=1/3 & ebd[t,3]<=1/2) {V(test_sna)$color[k] = ytd_color[10]}

    } else {
      print('error')
    }
  }  
  return(V(test_sna)$color)
}
