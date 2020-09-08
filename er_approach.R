# ER Approach
# Refered to Yang et al. (1994), Xu & Yang (2001)
# Input 
#   bel_dist_1: the assessment (belief distributions) of evidence 1
#     (e.g., bel_dist_1 --> S(A_i(O_j)) = {(H_n,p_nj(O_j)), n=1,...,N})
#   bel_dist_2: the assessment (belief distributions) of evidence 2
#   rel_wgt_1: relative weight/interpersonal influence of evidence 1
#     (e.g., 0 <= rel_wgt_1 <= 1)
#   rel_wgt_2: relative weight/interpersonal influence of evidence 2
# Output
#   agr_bel_dist: aggregated belief distributions


er_approach <- function(bel_dist_1, bel_dist_2,rel_wgt_1, rel_wgt_2){
  
  # Mass functions [bel_dist_1]
  m_n1 <- rel_wgt_1 * bel_dist_1
  
  m_tilda_1 <- rel_wgt_1 * (1 - sum(bel_dist_1))  # incompleteness
  m_bar_1 <- 1 - rel_wgt_1  # the scope where other criteria play a role in the assessment
  
  m_H1 <- m_bar_1 + m_tilda_1
  
  # Mass functions [bel_dist_2]
  m_n2 <- rel_wgt_2 * bel_dist_2
  
  m_tilda_2 <- rel_wgt_2 * (1 - sum(bel_dist_2))
  m_bar_2 <- 1 - rel_wgt_2
  
  m_H2 <- m_tilda_2 + m_bar_2
  
  # ER algorithm
  if (length(m_n1) != length(m_n2)){
    print('Error: scales are different for two belief distributions')
    break()
  }
  
  # denominator k
  k_i <- c()
  for (i in 1:length(m_n1)) {k_i[i] <- m_n1[i] * sum(m_n2[-i])}
  k_value <- 1/(1 - sum(k_i)) 
  
  # beta (combined assessment --> belief distributions)
  
  m_bar_H <- k_value * (m_bar_1 * m_bar_2)
  m_tilda_H <- k_value * (m_tilda_1 * m_tilda_2 + m_tilda_1 * m_bar_2 + m_tilda_2 * m_bar_1)
  
  agr_bel_dist <- c()
  for (i in 1:length(m_n1)) {
    m_i <- (m_n1[i] * m_n2[i] + m_n1[i] * m_H2 + m_n2[i] * m_H1) * k_value
    agr_bel_dist[i] <- m_i/(1 - m_bar_H)
  }
  
  # the incompleteness of the aggregated belief distribution is not reflected in the output
  # in other words, sum(agr_bel_dist) <= 1
  return(agr_bel_dist)
}


