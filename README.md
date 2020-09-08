# Multiple-criteria-belief-modeling

Codes and data for the paper submitted to EJOR 
(Towards Understanding Socially Influenced Vaccination Decision Making: An Integrated Model of Multiple Criteria Belief Modelling and Social Network Analysis)

# 1. Codes

er_approach.R
- aggregating (weighted) belief distributions between two individuals/criteria

bel_to_dec_tran.R
- generating aggregated belief distributions (from bottom to top) and transforming 'vaccination intention' to m(accept), m(reject), and m(wait)

belief_generator.R
- generating initial belief distributions for each individual so that the initial vaccination coverage in the social network will approach to the reality (e.g., 44.9% for people aged 6 months to under 65 years with ont or more underlying clinical risk factors during 2019-20 winter season in England)

node_colour.R
- codes for assigning the colour of nodes based on the belief values

general_spreading_and_simulation
- codes for section 4.1 and 4.2
- starting from initial belief belief generation, going through belief spreading and updating, ending with (transforming belief to) vaccination decisions

# 2. Data

out.moreno_oz_oz
- network data used in the simulation-based analysis
- network description (http://moreno.ss.uci.edu/data.html#oz) and detailed statistics (http://konect.cc/networks/moreno_oz/)

# 3. (Folder) scenarios

containing both codes for each scenario (in section 4.3) and the results for 100 simulation runs
