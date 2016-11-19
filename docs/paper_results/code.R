library(NFP)
library(igraph)
load('sample_FOXM1_Kwoneel.rdata')
FOXM1_net <- igraph.to.graphNEL(sample_FOXM1_Kwoneel)

## may take several hours duo to the large nperm (1000)
FOXM1_nfp <- calc_sim_score(FOXM1_net,kegg_refnet, nperm = 1000)
