# Init

s=c('stringr','plyr','KEGGgraph','igraph','apcluster','ggplot2','dplyr','graph','magrittr')
lapply(s,install.packages)
lapply(s,library,character.only = TRUE)

ref.net=loadKEGGRefNet()
save(ref.net, file='net.rdata')
exportKEGGRefNet()


#Reload script
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#Prepare for debug
load('net.rdata')
data(geneSimData)
sourceDir('../R')

# test sim score
Rprof('prof')
temp=calc_sim_score(all.net[[1]],ref.net,nperm =1, plot = TRUE)
Rprof(NULL)
summaryRprof('prof')

# validate
net1=igraph.from.graphNEL(parseKGML2Graph(getKGMLurl("04630",organism = 'hsa')))
net2=igraph.from.graphNEL(parseKGML2Graph(getKGMLurl("04650",organism = 'hsa')))
temp=calc_sim_score(net1,net2,nperm =1, plot = TRUE)

##=======================================================
##                       YANG
##=======================================================
library(NFP)
 kegg_refnet <- loadKEGGRefNet()

