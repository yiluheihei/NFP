## ----style, eval=TRUE, echo=FALSE, results='asis'---------------------------------------
BiocStyle::latex()

## ----setup, include=FALSE, cache=FALSE, eval = TRUE, echo = FALSE-------------
library(knitr)
opts_chunk$set(
               fig.align='center', fig.show='asis',
               eval = TRUE,
               tidy = FALSE,
               message = FALSE,
               warning = FALSE)
options(replace.assign=TRUE,width=80)

## ----install-pkg, eval=FALSE--------------------------------------------------
#  ## install release version of mmnet
#  source("http://bioconductor.org/biocLite.R")
#  biocLite("NFP")
#  
#  ##install the latest development version
#  useDevel()
#  biocLite("NFP")

## ----install-pkg-github, eval=FALSE-------------------------------------------
#  ## install NFP from github
#  if (!require(devtools))
#    install.packages("devtools")
#  devtools::install_github("yiluheihei/NFP")

## ----load-pkg,eval=TRUE, include=FALSE----------------------------------------
library(NFP)

## ----load_kegg_refnet, eval=TRUE, echo=TRUE-----------------------------------
## donot run, retrive pathway maps from KEGG database may take several minutes,
## we have pre-stored this data in our package
## kegg_refnet <- load_KEGG_refnet(organism = 'hsa')
data(kegg_refnet)

# show the kegg reference networks
show(kegg_refnet)

## ----keggrefnet-methods,echo=TRUE,eval=TRUE-----------------------------------
## group information of kegg reference networks
refnet_group <- group(kegg_refnet)
show(refnet_group)

## select goup 1 and 2, and subset this two groups
selected_group <- refnet_group$name[c(1,2)]
NFPnet <- subnet(kegg_refnet,selected_group)
NFPnet

## ----reactome-map-------------------------------------------------------------
## Reactome human pathway maps
require(graphite)
human_pathway <- pathways("hsapiens", "kegg")
## just choose first two pathway maps for testing
p <- human_pathway[1:2]
show(p)
g <- lapply(p, pathwayGraph)
show(g)

## ----customize-refnet,echo=TRUE, eval=TRUE------------------------------------
## here, just take the above two reactome pathway maps as NFP basic reference
## networks as example
g_names <- names(human_pathway)[1:2]
## only one group and two reference networks
customized_refnet <- new("NFPRefnet",network = list(g), name = list(g_names),
  group = "test group", organism ='hsa')

## methods of NFPRefnet
show(customized_refnet)
group(customized_refnet)
subnet(customized_refnet, 'test group', 1)

## ----calc-NFP, eval=TRUE, echo=TRUE-------------------------------------------
## set g as the query network
query_net <- g
## a subset of kegg_refnet, select the head five networks of group 1, 2
group_names <- group(kegg_refnet)$name
sample_NFPRefnet <- subnet(kegg_refnet, group_names[1:2],list(1:5,1:5))

## In order to save calculating time, we take nperm = 10
NFP_score <- lapply(query_net, calc_sim_score,NFPnet = sample_NFPRefnet,
  nperm = 10)

## methods of NFP class
show(NFP_score[[1]])
randomized_score  <- perm_score(NFP_score[[1]])
cluster <- cluster_info(NFP_score[[1]])

## ----plot-nfp,echo=TRUE,eval=TRUE,fig=TRUE, fig.align='center',fig.cap='Plot a NFP object',dev='pdf',fig.show='hold',out.width='.8\\linewidth', out.height='.7\\linewidth'----
plot(NFP_score[[1]])

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

## ----closeConnetions----------------------------------------------------------
allCon <- showConnections()
socketCon <- as.integer(rownames(allCon)[allCon[, "class"] == "sockconn"])
sapply(socketCon, function(ii) close.connection(getConnection(ii)) )

