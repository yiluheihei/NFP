#' @title calculate the similarity score in a specific cluster.
#'
#' @param node1, a nodes vector of net1
#' @param node2, a nodes vector of net2
#' @param cluster, cluster as a list of indices of samples belonging to the
#' cluster.
#' @param gene_sim, gene similarity matrix based on Gene Ontology

calc_cluster_score <- function(cluster,node1,node2){
 v1 <- intersect(node1,names(cluster))
 v2 <- intersect(node2,names(cluster))
 index1 <- match(v1,row.names(geneSimData),nomatch = 0)
 index2 <- match(v2,row.names(geneSimData),nomatch = 0) ## same as setdiff(cluster,index1)
 if (length(index1) && length(index2)){
   LS1 <- sapply(index1,function(x)max(geneSimData[x,index2])) %>% sum
   LS2 <- sapply(index2,function(x)max(geneSimData[x,index1])) %>% sum
   LS <- (LS1 + LS2)/(length(index1)+length(index2))
 }
 else
   LS <- 0
 return(LS)
}

#' calculate the similarity scores between two networks
#'
#' @param net1,  a igaph network
#' @param net2, same as net2
#' @param ..., all other arguments are passed to apcluster

calc_twonet_score <- function(net1,net2, ...){
  if(!(exists('geneSimData') && typeof(geneSimData) == "double"))
    data(geneSimData)

  subgraphwithbp <- function(net){
    if(!(exists('geneSimData') && typeof(geneSimData) == "double"))
      data(geneSimData)
    node <- V(net)$name %>% str_replace('hsa:','')
    node_index <- node %>% match(row.names(geneSimData),nomatch = 0)
    node_bp <- row.names(geneSimData)[node_index] %>% paste('hsa:',.,sep='')
    if(length(node_index) != length(node_bp)) {
      warning("nodes in network without BP annotation will be ignored!")
      return(induced.subgraph(net,node_bp))
    }
    else
      return(net)
  }
  ## merge two networks
  net1 %<>% subgraphwithbp
  net2 %<>% subgraphwithbp
  
  node1 <- V(net1)$name %>% str_replace('hsa:','')
  node2 <- V(net2)$name %>% str_replace('hsa:','')
  node_index <- c(node1,node2) %>% match(row.names(geneSimData),nomatch = 0)
  gene_sim <- geneSimData[node_index,node_index]
  
  lo=1;hi=length(node1);
  gene_sim[lo:hi, lo:hi] <- gene_sim[lo:hi, lo:hi] * as.matrix(as_adj(net1))
  
  lo=length(node1)+1;hi=length(node1)+length(node2);
  gene_sim[lo:hi, lo:hi] <- gene_sim[lo:hi, lo:hi] * as.matrix(as_adj(net2))
  
  gene_cluster <- apcluster(s = gene_sim)
  ## cluster <- names(gene_cluster[[1]])
  ## calculate the similarity score in a specific cluster
  score <- sapply(1:length(gene_cluster),function(x)calc_cluster_score(gene_cluster[[x]],
    node1,node2)) %>% mean
  return(score)
}

#' @title randomize the network

randomize_net <- function(net){
  #edge shuffle
  net <- rewire(net, keeping_degseq(niter = gsize(net) * 4)) 
  
  #node shuffle
  genes <- row.names(geneSimData)
  index <-  genes %>% length %>% sample(length(V(net)))
  new_node <- genes[index] %>% paste0('hsa:',.)
  V(net)$name <- new_node
  return(net)
}

#' @title  standardize the similarity score

standardize_score <- function(net,refnet,nperm,...){
  calc_random_score <- function(net,refnet,...){
    random_net <- randomize_net(net)
    if (class(refnet) == 'list')
      random_score <- llply(refnet,function(x)calc_twonet_score(random_net,x,...))
    else
      random_score <- calc_twonet_score(random_net,refnet,...)
    return(random_score)
  }
  random_score <- replicate(nperm,calc_random_score(net,refnet),simplify = FALSE)
  return(random_score)
}

#' @title Calculating the similarity scores
#'
#' @description This function was used to calculate the similarity scores
#' between a network and the reference network.
#'
#' @param  net, a network, can be represented as adj matrix, edge list, or in
#' igraph class
#'
#' @param  refnet, a reference network list, one or more kegg pathway map, or
#' customized networks, can be represented as adj matrix, edge list, or in
#' igraph class
#'
#' @param nperm, number of random networks for similarity score standardization
#'
#'
#' @return a similarity scoring vector,length is the same as the number of
#' networks
#'
#' @export



calc_sim_score <- function(net,refnet,nperm = 100, plot = TRUE, ...){
  if (class(net) != 'igraph')
    stop("Please ensure your network is in graphNEL class")
  if (class(refnet) == 'list'){
    sim_score <- llply(refnet,function(x)calc_twonet_score(net,x,...)) %>% unlist
    random_score <- standardize_score(net,refnet,nperm, ...)
    random_score_E <- sapply(random_score, function(x) x %>% unlist %>% mean)
    random_score_SD <- sapply(random_score, function(x) x %>% unlist %>% sd)
  }else{
    sim_score <- calc_twonet_score(net,refnet,...)
    random_score <- standardize_score(net,refnet,nperm, ...) %>% unlist
    random_score_E <- mean(random_score)
    random_score_SD <- sd(random_score)
  }

  sim <- (sim_score - random_score_E)/random_score_SD

  if (is.null(names(refnet)))
    sim_df <- data.frame(sim = sim, pathway = 1:length(sim))
  else
    sim_df <- data.frame(sim = sim, pathway = names(refnet))
  if (plot && !is.na(sim)){ # skip plot if sim is NA
    print(ggplot(sim_df,aes(x=pathway,y=sim)) + geom_point(size=3) +
      geom_segment(aes(xend=pathway,yend=0),size=1))
  }
  return(sim)
}
