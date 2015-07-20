#' @title calculate the similarity score in a specific cluster.
#'
#' @param node1, a nodes vector of net1
#' @param node2, a nodes vector of net2
#' @param cluster, cluster as a list of indices of samples belonging to the
#' cluster.
#' @param gene_sim, gene similarity matrix based on Gene Ontology

calc_cluster_score <- function(cluster,node1,node2,gene_sim){
 v1 <- intersect(node1,names(cluster))
 v2 <- intersect(node2,names(cluster))
 cluster_sim <- gene_sim[cluster,cluster]
 index1 <- match(v1,row.names(gene_sim),nomatch = 0)
 index2 <- match(v2,row.names(gene_sim),nomatch = 0) ## same as setdiff(cluster,index1)
 if (length(index1) && length(index2)){
   LS1 <- sapply(index1,function(x)max(gene_sim[x,index2])) %>% sum
   LS2 <- sapply(index2,function(x)max(gene_sim[x,index1])) %>% sum
   LS <- (LS1 + LS2)/length(cluster)
 }
 else
   LS <- 0
}

#' calculate the similarity scores between two networks
#'
#' @param net1,  a igaph network
#' @param net2, same as net2
#' @param ..., all other arguments are passed to apcluster

calc_twonet_score <- function(net1,net2, ...){
  ## merge two networks
  node1 <- V(net1)$name %>% str_replace('hsa:','')
  node2 <- V(net2)$name %>% str_replace('hsa:','')
  data(geneSimData)
  node_index <- c(node1,node2) %>% unique %>%
    match(row.names(geneSimData),nomatch = 0)
  new_nodes <- row.names(geneSimData)[node_index]
  edges1 <- get.data.frame(net1)
  edges1$from <- str_replace(edges1$from,'hsa:','')
  edges1$to <- str_replace(edges1$to,'hsa:','')
  edges2 <- get.data.frame(net2)
  edges2$from <- str_replace(edges2$from,'hsa:','')
  edges2$to <- str_replace(edges2$to,'hsa:','')
  new_edges <- rbind(edges1,edges2) %>%
    filter(is.element(from,new_nodes) & is.element(to,new_nodes))
  new_graph <- graph.data.frame(new_edges,vertices = new_nodes)

  gene_sim <- geneSimData[node_index,node_index]
  gene_cluster <- apcluster(s = gene_sim, p = mean(preferenceRange(gene_sim), ...))
  ## cluster <- names(gene_cluster[[1]])
  ## calculate the similarity score in a specific cluster
  score <- sapply(1:length(gene_cluster),function(x)calc_cluster_score(gene_cluster[[x]],
    node1,node2,gene_sim)) %>% mean
  return(score)
}

#' @title randomize the network

randomize_net <- function(net){
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
  if (plot)
    print(ggplot(sim_df,aes(x=pathway,y=sim)) + geom_point(size=3) +
      geom_segment(aes(xend=pathway,yend=0),size=1))
  return(sim)
}
