utils::globalVariables("geneSimData")

#' @title Calculating the similarity scores
#'
#' @description This function was used to calculate the similarity scores
#' between a network and the reference network.
#'
#' @param  net, a graphNel object to represent the query biological networks,
#' for more details see \code{\link[graph]{graphNEL}}
#'
#' @param  NFPnet, a NFPRefnet object, one or more kegg pathway map, or
#' customized networks. For more details see \code{\link{NFPRefnet-class}}.
#'
#' @param nperm, number of random networks for similarity score standardization
#'
#' @param ..., arguments passed to apcluster
#'
#' @return a similarity scoring vector,length is the same as the number of
#' networks
#'
#' @export
#'
#' @seealso \code{\link{NFPRefnet-class}}.



calc_sim_score <- function(net,NFPnet,nperm = 100, ...){
  
  #' @title calculate the similarity score in a specific cluster.
  #'
  #' node1, a nodes vector of net1
  #' node2, a nodes vector of net2
  #' cluster, cluster as a list of indices of samples belonging to the
  #' cluster.
  
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
  #' This function is used for similarity score calculation between two networks. 
  #' 
  #'  net1,  a igraph network
  #'  net2, same as net2
  #'  ..., all other arguments are passed to apcluster
  
  calc_twonet_score <- function(net1,net2, ...){
    
    subgraphwithbp <- function(net){
      ##node <- V(net)$name %>% str_replace(paste0(organism,':'),'')
      node <- V(net)$name
      node_index <- node %>% match(row.names(geneSimData),nomatch = 0)
      ##node_bp <- row.names(geneSimData)[node_index] %>% paste(paste0(organism,':'),.,sep='')
      node_bp <- row.names(geneSimData)[node_index]
      if(length(node_index) != length(node_bp)) {
        ##warning("nodes in network without BP annotation will be ignored!")
        return(induced.subgraph(net,node_bp))
      }
      else
        return(net)
    }
    ## merge two networks
    net1 %<>% subgraphwithbp
    net2 %<>% subgraphwithbp
    
    
    ##node1 <- V(net1)$name %>% str_replace(paste0(organism,':'), '')
    ##node2 <- V(net2)$name %>% str_replace(paste0(organism,':'), '')
    node1 <- V(net1)$name
    node2 <- V(net2)$name
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
    return(list(score = score, cluster = gene_cluster))
  }
  
  #'  randomize the network
  #'  randomize the network for similarity score standardization.
  #'  net, igraph object
  
  randomize_net <- function(net){
    #edge shuffle
    ##net <- igraph.from.graphNEL(net)
    net <- rewire(net, keeping_degseq(niter = gsize(net) * 4))
    
    #node shuffle
    genes <- row.names(geneSimData)
    index <-  genes %>% length %>% sample(length(V(net)))
    ##new_node <- genes[index] %>% paste0(paste0(organism,':'),.)
    new_node <- genes[index]
    V(net)$name <- new_node
    return(net)
  }
  
  #'  standardize the similarity score
  #'  This function is to standardize the similarity score.
  #'  net, igraph object
  #'  NFPnet, NFPRefnet, the reference NFP networks
  #'  nperm, permutation number for NFP standardization
  #' ..., argument passed to calc_twonet_score
  
  standardize_score <- function(net,NFPnet,nperm,...){
    calc_random_score <- function(net,NFPnet,...){
      random_net <- randomize_net(net)
      refnet <- net(NFPnet) %>%
        llply(. %>% llply( igraph.from.graphNEL))
      random_score <- lapply(refnet,
        function(x)lapply(x, function(y)calc_twonet_score(random_net,y))) %>%
        lapply(. %>% lapply(. %>% extract2(1))) %>% unlist
      ##if (class(refnet) == 'list')
      ##  random_score <- llply(refnet,function(x)calc_twonet_score(random_net,x,...))
      ##else
      ##  random_score <- calc_twonet_score(random_net,refnet,...)
      ##group_name <- group(NFPnet)$name
      ##times <- net(NFPnet) %>% sapply(. %>% length)
      ##names(random_score) <- group_name
      ##group <- rep(group_name,times)
      ##return(data_frame(random_score = random_score, group = group))
      return(random_score)
    }
    random_score <- replicate(nperm,calc_random_score(net,NFPnet),simplify = FALSE) %>%
      as.data.frame
    group_name <- group(NFPnet)$name
    times <- net(NFPnet) %>% sapply(. %>% length)
    names(random_score) <- group_name
    group <- rep(group_name,times)
    random_score['group'] <- group
    random_score['net_names'] <- refnet_name(NFPnet) %>% unlist
    rownames(random_score) <- NULL
    return(random_score)
  }
  
  if (class(net) != 'graphNEL')
    stop("Please ensure your network is in graphNEL class")
  net <- igraph.from.graphNEL(net)
  refnet <- net(NFPnet)
  refnet <- refnet %>%
    llply(. %>% llply( igraph.from.graphNEL))
  if (!requireNamespace("NFPdata", quietly = TRUE )){ 
    stop("NFPdata needed for similarity calculation, please install it first: install_data_package()")
  }
  geneSimData <- NFPdata::geneSimData
  sim_score_cluster  <- lapply(refnet,function(x)lapply(x,
    function(y)calc_twonet_score(net,y)))
  sim_score <- sim_score_cluster %>%
    lapply(. %>% lapply(. %>% extract2(1))) %>% unlist
  names(sim_score) = NULL
  gene_cluster <- sim_score_cluster %>%
    lapply(. %>% lapply(. %>% extract2(2))) %>% unlist
  group_name <- group(NFPnet)$name
  times <- net(NFPnet) %>% sapply(. %>% length)
  group <- rep(group_name,times)
  sim_score <- data.frame(sim_score = sim_score, group =  group,
    net_names = refnet_name(NFPnet) %>% unlist)
  rownames(sim_score) <- NULL
  random_score <- standardize_score(net,NFPnet,nperm, ... )
  random_score_E <- rowMeans(random_score[1:nperm])
  random_score_SD <- apply(random_score[1:nperm], 1, sd, na.rm = TRUE)

  if (FALSE){
    if (class(refnet) == 'list'){
      sim_score <- llply(refnet,function(x)calc_twonet_score(net,x,.)) %>% unlist
      random_score <- standardize_score(net,refnet,nperm, ...)
      random_score_E <- sapply(random_score, function(x) x %>% unlist %>% mean)
      random_score_SD <- sapply(random_score, function(x) x %>% unlist %>% sd)
    }else{
      sim_score <- calc_twonet_score(net,refnet,...)
      random_score <- standardize_score(net,refnet,nperm, ...) %>% unlist
      random_score_E <- mean(random_score)
      random_score_SD <- sd(random_score)
    }
  }

  sim <- (sim_score[["sim_score"]] - random_score_E)/random_score_SD

  ## plot
  if(FALSE){
    if (is.null(names(refnet)))
      sim_df <- data.frame(sim = sim, pathway = 1:length(sim))
    else
      sim_df <- data.frame(sim = sim, pathway = names(refnet))
    if (plot && !is.na(sim)){ # skip plot if sim is NA
      print(ggplot(sim_df,aes(x=pathway,y=sim)) + geom_point(size=3) +
        geom_segment(aes(xend=pathway,yend=0),size=1))
    }
  }
  return(new('NFP',raw_score = sim_score[["sim_score"]],
    randomized_score = random_score,
    standardized_score = sim,
    cluster = gene_cluster)
  )
}
