utils::globalVariables(".")
utils::globalVariables("geneSimData")
utils::globalVariables("refnet_index")
utils::globalVariables("NFP_sim")
.check.NFP <- function(object){
  if(!is(object, "NFP")) stop("object has to be of class \"NFP\" ")
  errors <- character()
  if (!is.numeric(object@raw_score))
    errors <- c(errors, "raw_score must be a numeric")
  if (!is.data.frame(object@randomized_score))
    errors <- c(errors, "randomzid_score must be a character")
  if(!is.numeric(object@standardized_score))
    errors <- c(errors, "standardized_score must be a numeric")
  if(!is.list(object@cluster))
    errors  <- c(errors, "name must be a list")
  ##if(!is.character(object@refnet_names))
  ##errors <- c(errors, "refnet_names must be a character")
  ##if(!is.character(object@group))
  ##errors <- c(errors, "group must be a character")
  if(length(errors) == 0)
    TRUE
  else
    errors
}

#'\code{NFP-class}
#'
#'An S4 object for storing network fingerprint similarity score information.
#'
#'@slot raw_score, a numeric vector, network fingerprint based on reference
#'networks before standardization.
#'@slot randomized_score, a data frame, the permulated similarity score.
#'@slot standardized_score, a numeric vector, the final standardized network fingerprint.
#'@slot cluster, an \emph{APResult} list, more details see package **apcluster**,
#'each element provides a cluster information of a
#'biological network based on one reference networks.
#' #'@section method:
#'    \itemize{
#'      \item{perm_score, \code{signature(object = "NFP")}:
#'        extract the randomized similarity score}
#'      \item{cluster_info, \code{signature(object = "NFP")}:
#'        extract the cluster information}
#'      \item{sub_NFP, \code{signature(object = "NFP")}:
#'        subset of NFP object}
#'      \item{plot, \code{signature(object, type = "character", p_size = "numeric", l_size = 'numeric')}:
#'        plot NFP results}
#'      \item{show, \code{signature(object = "NFP")}:
#'        display methods for S4 classes NFP, see also
#'        \code{\link[methods]{show}}}
#'    }
#'
#' @name NFP-class
#' @rdname NFP-class
#' @exportClass NFP
#' @seealso \code{\link{show-methods}},
#' \code{\link{plot-methods}}, \code{\link{perm_score-methods}},
#' \code{\link{cluster_info-methods}}, \code{\link{sub_NFP-methods}}
#'
setClass("NFP", slot=list(raw_score = "numeric", randomized_score = 'data.frame',
  standardized_score = "numeric",cluster = "list"),
  prototype = list(raw_score = numeric(0), randomized_score = NULL,
    standardized_score = numeric(0), cluster = NULL))

#' Extract the randomized similarity score
#'
#' This function extract the randomized similarity score for standardization.
#'
#'@exportMethod perm_score
#'@rdname perm_score-methods
#'@name perm_score-methods
#'@param object, \code{NFP} class
#'@aliases perm_score perm_score-methods
#'@docType methods
#'@seealso \code{\link{NFP}}
#'@return a data frame, each col (elements) represents once permutation
#'similarity score, each row indicate a reference basic network.

setGeneric("perm_score",
  function(object){standardGeneric("perm_score")})
#' @rdname perm_score-methods
#' @aliases perm_score perm_score-methods
setMethod("perm_score",signature="NFP",
  function(object){
    object@randomized_score
  }
)

#' Extract the cluster information of \emph{NFP}.
#'
#' This function extract the cluster information of network fingerprint.
#'
#'@exportMethod cluster_info
#'@rdname cluster_info-methods
#'@name cluster_info-methods
#'@param object \code{NFP} object
#'@aliases cluster_info cluster_info-methods
#'@docType methods
#'@seealso \code{\link{NFP}}
#'@return a list which contains the number, the examplar and some other cluster
#'properties.

setGeneric("cluster_info",
  function(object){standardGeneric("cluster_info")})
#' @rdname cluster_info-methods
#' @aliases cluster_info cluster_info-methods
setMethod("cluster_info",signature="NFP",
  function(object){
    NFP_cluster <- object@cluster
    return(NFP_cluster)
  }
)

#' subset of NFP object
#'
#' This function extract the subsets of NFP-class.
#'
#'@exportMethod sub_NFP
#'@rdname sub_NFP-methods
#'@name sub_NFP-methods
#'@param object, \code{NFP} class
#'@param i, numeric or character indicating the index or the names of the
#'reference network
#'@aliases sub_NFP sub_NFP-methods
#'@docType methods
#'@seealso \code{\link{NFP-class}}
#'@return an similar NFP object contain just the selected elements.

setGeneric("sub_NFP",
  function(object, i){standardGeneric("sub_NFP")})
#' @rdname sub_NFP-methods
#' @aliases sub_NFP sub_NFP-methods
setMethod("sub_NFP",signature="NFP",
  function(object,i){
    if(!(is.character(i) || is.numeric(i)))
      stop('i must be character or numeric')
    ##ans <- c("raw_score", "randomized_score", "standardized_score", "cluster")
    raw_score <- object@raw_score
    randomized_score <- object@randomized_score
    standardized_score <- object@standardized_score
    cluster <- object@cluster
    refnet_name <- randomized_score$net_names
    names(raw_score) <- refnet_name
    row.names(randomized_score) <- refnet_name
    names(standardized_score) <- refnet_name
    names(cluster) <- refnet_name
    if(is.character(i))
      i <- match(i,refnet_name, nomatch = 0)
    sub_raw_score <-raw_score[i]
    sub_randomized_score <- randomized_score[i,]
    sub_standardized_score <- standardized_score[i]
    sub_cluster <- cluster[i]
    return(new('NFP',raw_score = sub_raw_score, randomized_score = sub_randomized_score,
      standardized_score = sub_standardized_score, cluster = sub_cluster))
  }
)


#' Show an Object
#'
#' show method short for NFP object, see \code{\link[methods]{show}}.
#'
#'@exportMethod show
#'@param object, \code{NFP} object
#'@docType methods
#'@rdname show_NFP-methods
#'@aliases show_NFP show_NFP-methods
setMethod("show", "NFP",
  function(object){
    para_name <- c("Number of ref networks", "Permutation number")
    network_num <- object@raw_score %>% length
    perm_num <- object@randomized_score %>% ncol %>% subtract(2)
    cluster_num <- lapply(object@cluster, length) %>% unlist
    cluster_num <- ifelse(length(cluster_num) > 5, cluster_num[1:5], cluster_num)
    para_num <- c(length(object@raw_score), length)
    group_num <- object@randomized_score %>% use_series('group') %>%
      unique %>% length
    cat(class(object),"object","\n")
    cat("\n")
    cat("Number of ref networks", "=", network_num, "\n")
    cat('Group numbrer of networks', '=', group_num, '\n')
    cat("Permutation number", "=", perm_num,"\n")
    if(length(cluster_num) > 5)
      cat("Number of clusters", '=', paste0(cluster_num,','),
        "...omit several results","\n")
    else
      cat("Number of clusters", '=', cluster_num,"\n")
    cat('\n')
    cat('Standardized NFP\n')
    print(object@standardized_score)
  }
)

#' Plot NFP results
#'
#' Function for visualization NFP results.
#'
#'@exportMethod plot
#'@param x, \code{NFP} class
#'@param type, types of the visaulization of \emph{NFP} object, point or line.
#'Default is point.
#'@param p_size, point size of plot, default is 3.
#'@param l_size, line size of plot, default is 1.
#'@docType methods
#'@rdname plot-methods
#'@seealso \code{\link{NFP-class}}
#'@aliases plot plot-methods
setMethod("plot",signature = (x = "NFP"),
  function(x, type = c('matchstick', 'line','point'), p_size = 3, l_size = 1){
    type <- match.arg(type, c('matchstick', 'line','point'))
    nfp_score <- x@standardized_score
    nfp_refnet_group <- x@randomized_score %>%
      use_series('group')
    sim_df <- data.frame(NFP_sim = nfp_score, group = nfp_refnet_group,
      refnet_index = 1:nrow(x@randomized_score))
    network_num <- length(nfp_score)
    if(all(!is.na(nfp_score))){ # skip plot if sim is NA
      p <- ggplot(sim_df,aes(x = refnet_index, y = NFP_sim))
        ##scale_x_discrete(breaks = 1:network_num, labels =  1:network_num) +
        ##theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      if(type == "point")
        print(p + geom_point(size = p_size, aes(color = group)))
      if(type == "line")
        print(p + geom_line(size = l_size, aes(color = group)))
      else
        print(p + geom_point(size = p_size, aes(color = group)) +
          geom_segment(aes(xend = refnet_index, yend = 0, color = group),
            size = l_size))
    }
    else
      stop('NFP similarity score must be NA')
  }
)

