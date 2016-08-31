#' The NFP package
#'
#' This package implementation the applications of network finger print method.
#'
#' @name NFP
#' @docType package
#'
#' @importFrom magrittr extract2 %>% extract subtract use_series %<>%
#' @importFrom plyr laply llply
#' @importFrom KEGGgraph getKGMLurl parseKGML2Graph
#' @importFrom igraph igraph.from.graphNEL V get.data.frame graph.data.frame V<- rewire keeping_degseq gsize
#' induced.subgraph as_adj
#' @importFrom apcluster apcluster preferenceRange
#' @importFrom ggplot2 ggplot geom_point geom_segment theme scale_x_discrete
#' element_text geom_line aes xlab ylab guides guide_legend
#' @importFrom stringr str_replace str_replace_all str_extract_all
#' @importFrom dplyr filter bind_cols
#' @importFrom graph nodeDataDefaults<- nodes
#' @importFrom tidyr gather
#' @importFrom methods is new show
#' @importFrom stats sd
#' @importFrom utils data install.packages read.delim2
NULL
