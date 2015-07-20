#' @title  Load the kegg signal pathway map as the reference net
#'
#' @description This function generates the well-studied KEGG signal pathway
#' which was taken as the reference net.
#'
#' @return  a igraph list which represents the kegg signal pathway
#'
#' @export

loadKEGGRefNet <- function(){
  pathway_ID <- system.file('extdata/signalPathway.txt',package='NFP') %>%
    read.delim(stringsAsFactors = FALSE,header = FALSE,colClasses = 'character') %>%
    extract2(1)
  human_pathway <- system.file('extdata/humanPathway.txt',package='NFP') %>%
    read.delim(stringsAsFactors = FALSE,
      header = FALSE,colClasses = 'character') %>%
    extract2(1)
  human_signalpathway <- intersect(pathway_ID,human_pathway)
  human_signalpathway <- human_signalpathway[-125] ## ? bug
  KEGG_graph <- human_signalpathway %>% laply(getKGMLurl,organism = 'hsa') %>%
    llply(parseKGML2Graph,.progress = 'text') %>% llply(igraph.from.graphNEL)
  names(KEGG_graph) <- human_signalpathway
  return(KEGG_graph)
}
