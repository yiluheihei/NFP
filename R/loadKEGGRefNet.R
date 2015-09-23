getPathwayList <- function(){
  ## detail kegg pathway information 
  map_doc <- readLines(paste0('http://www.kegg.jp/kegg-bin/',
    'download_htext?htext=br08901.keg&format=htext&filedir='))
  
  index <- lapply(map_doc,grepl,pattern = '^A|^C') %>%
    unlist %>%
    which
  map_doc <- map_doc[index]
  class_index <- lapply(map_doc, grepl, pattern = '^A') %>%
    unlist %>%
    which
  class_name <- map_doc[class_index] %>%
    lapply(str_replace_all,pattern = '^A<b>(.*)</b>', replacement = "\\1") %>%
    unlist
  map_class <- mapply(function(x,y){map_doc[x:y] %>% lapply(str_extract_all,
    pattern = '\\d{5}') %>% unlist}, class_index + 1,
    c(class_index[-1] - 1,length(map_doc)))
  names(map_class) <- class_name
  
  ## Obtain human pathway from KEGG api 
  human_pathway <- read.delim2('http://rest.kegg.jp/list/pathway/hsa',
    stringsAsFactors = FALSE, head = FALSE)
  human_pathway[[1]] <- lapply(human_pathway[[1]],str_extract_all,'\\d+') %>%
    unlist
  human_pathway_class <- lapply(map_class, match, table = human_pathway[[1]], 
    nomatch = 0) %>%
    lapply(function(x)human_pathway[x,]) %>%
    extract(-length(.))
  human_signal_pathway  <- human_pathway_class[c(-1,-6)] %>%
    do.call(rbind,.)
  ## return(kegg_reference_net <- list(
  ## Metabolism = human_pathway_class[["Metabolism"]],
  ## Signal = human_pathway_class[c(-1,-6)],
  ## Disease = human_pathway_class[["Human Diseases"]])
  ## )
  return(human_pathway_class[c(-1,-6)])
}

#' @title  Load the the reference molecular networks
#'
#' @description This function generates the well-studied "basic networks". 
#' 
#' @param organsim, a character indicating to which organsim's pathway map was 
#' taken as the basic network. e.g. \emph{hsa}.
#' 
#'
#'@details KEGG pathway is a well-studied and the most widely used 
#' biolocgial networks database. This function help users to load kegg pathway 
#' maps as the basic networks. 
#' 
#' Appanrently, users can also load their customozied biological networks as the
#' basic networks by creating a new \emph{NFPRefnet} object.  
#'
#' @return  a KEGGRefnet object
#' 
#' @seealso \code{\link{NFPRefnet-class}}
#'
#' @export
#' 
#' 
loadKEGGRefNet  <- function(organism = 'hsa'){
  human_signalpathway <- getPathwayList()
  human_signalpathway[[4]] <- human_signalpathway[[4]][-62,]
  ref_net <- human_signalpathway %>%
    llply(. %>% extract2(1) %>% laply(getKGMLurl,organism = organism)) %>% 
    llply(. %>% llply(parseKGML2Graph), .progress = 'text') %>% 
    llply(. %>% llply( igraph.from.graphNEL)) 
  group <- names(ref_net)
  names <- human_signalpathway %>% llply(. %>% extract2(2))
  ref_net_organism <- organism
  return(new("NFPRefnet",network = ref_net, name = names, group = group, 
    organism = ref_net_organism))
}




