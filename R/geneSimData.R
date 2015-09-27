#' KEGG gene semantic similarity based on Gene ontology
#'
#' A dataset containing the semantic similarity score among all KEGG genes based on
#' Gene Ontology
#'
#'
#' @format A similarity matrix whose rownames and the colnames represents the
#' gene name
#'
#'
#' @name geneSimData


loadSimData <- function(){
  if(!(exists('geneSimData') && typeof(geneSimData) == "double"))
    data(geneSimData)
  ##if(!exists('geneSimData'))
  ##geneSimData <- generateGeneSimData()
  return(geneSimData)
}

if(FALSE){
generateGeneSimData <- function(){
  #source("http://bioconductor.org/biocLite.R")
  #biocLite("GOSemSim")
  #biocLite('org.Hs.eg.db')
  ##library(GOSemSim)
  ##library(org.Hs.eg.db)
  all_genes <- mappedkeys(org.Hs.egENSEMBL)
  s=mgeneSim(mapped_genes, ont="BP", organism="human", measure="Resnik")
  return(s)
}
}
