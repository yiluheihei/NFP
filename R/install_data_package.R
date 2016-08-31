#' @title Install NFP data package NFPdata
#' @description Downloads and Install the NFPdata Package to use with the NFP 
#' package
#' @param type A \code{string} with value \code{"ONL"} or \code{"LOCAL"}
#' @param loc  A \code{string} that contains the file location.
#' @details The NFPdata Package contains data that from kegg gene similarity
#'   based on gene ontology and is approximately a 16.4 MB download.
#'   
#' @examples 
#' \dontrun{
#' # Online install
#' install_data_package()
#' }
#' @export
install_data_package = function(type="ONL",loc=NULL){
  type = toupper(type)
  if(type == "ONL"){
    install.packages("NFPdata", repos = "https://yiluheihei.github.io/datarepo/")
  }else{
    if(!is.null(loc)){ 
      message("Installing the package using a local .tar file.")
      message("Note: You must have a compiler installed!")
      message("you must download the .tar file first from our github")
      install.packages(loc, type="source")
    }else{ 
      message("To install locally, you must also set the `loc` paramter!")
    }
  }
}
