utils::globalVariables("sim")
#' Plot multiple NFPs.
#'
#' Function for visualization multiple NFPs.
#'

#' @param object, \code{NFP} class list.
#' @param l_size, line size of plot, default is 0.5.
#' @aliases plot_NFPlist
#' @seealso \code{\link{NFP-class}}
#' @export

plot_NFPlist <- function(object, l_size = 0.5){
  if (class(object) != 'list')
    stop('object must be a NFP list while visualization of multiple nfps')
  nfp_num <- length(object)
  nfp_score <- llply(object, function(object)object@standardized_score) %>% do.call(what = cbind) %>% 
    as.data.frame
  names(nfp_score) <- names(nfp_score)
  network_num <- nrow(nfp_score)
  sim_df <- data.frame(nfp_score,refnet_index = 1:network_num) %>% 
    gather(net,sim,-refnet_index)
  p <- ggplot(data = sim_df, aes(x = refnet_index, y =sim)) + 
    geom_line(aes(color = net),size = l_size) + 
    ylab('Network fingerprint') + xlab('Index of basic networks') +
    guides(color = guide_legend("query network"))
  print(p)
}