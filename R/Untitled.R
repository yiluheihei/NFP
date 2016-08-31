
plotnfp <-   function(x, type = c('matchstick', 'line','point'), p_size = 2, l_size = 0.5){
  type <- match.arg(type, c('matchstick', 'line','point'))
  if (class(x) == 'NFP'){
    nfp_score <- x@standardized_score
    nfp_refnet_group <- x@randomized_score %>%
      use_series('group')
    sim_df <- data.frame(NFP_sim = nfp_score, group = nfp_refnet_group,
      refnet_index = 1:nrow(x@randomized_score))
    network_num <- length(nfp_score)
    if(all(!is.na(nfp_score))){ # skip plot if sim is NA
      p <- ggplot(sim_df,aes(x = refnet_index, y = NFP_sim))
      if(type == "point")
        print(p + geom_point(size = p_size, aes(color = group)))
      if(type == "line")
        print(p + geom_line(size = l_size, aes(color = group, group = 1)))
      if (type == 'matchstick')
        print(p + geom_point(size = p_size, aes(color = group)) +
            geom_segment(aes(xend = refnet_index, yend = 0, color = group),
              size = l_size))
    }
    else
      stop('NFP similarity score must be NA')
  }
  else
  {
    nfp_num <- length(x)
    if (type != 'line')
      stop('type must be line while visualization of multiple nfps')
    nfp_score <- llply(x, function(x)x@standardized_score) %>% do.call(what = cbind) %>% 
      as.data.frame
    network_num <- nrow(nfp_score)
    sim_df <- data.frame(nfp_score,refnet_index = 1:network_num) %>% 
      gather(net,sim,-refnet_index)
    p <- ggplot(data = sim_df, aes(x = refnet_index, y =sim)) + 
      geom_line(aes(color = net),size = l_size) + 
      ylab('Network fingerprint') + xlab('Index of basic networks') +
      guides(color = guide_legend("query network"))
    print(p)
  }
}
