data(kegg_refnet)

# query_net <- igraph.from.graphNEL(parseKGML2Graph(getKGMLurl("04630",
#   organism = 'hsa')))

nodes_standard <- function(x){
  x@nodes = str_replace_all(nodes(x), paste0('hsa',":"),'')
  x.edge <- x@edgeL
  names(x.edge) <- x@nodes
  x@edgeL <- x.edge
  return(x)}

query_net <- parseKGML2Graph(getKGMLurl("04630",
  organism = 'hsa')) %>% nodes_standard

ref_net <- subnet(kegg_refnet,kegg_refnet@group[1],1:5)

## sim_score <- NFP::calc_sim_score(query_net,ref_net,nperm = 2)

## test_that('class of NFP and its components are as expected',{
##  expect_that(sim_score, is_a('NFP'))
##  expect_that(sim_score@raw_score, is_a('numeric'))
##  expect_that(sim_score@randomized_score, is_a('data.frame'))
##  expect_that(sim_score@standardized_score, is_a('numeric'))
##  expect_that(sim_score@cluster, is_a('list'))
##})

##test_that('perm_score method of NFP are as expected',{
##  perm.score <- perm_score(sim_score)
##  expect_that(perm.score, is_a('data.frame'))
##  expect_that(nrow(perm.score), is_equivalent_to(5))
##  expect_that(ncol(perm.score), is_equivalent_to(4))
##})

##test_that('cluster_info method of NFP are as expected',{
##  cluster.info <- cluster_info(sim_score)
##  expect_that(cluster_info(sim_score), is_a('list'))
##  expect_that(length(cluster.info), is_equivalent_to(5))
##  expect_that(cluster.info[[1]], is_a('APResult'))
##})

##test_that('sub_NFP method of NFP are as expected',{
##  sub.NFP <- sub_NFP(sim_score,1)
##  expect_that(sub_NFP(sim_score,1), is_a('NFP'))
##  expect_that(length(sub.NFP), is_equivalent_to(1))
##})
