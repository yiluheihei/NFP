data(kegg_refnet)

## NFPRefnet object
test_that('class of NFPRefnet and its components are as expected',{
  expect_that(kegg_refnet, is_a('NFPRefnet'))
  expect_that(kegg_refnet@network, is_a('list'))
  expect_that(kegg_refnet@group, is_a('character'))
  expect_that(kegg_refnet@name, is_a('list'))
  expect_that(kegg_refnet@organism, is_a('character'))
})

test_that('method refnet_name of NFPRefnet are as expected',{
  refnet.name <- refnet_name(kegg_refnet)
  expect_that(refnet.name, is_a('list'))
  expect_that(length(refnet.name), is_equivalent_to(4))
  ##expect_that(lengths(refnet.name), is_equivalent_to(c(22,28,15,69)))
})

test_that('method net of NFPRefnet are as expected',{
  net.NFP <- net(kegg_refnet)
  expect_that(net(kegg_refnet), is_a('list'))
  expect_that(length(net.NFP), is_identical_to(4L))
  ##expect_that(lengths(net.NFP), is_equivalent_to(c(22,28,15,69)))
})

test_that('method group of NFPRefnet are as expected',{
  group.refnet <- group(kegg_refnet)
  expect_that(group(kegg_refnet), is_a('list'))
  expect_that(length(group(kegg_refnet)), is_identical_to(3L))
  expect_that(group.refnet$num, is_identical_to(4L))
  ##expect_that(group.refnet$size, is_equivalent_to(c(22,28,15,69)))
})

test_that('subnet of NFPRefnet are as expected',{
  group <- group(kegg_refnet)$name[1]
  sub_refnet <- subnet(kegg_refnet,group)
  expect_that(sub_refnet,is_a('NFPRefnet'))
  expect_that(length(sub_refnet),is_equivalent_to(1))
})
