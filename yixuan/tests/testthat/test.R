context("euclidean correctness")
test_that("eucledian correctness" , {
  expect_that(euclidean(1,4), equals(1))
  expect_that(euclidean(3,7), equals(1))
  expect_that(euclidean(10,100),equals(10))
  expect_that(euclidean(4,4),equals(4))
})

context("dijkstra correctness")
test_that("dijkstra correctness",{
  wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                          w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
  expect_that(dijkstra(wiki_graph, 1), equals(c(0, 7, 9, 20, 20, 11))) 
  expect_that(dijkstra(wiki_graph, 3), equals(c(9, 10, 0, 11, 11, 2)))
})