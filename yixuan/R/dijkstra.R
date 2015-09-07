#' Dijkstra's algorithm is an algorithm for finding the shortest 
#' paths between nodes in a graph.
#' @param graph A data.frame.
#' @param init_node A number.
#' @return {a vector of shortest path from the initial node to every other node in the graph.}
#' \code{\link{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}}
#' @examples
#' {wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)}

dijkstra <- function(graph, init_node){
  stopifnot(is.data.frame(graph) && is.numeric(init_node ) && any(init_node == graph[,1]))
  stopifnot(graph[,1]==v1&&graph[,2]==v2&&graph[,3]==w)
  result <- list()
  graph1 <- list()
  i <- 2
  cal <- vector(length = max(max(graph[,1]), max(graph[,2])))
  cal[init_node] <- init_node
  dis <- vector()
  dis[init_node] <- 0
  
  result[[1]] <- vector(mode = "numeric", length = 6L)
  result[[1]][] <- Inf
  result[[1]][init_node] <- 0
  for (i in 2 : max(max(graph[,1]), max(graph[,2]))){
    result[[i]] <- vector(mode = "numeric", length = 6L)
    result[[i]][] <- Inf
    j <- 1
    graph1[[(i-1)]] <- graph[graph[,1]==which.min(result[[(i-1)]]) , ]
    for(j in 1 : max(max(graph[,1]), max(graph[,2]))){
      if(any(j != cal[j])){
        
        result[[i]][j]<- min(min(result[[(i-1)]] + graph1[[(i-1)]][graph1[[(i-1)]][,2] == j, 3]),result[[(i-1)]][j])
      }
    }
    dis[which.min((result)[[i]])] <- min(result[[i]])
    cal[which.min((result)[[i]])] <- which.min((result)[[i]])
  }
  return(dis)
}