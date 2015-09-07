#' Add together two numbers.
#' 
#' @param gra A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)

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