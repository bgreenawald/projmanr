# Ben Greenawald

# Topologically sort project tasks

# Produces a list to be handled by the graph
make_node_list <- function(tasks, map, all_ids){
  ids <- character()
  successor <- character()
  
  for(id in all_ids){
    exp <- sprintf("map$%s", id)
    succ_task <- eval(parse(text = exp))
    for(id2 in succ_task$successor_id){
      ids <- c(ids, id)
      successor <- c(successor, id2)
    } 
  }
  
  ret <- data.frame(id = ids,
                    successor = successor, 
                    stringsAsFactors=FALSE)
  
  return(ret)
}
