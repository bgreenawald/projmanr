library(R6)
library(plotly)
library(igraph)
library(tidyr)
library(ggplot2)

# Class Defintion ---------------------------------------------------------


Task <- R6::R6Class("Task",
                    public = list(
                      id = NULL,
                      name = NULL,
                      duration = NULL,
                      predecessor_id = NULL,
                      successor_id = NULL,
                      early_start = NULL,
                      early_finish = NULL,
                      late_start = NULL,
                      late_finish = NULL,
                      slack = NULL,
                      is_critical = NULL,
                      start_date = NULL,
                      end_date = NULL,
                      initialize = function(id = NA, name = NA, duration = NA, predecessor_id = NA){
                        self$id <- to_id(id)
                        self$name <- name
                        self$duration <- as.numeric(duration)
                        self$predecessor_id <- unlist(proc_ids(predecessor_id))
                        self$successor_id <- NULL
                        self$is_critical <- FALSE
                        self$early_start <- 0
                        self$early_finish <- 0
                        self$late_start <- 0
                        self$late_finish <- 0
                        self$slack <- 0
                      }
                    )
)

# Functions ---------------------------------------------------------------


# Function to handle reading of processor ids
proc_ids <- function(ids){
  ids <- strsplit(ids, ",")
  ids <- lapply(ids, trimws)
  ids <- ids[[1]][ids[[1]] != ""]
  return(list(ids))
}

# Function to Convert input to R6 class
read_func <- function(x){
  id <- to_id(x[1])
  name <- x[2]
  duration <- x[3]
  pred_id <- x[4]
  new_Task <- Task$new(id, name, duration, pred_id)
}

# Convert numeric to id usable by the hash map
to_id <- function(id){
  id <- trimws(id)
  return(as.character(id))
}

# Gets the successor for an activity
get_successor <- function(task, full_tasks){
  ret_ids <- NULL
  task_id <- task$id
  for(cur_task in full_tasks){
    if(task_id %in% unlist(cur_task$predecessor_id)){
      ret_ids <- c(ret_ids, cur_task$id)
    }
  }
  task$successor_id <- ret_ids
  return(NULL)
}

# Function to walk ahead
walk_ahead <- function(map, ids, start_date = Sys.Date()){

  for(cur in ids){
    exp <- sprintf("map$'%s'", cur)
    current_task <- eval(parse(text = exp))
    if(length(current_task$predecessor_id) == 0){
      current_task$early_finish <- current_task$early_start + current_task$duration
      current_task$start_date <- Sys.Date()
    }else{
      for(id in current_task$predecessor_id){
        exp <- sprintf("map$'%s'", id)
        pred_task <- eval(parse(text = exp))
        if(current_task$early_start <= pred_task$early_finish){
          current_task$early_start <- pred_task$early_finish
          current_task$start_date <- pred_task$start_date + pred_task$duration
        }
      }
    }
    current_task$early_finish <- current_task$early_start + current_task$duration
    current_task$end_date <- current_task$start_date + current_task$duration
  }
}

# Function to walk back
walk_back <- function(map, ids){

  for(cur in rev(ids)){
    exp <- sprintf("map$'%s'", cur)
    current_task <- eval(parse(text = exp))
    if(length(current_task$successor_id) == 0){
      current_task$late_finish <- current_task$early_finish
    }
    for(id in current_task$successor_id){
      exp <- sprintf("map$'%s'", id)
      succ_task <- eval(parse(text = exp))
      if(current_task$late_finish == 0){
        current_task$late_finish <- succ_task$late_start
      }else{
        if(current_task$late_finish > succ_task$late_start){
          current_task$late_finish <- succ_task$late_start
        }
      }
    }
    current_task$late_start <- current_task$late_finish - current_task$duration
  }
}

# Calculate the critical path
crit_path <- function(ids, map){
  c_path <- NULL

  for(id in ids){
    exp <- sprintf("map$'%s'", id)
    task <- eval(parse(text = exp))
    if(task$early_finish == task$late_finish && task$early_start == task$late_start){
      c_path <- c(c_path, task$id)
      task$is_critical <- TRUE
    }else{
      task$is_critical <- FALSE
    }
  }

  return(c_path)
}

# Converts result to data frame for gantt chart
to_data_frame <- function(tasks){
  df <- data.frame(id <- character(),
                   name <- character(),
                   start_date <- double(),
                   end_date <- double(),
                   duration <- double(),
                   is_critical <- logical(),
                   pred_id <- character())

  for(task in tasks){
    if(task$id != "%id_source%" && task$id != "%id_sink%"){
      if(task$predecessor_id[1] == "%id_source%"){
        task$predecessor_id <- ""
      }
      df <- rbind(df, data.frame(id <- task$id,
                                 name <- task$name,
                                 start_date <- task$start_date,
                                 end_date <- task$end_date,
                                 duration <- task$duration,
                                 is_critical <- task$is_critical,
                                 pred_id <- paste(c(task$predecessor_id, " "), collapse = " "))
      )
    }
  }
  colnames(df) <- c("id", "name", "start_date", "end_date", "duration", "is_critical", "pred_id")
  return(df)
}

# Produces a list to be handled by the graph
make_node_list <- function(map, all_ids){
  ids <- character()
  successor <- character()

  for(id in all_ids){
    exp <- sprintf("map$'%s'", id)
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


#' Generate the critical path for a collection of related tasks
#'
#' @param data A data frame of tasks with columns ID, name, duration, dependencies in that order.
#' Name of columns does not matter, only order.
#' @param gantt Boolean that specifies whether or not to produce a gantt chart of the results.
#' @param start_date Starting date for the project. Defaults to the current date.
#' @return A list of results. First element is the id's of the critical path.
#' Second element is a data frame representation of the results that can be passed to the 'gantt' function.
#' Third element is the gantt chart if 'gantt' argument is true.
#' @examples
#' # Use provided sample data
#' data <- taskdata1
#'
#' res <- critical_path(data, gantt = F)
#' @export
critical_path <- function(data, gantt = F, network = F, start_date = Sys.Date()){
  # all_tasks <- apply(data, 1, read_func)
  all_tasks <- list()
  for(i in 1:nrow(data)){
    id <- to_id(data[i, 1])
    name <- data[i, 2]
    duration <- data[i, 3]
    pred_id <- as.character(data[i, 4])
    new_Task <- Task$new(id, name, duration, pred_id)
    text <- sprintf("all_tasks <- c(all_tasks, '%s' = new_Task)", new_Task$id)
    eval(parse(text = text))
  }

  ids <- lapply(data[,1], to_id)
  invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))

  # Topologically sort the ids
  adj_list <- make_node_list(all_tasks, ids)
  graph <- graph_from_data_frame(adj_list)
  sorted_ids <- names(topo_sort(graph = graph))

  # Create source node
  start_succ_ids <- c()
  for(task in all_tasks){
    if(length(task$predecessor_id) == 0){
      task$predecessor_id <- "%id_source%"
      start_succ_ids <- c(start_succ_ids, task$id)
    }
  }

  # Create a sink node
  end_pred_ids <- ""
  for(task in all_tasks){
    if(length(task$successor_id) == 0){
      task$successor_id <- "%id_sink%"
      end_pred_ids <- paste(end_pred_ids, ',', task$id, sep = "")
    }
  }

  # Add start task and end task to task list
  start_task <- Task$new("%id_source%", "%id_source%", 0, "")
  start_task$successor_id <- start_succ_ids
  end_task <- Task$new("%id_sink%", "%id_sink%", 0, end_pred_ids)

  all_tasks <- c("%id_source%" = start_task, all_tasks, "%id_sink%" = end_task)
  new_ids <- c("%id_source%", sorted_ids, "%id_sink%")

  # Perform the walk ahead
  walk_ahead(all_tasks, new_ids, start_date)

  # Perform the walk back
  walk_back(all_tasks, new_ids)

  # Prepare the results list
  ret <- list()

  # Calculate the critical path
  c_path <- crit_path(new_ids, all_tasks)
  ret$critical_path <- c_path[c(-1, -length(c_path))]
  ret$results <- to_data_frame(all_tasks)

  # If gantt is true, add network diagram to results
  if(gantt){
    ret$gantt <- gantt2(ret)
  }

  ret$total_duration <- sum((ret$results)[(ret$results)$is_critical,]$duration)
  ret$end_date <- start_date + ret$total_duration
  ret$network <- graph

  # If network is true, add network diagram
  if(network){
    ret$network_diagram <- network_diagram(ret)
  }

  ret
}

#' Creates a Gantt chart of tasks in a project.
#'
#' @param df A data frame of tasks. This function is called by
#' 'critical_path' if the 'gantt' argument is true. This data frame can either be raw data
#' (i.e not from the 'critical_path' function) or can be the data residing in
#' the 'results' element from the return value of the 'critical_path' function.
#' If the data is raw, if must have columns "ID, name, duration, dependencies"
#' in that order. These columns need not be named but they must be in that order.
#' @param raw Boolean indicating if the data is raw of if it has already been processed
#' by the 'critical_path' function.
#' @param start_date Starting date for the project. Defaults to the current date.
#' @return A gantt chart for the tasks. If raw is false, then this gantt chart will
#' color the critical path elements.
#' @examples
#' # Use raw example data
#' data <- taskdata1
#' # Create a gantt chart using the raw data
#' gantt(data, raw = T)
#'
#' res <- critical_path(data)
#'
#' # Create a second gantt chart using the processed data
#' gantt(res$results, raw = F)
#'
#' @export
# Produce a gantt chart
gantt <- function(df, start_date = Sys.Date()){
  if(ncol(df) == 4){
    raw = T
  }else if(ncol(df) != 7){
    stop("Invalid input, raw input should have 4 columns, processed should have 7")
  }else{
    raw = F
  }

  if(raw){
    # all_tasks <- apply(data, 1, read_func)
    all_tasks <- list()
    for(i in 1:nrow(data)){
      id <- to_id(data[i, 1])
      name <- data[i, 2]
      duration <- data[i, 3]
      pred_id <- as.character(data[i, 4])
      new_Task <- Task$new(id, name, duration, pred_id)
      text <- sprintf("all_tasks <- c(all_tasks, '%s' = new_Task)", new_Task$id)
      eval(parse(text = text))
    }

    ids <- lapply(data[,1], to_id)
    invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))

    # Topologically sort the ids
    adj_list <- make_node_list(all_tasks, ids)
    graph <- graph_from_data_frame(adj_list)
    new_ids <- names(topo_sort(graph = graph))

    # Create source node
    start_succ_ids <- c()
    for(task in all_tasks){
      if(length(task$predecessor_id) == 0){
        task$predecessor_id <- "%id_source%"
        start_succ_ids <- c(start_succ_ids, task$id)
      }
    }

    # Create a sink node
    end_pred_ids <- ""
    for(task in all_tasks){
      if(length(task$successor_id) == 0){
        task$successor_id <- "%id_sink%"
        end_pred_ids <- paste(end_pred_ids, ',', task$id, sep = "")
      }
    }

    # Add start task and end task to task list
    start_task <- Task$new("%id_source%", "%id_source%", 0, "")
    start_task$successor_id <- start_succ_ids
    end_task <- Task$new("%id_sink%", "%id_sink%", 0, end_pred_ids)

    all_tasks <- c("%id_source%" = start_task, all_tasks, "%id_sink%" = end_task)
    new_ids <- c("%id_source%", new_ids, "%id_sink%")

    # Perform the walk ahead
    walk_ahead(all_tasks, new_ids, start_date)
  }

  # Get dates in expected format
  df$start_date <- as.Date(df$start_date, format = "%m/%d/%Y")
  df$color <- " "

  # Assign colors based on critical path
  for(i in 1:nrow(df)){
    if(df$is_critical[i] == TRUE){
      df$color[i] <- "#f4424b"
    }else{
      df$color[i] <- "#41a9f4"
    }
  }

  # Create plotly object
  p <- plotly::plot_ly(type="scatter", mode = "lines")
  # Populate gantt chart
  for(i in 1:(nrow(df))){
    p <- plotly::add_trace(p,
                           mode = "lines",
                           x = c(df$start_date[i], df$start_date[i] + df$duration[i]),  # x0, x1
                           y = c(i, i),  # y0, y1
                           line = list(color = df$color[i], width = 30),
                           showlegend = F,
                           hoverinfo = "none"
    )
    text <- paste("Task: ", df$name[i], "<br>",
                  "Duration: ", df$duration[i], "days<br>",
                  "Dependencies: ", gsub("id", "", df$pred_id[i]), "<br>")
    p <- plotly::add_annotations(p,
                                 text = text,
                                 x = c(df$start_date[i] + df$duration[i] + 2),
                                 y = i,
                                 showarrow = F,
                                 font = list(size = 10)
    )
  }

  if(raw){
    p <- plotly::layout(p,
                        title = "Gantt Chart",
                        yaxis = list(title = "Task IDs",
                                     tickmode = "array",
                                     tickvals = 1:nrow(df),
                                     ticktext = df$id,
                                     showgrid = F,
                                     autorange = "reversed"),
                        xaxis = list(title = "Dates",
                                     showgrid = F)
    )
  }else{
    duration <- sum(df[df$is_critical, ]$duration)
    end_date <- start_date + duration
    text <- paste("Total Duration: ",duration, "days<br>",
                  "End Date: ", end_date)

    p <- plotly::layout(p,
                        title = "Gantt Chart",
                        yaxis = list(title = "Task IDs",
                                     tickmode = "array",
                                     tickvals = 1:nrow(df),
                                     ticktext = df$id,
                                     showgrid = F,
                                     autorange = "reversed"),
                        xaxis = list(title = "Dates",
                                     showgrid = F),
                        annotations = list(
                          list(text = text,
                               xref = "paper", yref = "paper",
                               x = 0.80, y = 0.9,
                               ax = 0, ay = 0,
                               align = "left")
                        )
    )
  }

  return(p)
}

gantt2 <- function(df, start_date = Sys.Date()){
  raw = F

  # LOOK AT THESE CONDITIONS
  if(length(ncol(df) > 0)){
    if(ncol(df) == 4){
      raw = T
    }
  }else if(is.null(df$results)){
    stop("Invalid input, raw input should have 4 columns, processed should have 7")
  }else if(ncol(df$results) != 7){
    stop("Invalid input, raw input should have 4 columns, processed should have 7")
  }

  if(!raw){
    df <- df$results
    if(df[1, "is_critical"] == TRUE){
      cols <- c("#41a9f4", "#f4424b")
    }else{
      cols <- c("#f4424b", "#41a9f4")
    }
  }

  # Make dependency labels
  deps <- c()
  for(i in 1:nrow(df)){
    row <- df[i, ]
    if(trimws(row$pred_id) == ""){
      deps <- c(deps, "")
    }else{
      deps <- c(deps, paste("Depends:\n", gsub(" ", ", ", trimws(row$pred_id))))
    }
  }

  dfr <- data.frame(
    name        = factor(df$id, levels = rev(df$id)),
    start.date  = as.Date(c(df$start_date)),
    end.date    = as.Date(c(df$end_date)),
    critical = c(df$is_critical),
    deps = deps
  )

  mdfr <- gather(dfr, measure.vars = c("start.date", "end.date"))

  # Make labels only show up once
  for(i in 1:(nrow(mdfr)/2)){
    mdfr[i, "deps"] <- ""
  }

  duration <- sum(df[df$is_critical, ]$duration)
  end_date <- start_date + duration

  p <- ggplot(mdfr, aes(value, name)) +
    geom_line(aes(colour = critical), size = 8) +
    geom_text(aes(label = deps), hjust = 0, nudge_x = 0.05, size = 3) +
    xlab(NULL) +
    ylab("Task ID") +
    scale_x_date(limits = c(start_date, end_date + 1)) +
    theme(legend.position = "none")

  if(!raw){
    p <-  p +scale_color_manual(values = cols)
  }

  p
}

network_diagram <- function(df){
  raw = F

  # LOOK AT THESE CONDITIONS
  if(length(ncol(df) > 0)){
    if(ncol(df) == 4){
      raw = T
    }
  }else if(is.null(df$results)){
    stop("Invalid input, raw input should have 4 columns, processed should have 7")
  }else if(ncol(df$results) != 7){
    stop("Invalid input, raw input should have 4 columns, processed should have 7")
  }

  if(raw){
    all_tasks <- list()
    for(i in 1:nrow(data)){
      id <- to_id(data[i, 1])
      name <- data[i, 2]
      duration <- data[i, 3]
      pred_id <- as.character(data[i, 4])
      new_Task <- Task$new(id, name, duration, pred_id)
      text <- sprintf("all_tasks <- c(all_tasks, '%s' = new_Task)", new_Task$id)
      eval(parse(text = text))
    }

    ids <- lapply(data[,1], to_id)
    invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))

    # Topologically sort the ids
    adj_list <- make_node_list(all_tasks, ids)
    graph <- graph_from_data_frame(adj_list)
    sorted_ids <- names(topo_sort(graph = graph))

    # Network diagram
    graph <- simplify(graph)
    l <- layout.reingold.tilford(graph)
    l[, c(1,2)] <- l[ ,c(2,1)]
    l[, 1] <- -l[, 1]

    # Clean up network coordinates, making sure a node and its dependency are not on the same level
    for(i in 1:length(sorted_ids)){
      for(j in i:length(sorted_ids)){
        if(get.edge.ids(graph, c(i,j)) > 0 && l[i, 1] == l[j, 1]){
          l[j, 1] <- l[j, 1] + 0.4
          break
        }
      }
    }
    V(graph)$color <- "#41a9f4"
    plot(graph, layout = l, vertex.shape = "rectangle", vertex.size = 20, vertex.size2 = 15, edge.arrow.size = 0.75)

  }
  else{
    res <- df$results
    graph <- simplify(df$network)
    l <- layout.reingold.tilford(graph)
    l[, c(1,2)] <- l[ ,c(2,1)]
    l[, 1] <- -l[, 1]

    for(i in 1:length(V(graph))){
      id <- V(graph)[[i]]$name
      if(res[res$id == id, "is_critical"]){
        V(graph)$color[i] <- "#f4424b"
      }else{
        V(graph)$color[i] <- "#41a9f4"
      }
    }

    # Clean up network coordinates, making sure a node and its dependency are not on the same level
    for(i in 1:length(sorted_ids)){
      for(j in i:length(sorted_ids)){
        if(get.edge.ids(graph, c(i,j)) > 0 && l[i, 1] == l[j, 1]){
          l[j, 1] <- l[j, 1] + 0.4
          break
        }
      }
    }

    plot(graph, layout = l, vertex.shape = "rectangle", vertex.size = 20, vertex.size2 = 15, edge.arrow.size = 0.75)
  }
  p <- recordPlot()
  p
}

# Main Method Code --------------------------------------------------------

data <- as.data.frame(read.csv("C:/Users/Student/Box Sync/R Critical Path/data/Book1.csv", header = F))
# all_tasks <- apply(data, 1, read_func)
all_tasks <- list()
for(i in 1:nrow(data)){
  id <- to_id(data[i, 1])
  name <- data[i, 2]
  duration <- data[i, 3]
  pred_id <- as.character(data[i, 4])
  new_Task <- Task$new(id, name, duration, pred_id)
  text <- sprintf("all_tasks <- c(all_tasks, '%s' = new_Task)", new_Task$id)
  eval(parse(text = text))
}

ids <- lapply(data[,1], to_id)
invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))

# Topologically sort the ids
adj_list <- make_node_list(all_tasks, ids)
graph <- graph_from_data_frame(adj_list)
sorted_ids <- names(topo_sort(graph = graph))

res <- critical_path(data, gantt = T, network = T)
# # gantt2(res$results)
# # gantt(res$results, F)
#
# # Network diagram
# graph <- simplify(graph)
# l <- layout.reingold.tilford(graph)
# l[, c(1,2)] <- l[ ,c(2,1)]
# l[, 1] <- -l[, 1]
# data2 <- res$results
#
# for(i in 1:length(V(graph))){
#   id <- V(graph)[[i]]$name
#   if(data2[data2$id == id, "is_critical"]){
#     V(graph)$color[i] <- "#f4424b"
#   }else{
#     V(graph)$color[i] <- "#41a9f4"
#   }
# }
#
# # Clean up network coordinates, making sure a node and its dependency are not on the same level
# for(i in 1:length(sorted_ids)){
#   for(j in i:length(sorted_ids)){
#     if(get.edge.ids(graph, c(i,j)) > 0 && l[i, 1] == l[j, 1]){
#       l[j, 1] <- l[j, 1] + 0.4
#       break
#     }
#   }
# }
#
# plot(graph, layout = l, vertex.shape = "rectangle", vertex.size = 20, vertex.size2 = 15, edge.arrow.size = 0.75)
