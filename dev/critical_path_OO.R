library(R6)
library(hash)
library(plotly)
library(igraph)

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
  # if(is.character(id)){
  #   return(sprintf("id%s", id))
  # }else{
  #   return(sprintf("id%d", id))
  # }
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
walk_ahead <- function(tasks, map, ids, start_date = Sys.Date()){

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
        if(current_task$early_start < pred_task$early_finish){
          current_task$early_start <- pred_task$early_finish
          current_task$start_date <- pred_task$start_date + pred_task$duration
        }
      }
    }
    current_task$early_finish <- current_task$early_start + current_task$duration
  }
}

# Function to walk back
walk_back <- function(tasks, map, ids){

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
crit_path <- function(tasks, ids, map){
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
                   duration <- double(),
                   is_critical <- logical(),
                   pred_id <- character())

  for(task in tasks){
    df <- rbind(df, data.frame(id <- task$id,
                               name <- task$name,
                               start_date <- task$start_date,
                               duration <- task$duration,
                               is_critical <- task$is_critical,
                               pred_id <- paste(c(task$predecessor_id, " "), collapse = " "))
    )
  }
  colnames(df) <- c("id", "name", "start_date", "duration", "is_critical", "pred_id")
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
critical_path <- function(data, gantt = F, start_date = Sys.Date()){
  all_tasks <- apply(data, 1, read_func)
  ids <- lapply(data[,1], to_id)
  invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))


  # Create hash map for the tasks
  map <- hash::hash(keys = c(ids), values = all_tasks)

  # Topologically sort the ids
  adj_list <- make_node_list(map, ids)
  graph <- graph_from_data_frame(adj_list)
  new_ids <- names(topo_sort(graph = graph))

  walk_ahead(all_tasks, map, new_ids, start_date)
  walk_back(all_tasks, map, new_ids)
  ret <- list()
  ret$critical_path <- crit_path(all_tasks, new_ids, map)
  if(gantt){
    ret$results <- to_data_frame(all_tasks)
    ret$gantt <- gantt(as.data.frame(list(ret$results)), raw = F)
  }else{
    ret$results <- to_data_frame(all_tasks)
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
gantt <- function(df, raw = T, start_date = Sys.Date()){
  if(raw){
    all_tasks <- apply(df, 1, read_func)
    ids <- lapply(df[,1], to_id)
    invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))

    # Create hash map for the tasks
    map <- hash::hash(keys = c(ids), values = all_tasks)
    walk_ahead(all_tasks, map, start_date)
    df <- to_data_frame(all_tasks)
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



# Main Method Code --------------------------------------------------------


data <- as.data.frame(read.csv("C:/Users/Student/Box Sync/R Critical Path/data/Book1.csv", header = F))

all_tasks <- apply(data, 1, read_func)
ids <- lapply(data[,1], to_id)
invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))


# Create hash map for the tasks
map <- hash(keys = c(ids), values = all_tasks)

# Topologically sort the ids
adj_list <- make_node_list(map, ids)
graph <- graph_from_data_frame(adj_list)
new_ids <- names(topo_sort(graph = graph))

# Algorithm
walk_ahead(all_tasks, map, new_ids)
walk_back(all_tasks, map, new_ids)
res <- crit_path(all_tasks, new_ids, map)
df <- to_data_frame(all_tasks)
gantt(df, F)

# Network diagram
graph <- simplify(graph)
l <- layout_as_tree(graph)
plot(graph, layout=l)
l[, c(1,2)] <- l[ ,c(2,1)]
l[, 1] <- -l[, 1]
V(graph)$color[1] <- "red"
V(graph)$color[2] <- "blue"
plot(graph, layout = l)
