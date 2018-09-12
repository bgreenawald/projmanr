# Class Defintion ---------------------------------------------------------
# This class is used to represent a "task" in our R program.
#' @importFrom R6 R6Class
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
                      initialize = function(id = NA, name = NA,
                                            duration = NA,
                                            predecessor_id = NA){
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


# Function to handle reading of predecessor ids
# ensuring that we have a consitent id format
# by removing whitespace and removing null ids
proc_ids <- function(ids){
  ids <- strsplit(ids, ",")
  ids <- lapply(ids, trimws)
  ids <- ids[[1]][ids[[1]] != ""]
  return(list(ids))
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

  # For each task we have, check and see if the current
  # task exists in its list of predecessors, and if it
  # does, add it to the current task's list of successors
  for (cur_task in full_tasks) {
    if (task_id %in% unlist(cur_task$predecessor_id)) {
      ret_ids <- c(ret_ids, cur_task$id)
    }
  }
  task$successor_id <- ret_ids
  return(NULL)
}

# Implementation of the 'walk ahead' portion of the
# critical path algorithm
walk_ahead <- function(map, ids, start_date = Sys.Date()){
  # Perform the walk ahead for each task in the project.
  # It is assumed at this point that the ids have been sorted
  # 'chronologically' in which each task id appears before any
  # succesor task id.
  for (cur in ids) {
    # Get the task corresponding to the current id
    current_task <- map[[cur]]

    # If our task has no predecessors, we can set the start date
    # to whatever was input
    if (length(current_task$predecessor_id) == 0) {
      current_task$start_date <- start_date
    }
    # If we do have predecessors, find the one that finishes last
    else{
      # The first element is the baseline
      max_task <- map[[current_task$predecessor_id[1]]]
      
      # Iterate over all predecessors
      for (id in current_task$predecessor_id) {
        pred_task <- map[[id]]
        if (is.null(pred_task)) {
          stop(paste("Invalid predeccessor id. Using a predeccessor",
               "id for a task that does not exist."))
        }
        # If the current predecessors after before the current
        # task starts, update the start values
        if (max_task$early_finish < pred_task$early_finish) {
          max_task <- pred_task
        }
        
        # Update the current task with the max early finish
        current_task$early_start <- max_task$early_finish
        current_task$start_date <- max_task$start_date +
          max_task$duration
      }
    }

    # After we have checked all of the predecessors, we can update the current
    # tasks early finish and end date
    current_task$early_finish <- current_task$early_start +
      current_task$duration
    current_task$end_date <- current_task$start_date +
      current_task$duration
  }
}

# Implement the 'walk back' portion of the algorithm
walk_back <- function(map, ids){
  # Again, iterate over each id, but this time in reverse
  # order. Now, a task is always going to show up before
  # any of its predecessors.
  for (cur in rev(ids)) {
    # Get the task corresponding to the current id
    current_task <- map[[cur]]

    # If we have no successors, we can update late finish
    # right away
    if (length(current_task$successor_id) == 0) {
      current_task$late_finish <- current_task$early_finish
    }
    # If we do have successors, we must find the one that
    # finishes earliest
    else{
      # Iterate over each successor task id
      for (id in current_task$successor_id) {
        succ_task <- map[[id]]

        # If the current task does not yet have a late
        # finish, assign it at the first task we find.
        if (current_task$late_finish == 0) {
          current_task$late_finish <- succ_task$late_start
        }else{
          # If the successor starts earlier than we finish,
          # we update our last finish time.
          if (current_task$late_finish > succ_task$late_start) {
            current_task$late_finish <- succ_task$late_start
          }
        }
      }
    }

    # After we have iterate, we can record the final late start value.
    current_task$late_start <- current_task$late_finish -
      current_task$duration
  }
}

# Implement the actual finding of critical path.
# Assuming both walk ahead and walk back have been computed.
crit_path <- function(ids, map){
  c_path <- NULL

  # For each task, check if it meets the requirements for critical path.
  for (id in ids) {
    task <- map[[id]]
    if (task$early_finish == task$late_finish &&
        task$early_start == task$late_start) {
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

  # Create a dataframe to hold the tasks.
  df <- data.frame(id <- character(),
                   name <- character(),
                   start_date <- double(),
                   end_date <- double(),
                   duration <- double(),
                   is_critical <- logical(),
                   pred_id <- character())

  # For each task, extract the necesary information and
  # add it to the dataframe.
  for (task in tasks) {
    if (task$id != "%id_source%" && task$id != "%id_sink%") {
      if (task$predecessor_id[1] == "%id_source%") {
        task$predecessor_id <- ""
      }
      df <- rbind(df, data.frame(id <- task$id,
                                 name <- task$name,
                                 start_date <- task$start_date,
                                 end_date <- task$end_date,
                                 duration <- task$duration,
                                 is_critical <- task$is_critical,
                                 pred_id <- paste(c(task$predecessor_id, " "),
                                 collapse = " "))
      )
    }
  }
  colnames(df) <- c("id", "name", "start_date",
                    "end_date", "duration", "is_critical", "pred_id")
  return(df)
}

# Produces a list to be handled by the graph
# so that the network may be visualized and
# tasks may be topologically sorted.
make_node_list <- function(map, all_ids){
  ids <- character()
  successor <- character()

  # Iterate over each task,
  # keeping track of the task and its
  # succesors.
  for (id in all_ids) {
    succ_task <- map[[id]]
    for (id2 in succ_task$successor_id) {
      ids <- c(ids, id)
      successor <- c(successor, id2)
    }
  }

  ret <- data.frame(id = ids,
                    successor = successor,
                    stringsAsFactors = FALSE)

  return(ret)
}

preprocess <- function(data) {
  # Create a list to hold the tasks
  all_tasks <- list()
  
  # For each row, or task, in the input, preprocess.
  for (i in 1:nrow(data)) {
    
    # Extract and process the id and name
    id <- to_id(data[i, 1])
    name <- data[i, 2]
    
    # Ensure durations are valid
    if (as.numeric(data[i, 3]) < 0) {
      stop("Durations must be non-negative")
    }
    duration <- data[i, 3]
    pred_id <- as.character(data[i, 4])
    
    # Create a new task object with the given info and
    # add it onto the task list
    new_Task <- Task$new(id, name, duration, pred_id)
    text <- sprintf("all_tasks <- c(all_tasks, '%s' = new_Task)", new_Task$id)
    eval(parse(text = text))
  }
  
  # Create a separate list of ids
  ids <- lapply(data[, 1], to_id)
  
  # Calculate the successors for each task
  invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))
  
  # Topologically sort the ids
  adj_list <- make_node_list(all_tasks, ids)
  graph <- igraph::graph_from_data_frame(adj_list)
  sorted_ids <- names(igraph::topo_sort(graph = graph))
  
  # Make sure sorted IDs contains all ids, even those that would not
  # appear in the graph
  sorted_ids <- c(unlist(setdiff(ids, sorted_ids)), sorted_ids)
  
  # Create source node
  start_succ_ids <- c()
  for (task in all_tasks) {
    # If a task has no predecessors,
    # make it a successor of the source node
    if (length(task$predecessor_id) == 0) {
      task$predecessor_id <- "%id_source%"
      start_succ_ids <- c(start_succ_ids, task$id)
    }
  }
  
  # Create a sink node
  end_pred_ids <- ""
  for (task in all_tasks) {
    # If a task has no successors, make
    # it a successor of the sink node
    if (length(task$successor_id) == 0) {
      task$successor_id <- "%id_sink%"
      end_pred_ids <- paste(end_pred_ids, ",", task$id, sep = "")
    }
  }
  
  # Add start task and end task to task list
  start_task <- Task$new("%id_source%", "%id_source%", 0, "")
  start_task$successor_id <- start_succ_ids
  end_task <- Task$new("%id_sink%", "%id_sink%", 0, end_pred_ids)
  
  all_tasks <- c("%id_source%" = start_task, all_tasks, "%id_sink%" = end_task)
  new_ids <- c("%id_source%", sorted_ids, "%id_sink%")
  
  return(list(all_tasks, new_ids, graph))
}