# Class Defintion ---------------------------------------------------------

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
      current_task$start_date <- start_date
    }else{
      for(id in current_task$predecessor_id){
        exp <- sprintf("map$'%s'", id)
        pred_task <- eval(parse(text = exp))
        if(is.null(pred_task)){
          stop("Invalid predeccessor id. Using a predeccessor id for a task that does not exist.")
        }
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
