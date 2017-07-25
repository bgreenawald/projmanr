library(R6)
library(hash)
library(plotly)

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
                        self$predecessor_id <- proc_ids(predecessor_id)
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
  return(lapply(ids, to_id))
}

# Function to Convert input to R6 class
read_func <- function(x){
  id <- x[1]
  name <- x[2]
  duration <- x[3]
  pred_id <- x[4]
  new_Task <- Task$new(id, name, duration, pred_id)
}

# Convert numeric to id usable by the hash map
to_id <- function(id){
  id <- trimws(id)
  if(is.character(id)){
    return(sprintf("id%s", id))
  }else{
    return(sprintf("id%d", id))
  }
}

# Gets the successor for an activity
get_successor <- function(task, full_tasks){
  ret_ids <- NULL
  task_id <- task$id
  for(cur_task in full_tasks){
    if(task_id %in% cur_task$predecessor_id){
      ret_ids <- c(ret_ids, cur_task$id)
    }
  }
  task$successor_id <- ret_ids
  return(NULL)
}

# Function to walk ahead
walk_ahead <- function(tasks, map){

  number_activities <- length(tasks)

  for(i in 1:number_activities){
    current_task <- tasks[[i]]
    if(length(tasks[[i]]$predecessor_id) == 0){
      tasks[[i]]$early_finish <- tasks[[i]]$early_start + tasks[[i]]$duration
      tasks[[i]]$start_date <- Sys.Date()
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
walk_back <- function(tasks, map){
  number_activities <- length(tasks)

  tasks[[number_activities]]$late_finish <- tasks[[number_activities]]$early_finish

  for(i in number_activities:1){
    current_task <- tasks[[i]]
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
crit_path <- function(tasks){
  c_path <- NULL

  for(task in tasks){
    if(task$early_finish == task$late_finish && task$early_start == task$late_start){
      c_path <- c(c_path, gsub("id", "", task$id))
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
    df <- rbind(df, data.frame(id <- gsub("id", "", task$id),
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

gantt <- function(df, raw=T){
  if(raw){
    all_tasks <- apply(df, 1, read_func)
    ids <- lapply(df[,1], to_id)
    invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))

    # Create hash map for the tasks
    map <- hash::hash(keys = c(ids), values = all_tasks)
    walk_ahead(all_tasks, map)
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
                           showlegend = F
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

  p <- layout(p,
              title = "Gantt Chart",
              yaxis = list(title = "Task IDs",
                           tickmode = "array",
                           tickvals = 1:nrow(df),
                           ticktext = df$id,
                           showgrid = F),
              xaxis = list(title = "Dates",
                           showgrid = F)
              )

  return(p)
}

# Main Method Code --------------------------------------------------------


data <- as.data.frame(read.csv("C:/Users/Student/Box Sync/R Critical Path/data/Book1.csv", header = F))

all_tasks <- apply(data, 1, read_func)
ids <- lapply(data[,1], to_id)
invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))


# Create hash map for the tasks
map <- hash(keys = c(ids), values = all_tasks)

walk_ahead(all_tasks, map)
walk_back(all_tasks, map)
res <- crit_path(all_tasks)
df <- to_data_frame(all_tasks)
gantt(df, F)
