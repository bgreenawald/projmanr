#' Generate the critical path for a collection of related tasks
#'
#' @param data A data frame of tasks with columns ID, name, duration, dependencies in that order.
#' Name of columns does not matter, only order.
#' @param gantt Boolean that specifies whether or not to produce a gantt chart of the results.
#' @return A list of results. First element is the id's of the critical path.
#' Second element is a data frame representation of the results that can be passed to the 'gantt' function.
#' Third element is the gantt chart if 'gantt' argument is true.
#' @export
critical_path <- function(data, gantt = F){
  all_tasks <- apply(data, 1, read_func)
  ids <- lapply(data[,1], to_id)
  invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))


  # Create hash map for the tasks
  map <- hash::hash(keys = c(ids), values = all_tasks)

  walk_ahead(all_tasks, map)
  walk_back(all_tasks, map)
  ret <- list()
  ret$critical_path <- crit_path(all_tasks)
  if(gantt){
    ret$results <- to_data_frame(all_tasks)
    ret$gantt <- gantt(as.data.frame(list(ret$results)), raw = F)
  }else{
    ret$results <- to_data_frame(all_tasks)
  }
  ret
}

#' Creates a Gantt chart of tasks in a project. This function is called by
#' 'critical_path' if the 'gantt' argument is true.
#'
#' @param df A data frame of tasks. This data frame can either be raw data
#' (i.e not from the 'critical_path' function) or can be the data residing in
#' the 'results' element from the return value of the 'critical_path' function.
#' If the data is raw, if must have columns "ID, name, duration, dependencies"
#' in that order. These columns need not be named but they must be in that order.
#' @param raw Boolean indicating if the data is raw of if it has already been processed
#' by the 'critical_path' function.
#' @return A gantt chart for the tasks. If raw is false, then this gantt chart will
#' color the critical path elements.
#' @export
# Produce a gantt chart
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
  p <- plotly::plot_ly(type="scatter", mode="lines")
  # Populate gantt chart
  for(i in 1:(nrow(df) - 1)){
    p <- plotly::add_trace(p,
                           x = c(df$start_date[i], df$start_date[i] + df$duration[i]),  # x0, x1
                           y = c(i, i),  # y0, y1
                           line = list(color = df$color[i], width = 20),
                           showlegend = F,
                           hoverinfo = "text",

                           # Create custom hover text

                           text = paste("Task: ", df$name[i], "<br>",
                                        "Duration: ", df$duration[i], "days<br>")
    )
  }

  return(p)
}

