#' Generate the critical path for a collection of related tasks
#'
#' @param df A data frame of tasks with columns ID, name, duration, id's of predecessrs (as a comma separated string)
#' in that order. Name of columns does not matter, only order.
#' @param gantt Boolean that specifies whether or not to produce a gantt chart of the results.
#' @param start_date Starting date for the project. Defaults to the current date.
#' @return A list of results.
#'
#' \itemize{
#' \item \strong{critical_path} The id's of the critical path.
#' \item \strong{results} A data frame representation of the results that can be passed to the 'gantt' function.
#' \item \strong{gantt} Gantt chart if 'gantt' argument is true.
#' \item \strong{duration} The duration of the project in days.
#' \item \strong{end_date} The end date of the project.
#' }
#' @examples
#' # Use provided sample data
#' df <- taskdata1
#'
#' res <- critical_path(df, gantt = F)
#' @export
critical_path <- function(df, gantt = F, start_date = Sys.Date()){
  all_tasks <- apply(df, 1, read_func)
  ids <- lapply(df[,1], to_id)
  invisible(lapply(all_tasks, get_successor, full_tasks = all_tasks))


  # Create hash map for the tasks
  map <- hash(keys = c(ids), values = all_tasks)

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
    ret$gantt <- gantt(as.data.frame(list(ret$results)))
  }else{
    ret$results <- to_data_frame(all_tasks)
  }

  ret$total_duration <- sum((ret$results)[(ret$results)$is_critical,]$duration)
  ret$end_date <- start_date + ret$total_duration
  ret
}

#' Creates a Gantt chart of tasks in a project.
#' @usage gantt(taskdata1)
#' @usage gantt(res$results)
#' @param df A data frame of tasks. This data frame can either be raw data
#' (i.e not from the 'critical_path' function) or can be the data residing in
#' the 'results' element from the return value of the 'critical_path' function.
#' If the data is raw, if must have columns "ID, name, duration, dependencies"
#' in that order. These columns need not be named but they must be in that order.
#' Type 'taskdata1' into the console for an example.
#' @param start_date Starting date for the project. Defaults to the current date.
#' @return A gantt chart for the tasks. If raw is false, then this gantt chart will
#' color the critical path elements.
#' @examples
#' # Use raw example data
#' data <- taskdata1
#' # Create a gantt chart using the raw data
#' gantt(data)
#'
#' res <- critical_path(data)
#'
#' # Create a second gantt chart using the processed data
#' gantt(res$results)
#'
#' @export
# Produce a gantt chart
gantt <- function(df, start_date = Sys.Date()){
  if(ncol(df) == 4){
    raw = T
  }else if(ncol(df) != 6){
    stop("Invalid input, raw input should have 4 columns, processed should have 6")
  }else{
    raw = F
  }

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
