#' Generate the critical path for a collection of related tasks
#'
#' @param df A data frame of tasks with columns ID, name, duration, id's of predecessrs (as a comma separated string)
#' in that order. Name of columns does not matter, only order. Type 'taskdata1' into the console for an example of valid data.
#' @param gantt Boolean that specifies whether or not to produce a gantt chart of the results.
#' @param network Boolean that specifies whether or not to produce a network diagram of the results
#' @param start_date Starting date for the project. Defaults to the current date.
#' @return A list of results.
#'
#' \itemize{
#' \item \strong{critical_path} The id's of the critical path.
#' \item \strong{results} A data frame representation of the results that can be passed to the 'gantt' function.
#' \item \strong{gantt} Gantt chart if 'gantt' argument is true.
#' \item \strong{duration} The duration of the project in days.
#' \item \strong{end_date} The end date of the project.
#' \item \strong{network} Network diagram if 'network' argument true.
#' }
#' @examples
#' # Use provided sample data
#' df <- taskdata1
#'
#' res <- critical_path(df)
#'
#' @export
critical_path <- function(df, gantt = F, network = F, start_date = Sys.Date()){
  start_date <- as.Date(start_date)
  data <- df[, 1:4]
  all_tasks <- list()
  for(i in 1:nrow(data)){
    id <- to_id(data[i, 1])
    name <- data[i, 2]
    if(as.numeric(data[i,3]) < 0){
      stop("Durations must be non-negative")
    }
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
  graph <- igraph::graph_from_data_frame(adj_list)
  sorted_ids <- names(igraph::topo_sort(graph = graph))

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
    ret$gantt <- gantt(ret, start_date = start_date)
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
#' @param df Task input. This can either be a data frame of raw data
#' (i.e not from the 'critical_path' function) or can the return value from calling
#' the 'critical path' function.
#' If the data is raw, if must have columns "ID, name, duration, dependencies"
#' in that order. These columns need not be named but they must be in that order.
#' Type 'taskdata1' into the console for an example of raw data.
#' @param start_date Starting date for the project. Defaults to the current date.
#' @return A gantt chart for the tasks. If data has been processed by the critical path function,
#' then this gantt chart will color the critical path elements.
#' @examples
#' # Use raw example data
#' data <- taskdata1
#' # Create a gantt chart using the raw data
#' gantt(data)
#'
#' res <- critical_path(data)
#'
#' # Create a second gantt chart using the processed data
#' gantt(res)
#'
#' @export
gantt <- function(df, start_date = Sys.Date()){
  # DOES NOTHING, Used to suppress note in check
  DoesNothing <- R6::R6Class()
  
  # ACTUAL CODE STARTS HERE
  raw = F

  # Ensure start date type is correct
  start_date <- as.Date(start_date)

  # LOOK AT THESE CONDITIONS
  if(length(ncol(df) > 0)){
    if(ncol(df) == 4){
      raw = T

      # Do necesary preprocessing

      data <- df
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
      graph <- igraph::graph_from_data_frame(adj_list)
      sorted_ids <- names(igraph::topo_sort(graph = graph))

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
      walk_ahead(all_tasks, new_ids, start_date = start_date)

      df <- to_data_frame(all_tasks)
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

  mdfr <- tidyr::gather(dfr, measure.vars = c("start.date", "end.date"))

  # Make labels only show up once
  for(i in 1:(nrow(mdfr)/2)){
    mdfr[i, "deps"] <- ""
  }

  if(raw){
    p <- ggplot2::ggplot(mdfr, ggplot2::aes(mdfr$value, mdfr$name)) +
      ggplot2::geom_line(colour = "#41a9f4", size = 8) +
      ggplot2::geom_text(ggplot2::aes(label = mdfr$deps), hjust = 0, nudge_x = 0.05, size = 3) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab("Task ID")
  }else{
    duration <- sum(df[df$is_critical, ]$duration)
    end_date <- as.Date(start_date) + duration

    p <- ggplot2::ggplot(mdfr, ggplot2::aes(mdfr$value, mdfr$name)) +
      ggplot2::geom_line(ggplot2::aes(colour = mdfr$critical), size = 8) +
      ggplot2::geom_text(ggplot2::aes(label = mdfr$deps), hjust = 0, nudge_x = 0.05, size = 3) +
      ggplot2::xlab(NULL) +
      ggplot2::ylab("Task ID") +
      ggplot2::scale_x_date(limits = c(start_date, end_date + 1)) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_color_manual(values = cols)
  }

  p
}

#' Creates a network diagram of tasks in a project.
#'
#' @param df Task input. This can either be a data frame of raw data
#' (i.e not from the 'critical_path' function) or can the return value from calling
#' the 'critical path' function.
#' If the data is raw, if must have columns "ID, name, duration, dependencies"
#' in that order. These columns need not be named but they must be in that order.
#' Type 'taskdata1' into the console for an example of raw data.
#' @return A network diagram for the tasks. If data has been processed by the critical path function,
#' then this network diagram will color the critical path elements.
#' @examples
#' # Use raw example data
#' data <- taskdata1
#' # Create a network diagram chart using the raw data
#' network_diagram(data)
#'
#' res <- critical_path(data)
#'
#' # Create a second network diagram using the processed data
#' network_diagram(res)
#'
#' @export
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
    data <- df
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
    graph <- igraph::graph_from_data_frame(adj_list)
    sorted_ids <- names(igraph::topo_sort(graph = graph))

    # Network diagram
    graph <- igraph::simplify(graph)
    l <- igraph::layout.reingold.tilford(graph)
    l[, c(1,2)] <- l[ ,c(2,1)]
    l[, 1] <- -l[, 1]

    # Clean up network coordinates, making sure a node and its dependency are not on the same level
    for(i in 1:length(sorted_ids)){
      for(j in i:length(sorted_ids)){
        if(igraph::get.edge.ids(graph, c(i,j)) > 0 && l[i, 1] == l[j, 1]){
          l[j, 1] <- l[j, 1] + 0.4
          break
        }
      }
    }
    igraph::V(graph)$color <- "#41a9f4"
    graphics::plot(graph, layout = l, vertex.shape = "rectangle", vertex.size = 20, vertex.size2 = 15, edge.arrow.size = 0.65)

  }
  else{
    res <- df$results
    graph <- igraph::simplify(df$network)
    sorted_ids <- names(igraph::topo_sort(graph = graph))
    l <- igraph::layout.reingold.tilford(graph)
    l[, c(1,2)] <- l[ ,c(2,1)]
    l[, 1] <- -l[, 1]

    for(i in 1:length(igraph::V(graph))){
      id <- igraph::V(graph)[[i]]$name
      if(res[res$id == id, "is_critical"]){
        igraph::V(graph)$color[i] <- "#f4424b"
      }else{
        igraph::V(graph)$color[i] <- "#41a9f4"
      }
    }

    # Clean up network coordinates, making sure a node and its dependency are not on the same level
    for(i in 1:length(sorted_ids)){
      for(j in i:length(sorted_ids)){
        if(igraph::get.edge.ids(graph, c(i,j)) > 0 && l[i, 1] == l[j, 1]){
          l[j, 1] <- l[j, 1] + 0.4
          break
        }
      }
    }

    graphics::plot(graph, layout = l, vertex.shape = "rectangle", vertex.size = 20,
         vertex.size2 = 15, edge.arrow.size = 0.65)
  }
  p <- grDevices::recordPlot()
  p
}
