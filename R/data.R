#' Small collection of sample tasks

#'
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{id}{id for the task}
#'   \item{name}{name of the task}
#'   \item{duration}{duration of task, in days}
#'   \item{pred}{id for the task's predecessors as a comma separated string}
#' }
#'
"taskdata1"

#' Larger collection of sample tasks with hanging tasks

#'
#' @format A data frame with 14 rows and 4 variables:
#' \describe{
#'   \item{id}{id for the task}
#'   \item{name}{name of the task}
#'   \item{duration}{duration of task, in days}
#'   \item{pred}{id for the task's predecessors as a comma separated string}
#' }
#'
#'
"taskdata2"

#' Larger collection of sample tasks with uncertainty in task duration

#'
#' @format A data frame with 14 rows and 7 variables:
#' \describe{
#'   \item{id}{id for the task}
#'   \item{name}{name of the task}
#'   \item{duration}{duration of task, in days}
#'   \item{pred}{id for the task's predecessors as a comma separated string}
#'   \item{min}{minimum duration of task, in days}
#'   \item{mode}{most likely duration of task, in days}
#'   \item{max}{maximum duration of task, in days}
#' }
#'
#'
"taskdatauncertain1"

#' Larger collection of sample tasks with greater uncertainty in tasks

#'
#' @format A data frame with 14 rows and 7 variables:
#' \describe{
#'   \item{id}{id for the task}
#'   \item{name}{name of the task}
#'   \item{duration}{duration of task, in days}
#'   \item{pred}{id for the task's predecessors as a comma separated string}
#'   \item{min}{minimum duration of task, in days}
#'   \item{mode}{most likely duration of task, in days}
#'   \item{max}{maximum duration of task, in days}
#' }
#'
#'
"taskdatauncertain2"