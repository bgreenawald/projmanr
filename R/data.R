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

#' Sample task with uncertain durations

#'
#' @format A data frame with 14 rows and 7 variables:
#' \describe{
#'   \item{id}{id for the task}
#'   \item{name}{name of the task}
#'   \item{duration}{duration of task, in days}
#'   \item{preds}{id for the task's predecessors as a comma separated string}
#'   \item{min}{minimum duration in days for the task}
#'   \item{mode}{most likely duration in days for the task}
#'   \item{max}{maximum duration in days for the task}
#' }
#'
#'
"taskdatauncertain1"
