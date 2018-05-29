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
#'   \item{distribution}{Distrubtion for the uncertain task (currently, 
#'   "triangle", "uniform", and "normal" are supported}
#'   \item{X}{Depends on the distribution. If "triangle" or "uniform" then this value should be
#'   the minimum end time for the task. If "normal" then this value is the mean end time for the project
#'   (all values measured in days)}
#'   \item{X.1}{Depends on the distribution. If "triangle" or "uniform" then this value should be
#'   the maximum end time for the task. If "normal" then this value is the standard deviation of
#'    end time for the project (all values measured in days)}
#'   \item{X.2}{If "triangle" then this value is the most likely value for the end
#'   time (measured in days)}
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
#'   \item{distribution}{Distrubtion for the uncertain task (currently, 
#'   "triangle", "uniform", and "normal" are supported}
#'   \item{X}{Depends on the distribution. If "triangle" or "uniform" then this value should be
#'   the minimum end time for the task. If "normal" then this value is the mean end time for the project
#'   (all values measured in days)}
#'   \item{X.1}{Depends on the distribution. If "triangle" or "uniform" then this value should be
#'   the maximum end time for the task. If "normal" then this value is the standard deviation of
#'    end time for the project (all values measured in days)}
#'   \item{X.2}{If "triangle" then this value is the most likely value for the end
#'   time (measured in days)}
#' }
#'
#'
"taskdatauncertain2"