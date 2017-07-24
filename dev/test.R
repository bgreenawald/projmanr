setwd("C:/Users/Student/Box Sync/R Critical Path")

data <- as.data.frame(read.csv("Book1.csv", header = FALSE))
colnames(data) <- c("id", "name", "duration", "predeccesors")

new_cols <- c("succesors", "early_start", "early_finish", "late_start", "late_finish", "is_critical")


all_tasks <- apply(data, 1, read_func)
ids <- lapply(data[,1], to_id)
lapply(all_tasks, get_successor, full_tasks = all_tasks)


# Create hash map for the tasks
map <- hash(keys = c(ids), values = all_tasks)

walk_ahead(all_tasks, map)
walk_back(all_tasks, map)
critical_path(all_tasks)