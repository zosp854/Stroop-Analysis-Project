#create data frame
df <- data.frame (
subject <- (1:30), 
age <- sample(1:100, 30, replace = TRUE),
gender <- sample(c("male", "female"), 30, replace = TRUE),
mean_RT <- sample(200:6000, 30, replace = TRUE),
depression <- sample(0:100, 30, replace = TRUE),
mean_sleep_time <- sample(2:12, 30, replace = TRUE)
)

source("../functions.R")
create_summary (df,1,30)

