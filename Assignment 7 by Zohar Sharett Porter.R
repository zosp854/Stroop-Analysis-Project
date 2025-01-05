# R course for beginners
# Week 7
# assignment by Zohar Sharett Porter, id 208929539

#1

library(dplyr)
df <- data.frame()
file_names <- dir("stroop_data", full.names = TRUE)  
for (file in file_names) {
  temp_data <- read.csv(file)  
  df <- rbind(df, temp_data) 
}
names(df)

# יצירת משתנים task ו-congruency
df <- df %>%
  mutate(
    task = ifelse(grepl("word_reading_incong", condition), "Ink_Name", "Word_Read"),
    congruency = ifelse(grepl("incong", condition), "incongruent", "congruent"),
    acc = ifelse(participant_response == correct_response, 1, 0)
  )

# שמירת רק המשתנים הרלוונטיים
df <- df %>%
  select(subject, task, congruency, block, trial, acc, rt) %>%
  mutate(
    subject = as.factor(subject),
    task = as.factor(task),
    congruency = as.factor(congruency),
    block = as.numeric(block),
    trial = as.numeric(trial),
    acc = as.numeric(acc),
    rt = as.numeric(rt)
  )

# קידוד פקטורים
contrasts(df$task)<-c(1,0)
contrasts(df$task)
contrasts(df$congruency)<-c(1,0)
contrasts(df$congruency)

# שמירת הנתונים
save(df, file = "raw_data.RData")

#2
library(dplyr)

#מספר נבדקים
num_subjects <- df %>%
  summarise(num_subjects = n_distinct(subject)) %>%
  pull(num_subjects)

print(paste("Number of subjects:", num_subjects))

# הוצאות NA
df <- df %>%
  filter(!is.na(rt))

#  מעל 3 שניות או מתחת ל-0.3 שניות
df <- df %>%
  filter(!is.na(rt)) %>%
  filter(rt >= 300 & rt <= 3000)


#  אחוז הטריילים שנשארו אחרי ההוצאה
df <- df %>%
group_by(subject) %>%
  mutate(
   total_trials = 400,  # מספר הצעדים הכולל לנבדק
    remaining_trials = n(),  # מספר הצעדים שנשארו
    percent_remaining = (remaining_trials / total_trials) * 100
  )
df
#ממוצע וס"ת של אחוז הטריילים שנשארו
summary_stats <- df %>%
  summarise(
    mean_percent_remaining = mean(percent_remaining, na.rm = TRUE),
    sd_percent_remaining = sd(percent_remaining, na.rm = TRUE)
  )
summary_stats
save(df, file = "filtered_data.RData")

summary_data <- df %>%
  group_by(task, congruency) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    mean_acc = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

library(dplyr)
library(ggplot2)

# Calculate mean reaction time and accuracy by task
task_summary <- df %>%
  group_by(task) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    mean_acc = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate mean reaction time and accuracy by congruency
congruency_summary <- df %>%
  group_by(congruency) %>%
  summarise(
    mean_rt = mean(rt, na.rm = TRUE),
    mean_acc = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

# Bar plot for Reaction Time by Task
ggplot(task_summary, aes(x = task, y = mean_rt, fill = task)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Reaction Time by Task",
    x = "Task",
    y = "Mean Reaction Time (ms)",
    fill = "Task"
  ) +
  theme_minimal()

# Bar plot for Reaction Time by Congruency
ggplot(congruency_summary, aes(x = congruency, y = mean_rt, fill = congruency)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Reaction Time by Congruency",
    x = "Congruency",
    y = "Mean Reaction Time (ms)",
    fill = "Congruency"
  ) +
  theme_minimal()

