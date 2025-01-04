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
    task = ifelse(grepl("word_reading_incong", condition), "Ink", "Read"),
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
  filter(rt > 0.3 & rt < 3)

#  אחוז הטריילים שנשארו אחרי ההוצאה
df <- df %>%
  group_by(subject) %>%
  mutate(
    total_trials = n(),  # מספר הצעדים הכולל לנבדק
    remaining_trials = n(),  # מספר הצעדים שנשארו
    percent_remaining = (remaining_trials / total_trials) * 100
  )

#ממוצע וס"ת של אחוז הטריילים שנשארו
summary_stats <- df %>%
  summarise(
    mean_percent_remaining = mean(percent_remaining, na.rm = TRUE),
    sd_percent_remaining = sd(percent_remaining, na.rm = TRUE)
  )

save(df, file = "filtered_data.RData")

