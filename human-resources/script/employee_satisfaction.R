
library(readr)
library(dplyr)
library(caTools)
library(ggplot2)

# create an employees df
employees <- read_csv("hr.csv", col_types = "ddiiiiiicc") %>%
  mutate(sales = factor(sales, level = c("sales","technical","support","IT",
                                         "product_mng","marketing","RandD",
                                         "accounting","hr","management")),
         salary = factor(salary, level = c("low","medium","high"))) %>%
  rename("average_monthly_hours" = average_montly_hours)

# split data into a training + test set
set.seed(1)
split = sample.split(employees$left, SplitRatio = 0.6)
train <- employees %>%
  filter(split == TRUE)
test <- employees %>%
  filter(split == FALSE)


# Observations from Exploratory Analysis
#
#
# 1. Correlation between Variables
# 
# train %>%
#   select(satisfaction_level, last_evaluation, number_project, average_monthly_hours,
#          time_spend_company, Work_accident, left, promotion_last_5years) %>%
#   cor
# 
# Results:
#   satisfaction_level & left -> -0.39166
#
#
#
# 2. Histogram of satisfaction_level
#
# ggplot(train, aes(x = satisfaction_level)) +
# geom_histogram(binwidth = 0.05, fill = "dark grey", color = "black") +
# scale_x_continuous(breaks = 0:10 * 0.1)
#
# Results:
#      high # of low 
#
#
# 3. People who Left ~ sales 
#
# train %>%
#   mutate(sales = factor(sales, levels = c("hr","technical","sales","accounting",
#                                           "marketing","support","product_mng",
#                                           "IT","RandD","management"))) %>%
#   group_by(sales) %>%
#   summarize(left = sum(left),
#             total = n(),
#             prop_left = left/total * 100) %>%
#   ggplot(aes(x = sales, y = prop_left, label = round(prop_left,1))) +
#   geom_point(color = "dark blue", size = 5) +
#   geom_text(nudge_y = 0.5) + 
#   labs(x = "Department", y = "% that Left")
#
# Results:
#   hr - 28% left -> department with highest turnover rate
#
# 
#
# 4. People who Left ~ salary
#
# train %>%
#   group_by(salary) %>%
#   summarize(left = sum(left), 
#             total = n(),
#             prop_left = left/total * 100) %>%
#   ggplot(aes(x = salary, y = prop_left, label = round(prop_left,1))) + 
#   geom_point(color = "dark red", size = 5) +
#   geom_text(nudge_y = 1) +
#   ylab("% that Left")
#
# Results:
#   low salary - 29.7% left
#   medium salary - 20.2% left
#   high salary - 7% left


