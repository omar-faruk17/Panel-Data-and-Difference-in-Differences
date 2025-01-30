rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)
library(tidyr)
library(dplyr)

# Seminar 2 (https://uclspp.github.io/PUBL0050/5-panel-data-and-difference-in-differences.html)
#.........................................
m_wage <- read_dta("m_wage.dta")

#.........................................
# 1th question Answer 
#..........................................................

# The difference-in-differences estimator is the difference between these two changes: 0.5−(−2.4)=2.9
# This suggests that the minimum wage increase in New Jersey led to an increase of 2.9 employees per restaurant, 
# relative to what would have happened in Pennsylvania without the wage change.


# Parallel trends assumption: The employment trends in New Jersey and Pennsylvania would have been the same in the absence of 
# the minimum wage change (i.e., the change in employment in Pennsylvania reflects the counterfactual scenario for New Jersey).

# Potential Biases: If New Jersey and Pennsylvania were on different employment trajectories prior to the wage change, 
# the parallel trends assumption could be violated. For example, if the fast-food industry in New Jersey was experiencing growth 
# while Pennsylvania was declining, the results would overstate the effect of the minimum wage increase.



#.........................................
# 2th question Answer 
#..........................................................
# Ensure nj is a factor
min_wage <- m_wage
min_wage$nj <- as.factor(min_wage$nj)

# Calculate the average wage in both periods for both NJ and PA
avg_wage_pre <- tapply(min_wage$wage_st, min_wage$nj, mean, na.rm = TRUE)
avg_wage_post <- tapply(min_wage$wage_st2, min_wage$nj, mean, na.rm = TRUE)

# Print the average wages for both periods
print(avg_wage_pre)
print(avg_wage_post)

# Calculate the DiD estimate
delta_nj <- avg_wage_post["1"] - avg_wage_pre["1"]  # New Jersey (nj = 1)
delta_pa <- avg_wage_post["0"] - avg_wage_pre["0"]  # Pennsylvania (nj = 0)

did_estimate <- delta_nj - delta_pa
print(did_estimate)

# The DiD estimate of 0.481 suggests that the minimum wage increase in New Jersey resulted in an average wage increase of about 0.481 units (presumably dollars) 
# compared to Pennsylvania, which did not experience the wage increase.
# This suggests that the minimum wage policy in New Jersey was effective in raising the average wage in the state.



#.........................................
# 3th question Answer 
#..........................................................

# Calculate the average number of employees before and after treatment for NJ and PA
avg_emptot_pre <- tapply(min_wage$emptot, min_wage$nj, mean, na.rm = TRUE)
avg_emptot_post <- tapply(min_wage$emptot2, min_wage$nj, mean, na.rm = TRUE)

# Print the averages
print(avg_emptot_pre)
print(avg_emptot_post)

# Calculate the changes
delta_nj_emp <- avg_emptot_post["1"] - avg_emptot_pre["1"]  # NJ change
delta_pa_emp <- avg_emptot_post["0"] - avg_emptot_pre["0"]  # PA change

# Calculate the DiD estimator
did_estimate_emp <- delta_nj_emp - delta_pa_emp
print(did_estimate_emp)

# The DiD estimate of 2.75 suggests that the minimum wage increase in New Jersey resulted in an increase of 
# approximately 2.75 full-time employees per restaurant, relative to Pennsylvania, which did not experience the wage increase.
# This indicates that the minimum wage policy may have had a positive effect on employment in New Jersey.

# If the Parallel trends were similar before the policy change, this assumption holds.

# Combine the data into a single data frame for plotting
state <- c("Pennsylvania", "New Jersey", "Pennsylvania", "New Jersey")
time <- c("Pre-Treatment", "Pre-Treatment", "Post-Treatment", "Post-Treatment")
avg_emptot <- c(avg_emptot_pre[1], avg_emptot_pre[2], avg_emptot_post[1], avg_emptot_post[2])

data_plot <- data.frame(state, time, avg_emptot)

# Plot the trends
library(ggplot2)

ggplot(data_plot, aes(x = time, y = avg_emptot, color = state, group = state)) +
  geom_line(size = 1.2) +  # Line for average number of employees
  geom_point(size = 3) +    # Points for averages
  labs(title = "Average Number of Full-Time Employees in NJ and PA",
       x = "Time",
       y = "Average Number of Full-Time Employees",
       color = "State") +
  theme_minimal()
# The graph shows that the parallel trend assumption is violated.



#.........................................
# 4th question Answer 
#..........................................................
# Calculate the average price of a meal in both periods for NJ and PA
avg_pmeal_pre <- tapply(min_wage$pmeal, min_wage$nj, mean, na.rm = TRUE)
avg_pmeal_post <- tapply(min_wage$pmeal2, min_wage$nj, mean, na.rm = TRUE)

# Print the averages
print("Average price of a meal in pre-treatment period:")
print(avg_pmeal_pre)
print("Average price of a meal in post-treatment period:")
print(avg_pmeal_post)

# Calculate the changes in the price of meals
delta_nj_price <- avg_pmeal_post["1"] - avg_pmeal_pre["1"]  # New Jersey (wage increase)
delta_pa_price <- avg_pmeal_post["0"] - avg_pmeal_pre["0"]  # Pennsylvania (no wage increase)

# Calculate the DiD estimator for meal prices
did_estimate_price <- delta_nj_price - delta_pa_price
print("Difference-in-Differences estimate for meal prices:")
print(did_estimate_price)

# The difference-in-differences (DiD) estimator for the price of a meal is 0.0794, which suggests that, 
# on average, restaurants in New Jersey (subject to the wage increase) raised their prices by about 0.08 USD more than restaurants in Pennsylvania 



#.........................................
# 5th question Answer 
#..........................................................

# Create data frame for pre-treatment period
pre_treatment <- min_wage[, c("nj", "emptot", "pmeal", "wage_st", "pmeal", "co_owned", "bk", "kfc", "wendys")]
pre_treatment$time <- 0  # Pre-treatment period indicator

# Create data frame for post-treatment period
post_treatment <- min_wage[, c("nj", "emptot2", "pmeal2", "wage_st2", "pmeal2", "co_owned", "bk", "kfc", "wendys")]
post_treatment$time <- 1  # Post-treatment period indicator

# Rename the columns in both data frames to have the same names for binding
colnames(pre_treatment) <- c("nj", "emptot", "pmeal", "wage_st", "pmeal", "co_owned", "bk", "kfc", "wendys", "time")
colnames(post_treatment) <- c("nj", "emptot", "pmeal", "wage_st", "pmeal", "co_owned", "bk", "kfc", "wendys", "time")

# Combine the two data frames into a long format
long_data <- rbind(pre_treatment, post_treatment)

# View the resulting data
head(long_data)

long_data$nj <- as.factor(long_data$nj) # 1 = New Jersey
long_data$time <- as.factor(long_data$time)

# Model 1: Difference-in-Differences without covariates
model_1 <- lm(emptot ~ time + nj + time * nj, data = long_data)
summary(model_1)


# Model 2: Difference-in-Differences with restaurant-level covariates
model_2 <- lm(emptot ~ time * nj + co_owned + bk + kfc + wendys + pmeal, data = long_data)
summary(model_2)

# No, the treatment effect estimate remains very similar, and it continues to be statistically insignificant. 
# Therefore, controlling for restaurant-level covariates does meaningfully change the results in this case.
# We do not have strong evidence to conclude that when the minimum wage increased, there was a significant decrease on employment levels in NJ.
# Rather, the result (model 2) indicates that the increase in minimum wage led to a significant rise on employment levels.
# This result is statistically significant at 5% level. The result is similar with the findings of the paper of Card and Krueger (1994)



