rm(list=ls())

library(tidyverse)
library(haven)
library(gtsummary)
library(tidyr)
library(dplyr)

setwd("~/Downloads/seminar practice")

# Seminar 1 (https://uclspp.github.io/PUBL0050/5-panel-data-and-difference-in-differences.html)
#.........................................
load("~/Downloads/seminar practice/dinas_golden_dawn.Rdata")
#.........................................
# 1st question Answer
#..........................................................

# Subset the data for the post-treatment period (2016)
post_treatment_data <- subset(muni, year == 2016)

# Run the regression
model <- lm(gdvote ~ factor(treatment), data = post_treatment_data)

# Summarize the results
summary(model)

# The coefficient 2.7448 reflects the difference in vote shares in 2016 between treated and untreated municipalities. 
# It does not account for pre-treatment differences or potential confounding factors, so it does not represent the true ATT. 
# To estimate the ATT, a difference-in-differences analysis using data from both pre- and post-treatment periods would be required.



#.........................................
# 2nd question Answer
#..........................................................

# Subset the data for 2015 and 2016
data_2015_2016 <- subset(muni, year %in% c(2015, 2016))

# Calculate group-wise means using ever_treated for identifying treated municipalities
mean_treated_2015 <- mean(data_2015_2016$gdvote[data_2015_2016$ever_treated == TRUE & data_2015_2016$year == 2015], na.rm = TRUE)
mean_treated_2016 <- mean(data_2015_2016$gdvote[data_2015_2016$ever_treated == TRUE & data_2015_2016$year == 2016], na.rm = TRUE)

mean_untreated_2015 <- mean(data_2015_2016$gdvote[data_2015_2016$ever_treated == FALSE & data_2015_2016$year == 2015], na.rm = TRUE)
mean_untreated_2016 <- mean(data_2015_2016$gdvote[data_2015_2016$ever_treated == FALSE & data_2015_2016$year == 2016], na.rm = TRUE)

# Calculate the differences
diff_treated <- mean_treated_2016 - mean_treated_2015
diff_untreated <- mean_untreated_2016 - mean_untreated_2015

# Calculate the Difference-in-Differences
DiD <- diff_treated - diff_untreated

# Print results
cat("Mean Treated 2015:", mean_treated_2015, "\n")
cat("Mean Treated 2016:", mean_treated_2016, "\n")
cat("Mean Untreated 2015:", mean_untreated_2015, "\n")
cat("Mean Untreated 2016:", mean_untreated_2016, "\n")
cat("Difference-in-Differences Estimate:", DiD, "\n")



#.........................................
# 3rd question Answer
#..........................................................

# Subset the data for 2015 and 2016
data_2015_2016 <- subset(muni, year %in% c(2015, 2016))

# Create a post-treatment dummy variable
data_2015_2016$post_treatment <- ifelse(data_2015_2016$year == 2016, 1, 0)

# convert ever_treated to a factor 
data_2015_2016$ever_treated <- as.factor(data_2015_2016$ever_treated)

# Run the regression with the interaction term
model_did <- lm(gdvote ~ ever_treated + post_treatment + ever_treated * post_treatment, data = data_2015_2016)
summary(model_did)

# The Difference-in-Differences estimate (ATT) is 2.1236, indicating that the treatment (refugee influx) increased 
# the Golden Dawn vote share in treated municipalities by about 2.12 percentage points compared to untreated municipalities. 
# This effect is statistically significant





#.........................................
# 4th question Answer
#..........................................................

# The parallel trends assumption is a crucial condition for the validity of the Difference-in-Differences (DiD) methodology. 
# It assumes that, in the absence of the treatment, the treated and untreated groups would have followed the same trend over time in terms of the outcome variable.



#.........................................
# 5th question Answer
#..........................................................

# Aggregate the data to calculate the mean gdvote by year and treatment status
agg_data <- aggregate(gdvote ~ year + ever_treated, data = muni, FUN = mean)

# Plot the results
library(ggplot2)

ggplot(agg_data, aes(x = year, y = gdvote, color = factor(ever_treated))) +
  geom_line(size = 1) +
  labs(
    title = "Average Golden Dawn Vote Share Over Time",
    x = "Year",
    y = "Golden Dawn Vote Share (%)",
    color = "Treatment Status"
  ) +
  scale_color_manual(values = c("red", "blue"), labels = c("Control", "Treatment")) +
  theme_minimal()

# Since the parallel trends assumption seems reasonable, you can proceed confidently with your Difference-in-Differences analysis, 
# interpreting the treatment effect (e.g., the increase in Golden Dawn's vote share) as a causal effect of the refugee influx on treated municipalities.


#.........................................
# 6th question Answer
#..........................................................
# Create the post_time variable (1 if year is 2016, 0 otherwise)
muni$post_time <- ifelse(muni$year == 2016, 1, 0)

muni$ever_treated <- as.factor(muni$ever_treated)
muni$treatment <- as.factor(muni$treatment)

library(plm)
library(stargazer)
# Run the fixed-effects regression with two-way fixed effects (municipality and year)
FEmodel <- plm(gdvote ~ ever_treated * post_time + as.factor(municipality) + as.factor(year), 
               data = muni,
               index = c("municipality", "year"), 
               model = "within",  # for fixed effects
               effect = "twoways")  # to include both individual (state) and time (year) effects

stargazer(FEmodel, type='text')



#.........................................
# 7th question Answer 
#..........................................................
# Run the fixed-effects regression with two-way fixed effects (unit and time)
model_continuous <- plm(gdvote ~ trarrprop * post_time + as.factor(municipality) + as.factor(year),
                        data = muni, 
                        index = c("municipality", "year"),
                        model = "within",
                        effect = "twoways")
summary(model_continuous)

# The estimated average treatment effect on the treated (ATT) using the trarrprop variable is 0.606 percentage points.
# This means that, for each one-unit increase in the number of refugees per capita, the Golden Dawn vote share is expected 
# to increase by 0.606 percentage points in municipalities that were exposed to higher refugee inflows.





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



