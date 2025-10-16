# This script examines the predictive power of polling averages generated for previous
# US Senate races, 2006-2024. The polls were scraped from RealClearPolling with a
# Python script. A relatively simple weighting scheme was used (just two weights for
# each poll: sqrt(sample size)/sqrt(median sample size for other polls in the average)
# plus an exponential decay for the age of the poll.)

# First: libraries

library(dplyr)
library(ggplot2)
library(tidyr)
library(modelsummary)

# Import the data

polling_and_results <- read.csv("previous_2week_averages_with_actuals.csv")

# Right now, we just have weighted averages for each candidate. Let's combine them
# to get margins.

# Create polling margins:

polling_and_results <- polling_and_results %>%
  mutate(rep_lead = case_when(weighted_dem >= weighted_other ~ weighted_rep - weighted_dem, weighted_dem < weighted_other ~ weighted_rep - weighted_other))

polling_and_results <- polling_and_results %>%
  mutate(rep_lead = case_when(is.na(weighted_dem) ~ weighted_rep - weighted_other, is.na(weighted_other) ~ weighted_rep - weighted_dem))

# Final margins:

polling_and_results <- polling_and_results %>%
  mutate(rep_lead_actual = case_when(dem_actual >= other_actual ~ rep_actual - dem_actual, dem_actual < other_actual ~ rep_actual - other_actual))

# Now a simple linear regression: are polls usually related to results?

simple_reg <- lm(rep_lead_actual ~ rep_lead, polling_and_results)

summary(simple_reg)

# Naturally, they're related. This is true even though we haven't specified which dates
# to use.

# Now we're going to see how polls relate to outcomes over time. To do this, we'll
# generate the polling error for each day and then graph how this error changes
# over time. Note that errors here are absolute.

polling_and_results <- polling_and_results %>%
  mutate(polling_error = abs(rep_lead_actual - rep_lead))

ggplot(polling_and_results, aes(x = days_before_election, y = polling_error)) +
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Polling Error Over Time",
    x = "Days before election",
    y = "Polling error"
  ) +
  theme_minimal() + scale_x_reverse()

# You can see in "Plots" that the polling error gets closer and closer to zero
# as election day draws near. Accuracy really begins to kick in around 100 days
# before the election.

# Let's check errors by year with polling data from one week before the election.

polling_1week <- polling_and_results %>%
  filter(days_before_election == 7)

polling_1week %>%
  group_by(election_year) %>%
  summarize(
    mean_error = mean(polling_error, na.rm = TRUE),
    median_error = median(polling_error, na.rm = TRUE),
  )

# 2014 is the outlier here. If you check, there were some pretty big (but
# fairly unnoticeable) errors that year. In Arkansas, Tom Cotton was
# underestimated by 10 points:
# (https://www.realclearpolling.com/polls/senate/general/2014/arkansas/cotton-vs-pryor)
# I'm including a link only so you can verify the sample is correct.
# 2006 was also very weird, since the typical error was about 0.
 
# Let's see if we can generate a rule of thumb for how to interpret
# polling margins as odds of victory. First, we need a dummy variable for
# whether the Republican candidate won.

polling_and_results <- polling_and_results %>%
  mutate(rep_won = case_when(rep_lead_actual > 0 ~ 1, rep_lead_actual < 0 ~ 0))

polling_1week <- polling_1week %>%
  mutate(rep_won = case_when(rep_lead_actual > 0 ~ 1, rep_lead_actual < 0 ~ 0))

# First, a linear probability model:

lpm <- lm(rep_won ~ rep_lead, polling_1week)

summary(lpm)

# It seems we should assume candidates are equal in odds if they're dead
# even in the polls, and add 2.4 points to their odds for every point they're
# ahead in the polls.

# Next, a probit model, since it should provide a better fit:

probit <- glm(rep_won ~ rep_lead, family=binomial(link = "probit"), polling_1week)

summary(probit)

# Another option: use modelsummary for a nicer-looking table in the viewer

modelsummary(list(simple_reg, probit), 
             stars = TRUE,
             output = "default")

# This next bit of code is entirely from ChatGPT and visualizes our probit model.
# Lots of mundane tasks can be completed by LLMs like this.

# Create a new dataset covering the range of rep_lead values
newdata <- data.frame(rep_lead = seq(min(polling_1week$rep_lead, na.rm = TRUE),
                                     max(polling_1week$rep_lead, na.rm = TRUE),
                                     length.out = 100))

# Predict probabilities from the probit model
newdata$predicted_prob <- predict(probit, newdata, type = "response")

# Plot actual data and fitted curve
ggplot(polling_1week, aes(x = rep_lead, y = rep_won)) +
  geom_point(alpha = 0.3) +
  geom_line(data = newdata, aes(y = predicted_prob), color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "Probit Model: Probability Republican Wins vs. Republican Lead (60 Days Out)",
       x = "Republican Lead in Polls",
       y = "Predicted Probability Republican Wins")

# END OF GPT CODE

# It looks like our previous estimate was very poor. Between a lead of 0 and a
# lead of 10, we should move from 50% odds to 99% odds. So the better rule of
# thumb isn't 2.4 points, but 5!

# Let's take a look at how polling errors change over time by year.
polling_and_results %>%
  filter(days_before_election <= 200) %>%
  ggplot(aes(x = days_before_election, y = polling_error)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~election_year, scales="free") +
    theme_minimal() + scale_x_reverse()

# Things look noisier now, but the same pattern emerges almost every year.

# Finish by cleaning up the data and exporting it.

polling_and_results <- polling_and_results %>%
  filter(!(is.na(weighted_rep) & is.na(weighted_dem) & is.na(weighted_other)))

write.csv(polling_and_results, "clean_polling_data.csv", row.names = FALSE)
