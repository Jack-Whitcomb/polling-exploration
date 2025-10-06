# This script examines the predictive power of polling averages generated for previous
# US Senate races, 2006-2024. The polls were scraped from RealClearPolling with a
# Python script. A relatively simple weighting scheme was used (just two weights for
# each poll: sqrt(sample size)/sqrt(median sample size for other polls in the average)
# plus an exponential decay for the age of the poll.)

# First: libraries

library(dplyr)
library(ggplot2)

# Import the data

polling_and_results <- read.csv("previous_2week_averages_with_actuals.csv")

# Right now, we just have weighted averages for each candidate. Let's combine them
# to get margins.

# Polling margins:

polling_and_results <- polling_and_results %>%
  mutate(rep_lead = case_when(weighted_dem >= weighted_other ~ weighted_rep - weighted_dem, weighted_dem < weighted_other ~ weighted_rep - weighted_other))

# Final margins:

polling_and_results <- polling_and_results %>%
  mutate(rep_lead_actual = case_when(dem_actual >= other_actual ~ rep_actual - dem_actual, dem_actual < other_actual ~ rep_actual - other_actual))

# Now a simple linear regression: are polls usually related to results?

simple_reg <- lm(rep_lead ~ rep_lead_actual, polling_and_results)

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
# before the election. Eventually, we seem to get stuck around a typical polling
# error of 5 points.

# Finally, let's see if we can generate a rule of thumb for how to interpret
# polling margins as odds of victory. First, we need a dummy variable for
# whether the Republican candidate won.

polling_and_results <- polling_and_results %>%
  mutate(rep_won = case_when(rep_lead_actual > 0 ~ 1, rep_lead_actual < 0 ~ 0))

# Now restrict the data to 7 days before the election.

polling_for_lpm <- polling_and_results %>%
  filter(days_before_election == 7)

# First, a linear probability model:

lpm <- lm(rep_won ~ rep_lead, polling_for_lpm)

summary(lpm)

# It seems we should assume candidates are dead even in odds if they're dead
# even in the polls, and add 2.4 points to their odds for every point they're
# ahead in the polls.

# Next, a probit model, since it should provide a better fit:

probit <- glm(rep_won ~ rep_lead, family=binomial(link = "probit"), polling_for_lpm)

summary(probit)

# The rest of this code is from ChatGPT and visualizes our probit model.

# Create a new dataset covering the range of rep_lead values
newdata <- data.frame(rep_lead = seq(min(polling_for_lpm$rep_lead, na.rm = TRUE),
                                     max(polling_for_lpm$rep_lead, na.rm = TRUE),
                                     length.out = 100))

# Predict probabilities from the probit model
newdata$predicted_prob <- predict(probit, newdata, type = "response")

# Plot actual data and fitted curve
ggplot(polling_for_lpm, aes(x = rep_lead, y = rep_won)) +
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