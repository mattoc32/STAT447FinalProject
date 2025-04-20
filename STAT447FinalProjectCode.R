baseball_data <- read.csv(
  "/Users/matthewocampo/desktop/STAT 447/STAT447FinalProject/BaseballStatsFull.csv")

library(readr)
library(dplyr)
library(ggplot2)
library(car)
library(rstan)
library(knitr)

set.seed(42)

# OLS Analysis
train_index <- sample(1:nrow(baseball_data), 0.7*nrow(baseball_data))
train_data <- baseball_data[train_index, ]
test_data  <- baseball_data[-train_index, ]
head(train_data)
model_slg <- lm(slg_percent ~ launch_angle_avg + exit_velocity_avg + barrel_batted_rate + batting_avg, train_data)


vif(model_slg)


test_data$predicted_slg_OLS <- predict(model_slg, newdata = test_data)


r2_OLS <- cor(test_data$slg_percent, test_data$predicted_slg_OLS)^2
rmse_OLS <- sqrt(mean((test_data$slg_percent - test_data$predicted_slg_OLS)^2))

cat("R-squared on test data:",r2_OLS, "\n")
cat("RMSE on test data:",rmse_OLS, "\n")

# Bayesian Analysis
set.seed(42)
stan_data <- list(
  N = nrow(train_data),
  x_launch = train_data$launch_angle_avg,
  x_batting_avg = train_data$batting_avg,
  x_barrel = train_data$barrel_batted_rate,
  x_exit_velo = train_data$exit_velocity_avg,
  y = train_data$slg_percent
)

informative_prior_model <- stan_model("slg.stan")

informative_prior_fit <- sampling(
  informative_prior_model,
  data = stan_data,
  iter = 2000,
  chains = 1,
  seed = 42
)

posterior_1 <- extract(informative_prior_fit)
mean_params <- sapply(posterior_1[c("intercept", "beta_launch", "beta_avg", "beta_barrel",
                             "beta_exit_velo")], mean)

test_data$predicted_slg_stan1 <- with(test_data,
                           mean_params["intercept"] +
                             mean_params["beta_launch"] * launch_angle_avg +
                             mean_params["beta_avg"] * batting_avg +
                             mean_params["beta_barrel"] * barrel_batted_rate +
                             mean_params["beta_exit_velo"] * exit_velocity_avg
)

head(test_data)

r2_stan1 <- cor(test_data$slg_percent, test_data$predicted_slg_stan1)^2
rmse_stan1 <- sqrt(mean((test_data$slg_percent - test_data$predicted_slg_stan1)^2))



r2_OLS
rmse_OLS
r2_stan1
rmse_stan1


informative_prior_model_2 <- stan_model("slg_2.stan")

informative_prior_fit_2 <- sampling(
  informative_prior_model_2,
  data = stan_data,
  iter = 2000,
  chains = 1,
  seed = 42
)

posterior_2 <- extract(informative_prior_fit_2)
mean_params_2 <- sapply(posterior_2[c("intercept", "beta_launch", "beta_avg", "beta_barrel",
                                    "beta_exit_velo")], mean)

test_data$predicted_slg_stan2 <- with(test_data,
                                      mean_params_2["intercept"] +
                                        mean_params_2["beta_launch"] * launch_angle_avg +
                                        mean_params_2["beta_avg"] * batting_avg +
                                        mean_params_2["beta_barrel"] * barrel_batted_rate +
                                        mean_params_2["beta_exit_velo"] * exit_velocity_avg
)

head(test_data)

r2_stan2 <- cor(test_data$slg_percent, test_data$predicted_slg_stan2)^2
rmse_stan2 <- sqrt(mean((test_data$slg_percent - test_data$predicted_slg_stan2)^2))

r2_stan2
rmse_stan2
rmse_OLS
rmse_stan1



# Posterior Predictive Calibration
y_rep <- posterior_1$y_rep

lower <- apply(y_rep, 2, quantile, 0.025)
upper <- apply(y_rep, 2, quantile, 0.975)


within_interval <- train_data$slg_percent >= lower & train_data$slg_percent <= upper
coverage <- mean(within_interval)

cat("Empirical 95% coverage:", round(coverage * 100, 1), "%\n")


coverage_df <- data.frame(
  Observation = 1:length(train_data$slg_percent),
  Slugging_Percentage = train_data$slg_percent,
  Lower = apply(y_rep, 2, quantile, 0.025),
  Upper = apply(y_rep, 2, quantile, 0.975)
)


coverage_df$Inside_CI <- with(coverage_df, Slugging_Percentage >= Lower & Slugging_Percentage <= Upper)


ggplot(coverage_df, aes(x = Observation, y = Slugging_Percentage, color = Inside_CI)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 0.7) +
  scale_color_manual(values = c("FALSE" = "#F8766D", "TRUE" = "#00BFC4")) +
  labs(
    y = "Slugging Percentage", 
    color = "Inside_CI"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right"
  )


posterior <- rstan::extract(fit)

n_draws <- length(posterior_1$intercept)
N_test <- nrow(test_data)

y_rep_test <- matrix(NA, nrow = n_draws, ncol = N_test)

for (i in 1:n_draws) {
  mu <- posterior_1$intercept[i] +
    posterior_1$beta_launch[i] * test_data$launch_angle_avg +
    posterior_1$beta_avg[i] * test_data$batting_avg +
    posterior_1$beta_barrel[i] * test_data$barrel_batted_rate +
    posterior_1$beta_exit_velo[i] * test_data$exit_velocity_avg 
  
  y_rep_test[i, ] <- rnorm(N_test, mean = mu, sd = posterior_1$sigma[i])
}

observed_data <- test_data$slg_percent
predicted_data <- as.vector(y_rep_test)

df_testdata <- data.frame(
  value = c(observed_data, predicted_data),
  group = rep(c("Observed", "Posterior Predictive"), c(length(observed_data), length(predicted_data)))
)

df_testdata

ggplot(df_testdata, aes(x = value, color = group)) +
  geom_density() +
  labs(x = "Slugging Percentage", y = "Density") +
  scale_color_manual(name = "Model Type", values = c("blue", "red")) +  # Change legend title and colors
  guides(color = guide_legend(title = "Custom Legend Title"))

mean_preds <- colMeans(y_rep_test)

test_data$residuals <- test_data$slg_percent - mean_preds
test_data$predicted_slg__means <- mean_preds

ggplot(test_data, aes(x = predicted_slg, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Posterior Predictive Residuals (Test Data)",
       x = "Predicted Slugging %", y = "Residuals") +
  theme_minimal()

y_rep_long <- as.data.frame(y_rep_test[1:100, ]) %>%
  mutate(draw = row_number()) %>%
  pivot_longer(
    cols = -draw,
    names_to = "obs",
    values_to = "value"
  ) %>%
  mutate(group = "Posterior Predictive")

observed_df <- data.frame(
  value = test_data$slg_percent,
  group = "Observed"
)

combined_df <- bind_rows(
  y_rep_long %>% select(value, group, draw),
  observed_df %>% mutate(draw = NA)
)

ggplot(combined_df, aes(x = value, color = group, group = interaction(group, draw))) +
  geom_density(alpha = 0.4, size = 0.4) +
  scale_color_manual(values = c("Posterior Predictive" = "skyblue", "Observed" = "black")) +
  labs(x = "Slugging Percentage", y = "Density", color = "Distribution Type",
       title = "Posterior Predictive Distributions vs. Observed") +
  theme_minimal()




# Comparisons of RMSE
rmse_table <- data.frame(
  Model = c("OLS Regression", "Bayesian Stan (Initial)", "Bayesian Stan (Improved)"),
  RMSE = c(rmse_OLS, rmse_stan1, rmse_stan2)
)

kable(rmse_table, format = "markdown", digits = 3, caption = "Comparison of RMSE Values")
