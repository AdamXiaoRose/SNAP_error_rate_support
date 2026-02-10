#plotting code written by Claude
library(knitr)
library(kableExtra)
library(gt)
library(yardstick)  # tidymodels package for precision-recall


model_data <- subset(model_data, rawben_rel_max<=1)
model_data_new <- model_data %>% select(-c(rescaled_weight, total_error_amount))
model_data_new <- model_data_new %>%
  mutate(row_id = row_number())
names(model_data_new)

data_split <- initial_split(model_data_new, prop = 0.75, strata = payment_error_i)
train_data <- training(data_split)
test_data <- testing(data_split)

# Keep race/ethnicity in test data but don't use in modeling
train_data_for_model <- train_data %>% select(-c(race_ethnicity, row_id))
test_data_for_model <- test_data %>% select(-race_ethnicity)
test_data_race <- test_data %>% select(row_id, race_ethnicity)
names(test_data_for_model)
# Fit model (without race/ethnicity)

rf_fit <- rf_spec %>%
  fit(payment_error_i ~ ., data = train_data_for_model)

rf_fit$fit$variable.importance

rf_fit_parsnip <- extract_fit_parsnip(rf_fit$fit)
vip(rf_fit_parsnip, num_features = 100)
# Make predictions
predictions <- predict(rf_fit, test_data_for_model, type = "prob") %>%
  bind_cols(predict(rf_fit, test_data_for_model)) %>%
  bind_cols(test_data %>% select(row_id, payment_error_i, race_ethnicity))

# Now analyze bias by race/ethnicity
predictions %>%
  group_by(race_ethnicity) %>%
  summarise(
    n = n(),
    avg_pred_prob = mean(.pred_Yes),
    error_rate = mean(payment_error_i=="Yes"),
    accuracy = mean(.pred_class == payment_error_i)
  )


predictions <- predictions %>% mutate(pred_adjusted = case_when( 
  .pred_Yes>.583 ~ "Yes",
  TRUE ~ "No"
))

predictions$pred_adjusted
error_rates <- predictions %>%
  mutate(
    TP = (payment_error_i == "Yes" & pred_adjusted == "Yes"),
    FP = (payment_error_i == "No" & pred_adjusted == "Yes"),
    TN = (payment_error_i == "No" & pred_adjusted == "No"),
    FN = (payment_error_i == "Yes" & pred_adjusted == "No")
  ) %>%
  group_by(race_ethnicity) %>%
  summarise(
    # Counts
    FP_count = sum(FP),
    FN_count = sum(FN),
    total_negative = sum(FP) + sum(TN),
    total_positive = sum(FN) + sum(TP),
    
    # Rates
    FPR = FP_count / total_negative,
    FNR = FN_count / total_positive,
    
    # Confidence intervals using binom.test
    FPR_lower = if_else(total_negative > 0, 
                        binom.test(FP_count, total_negative)$conf.int[1], 
                        NA_real_),
    FPR_upper = if_else(total_negative > 0, 
                        binom.test(FP_count, total_negative)$conf.int[2], 
                        NA_real_),
    FNR_lower = if_else(total_positive > 0, 
                        binom.test(FN_count, total_positive)$conf.int[1], 
                        NA_real_),
    FNR_upper = if_else(total_positive > 0, 
                        binom.test(FN_count, total_positive)$conf.int[2], 
                        NA_real_),
    n = n()
  )



# Reshape for plotting
error_rates_long <- error_rates %>%
  pivot_longer(cols = c(FPR, FNR),
               names_to = "error_type",
               values_to = "rate") %>%
  mutate(
    error_type = recode(error_type,
                        FPR = "False Positive Rate",
                        FNR = "False Negative Rate"),
    lower = ifelse(error_type == "False Positive Rate", FPR_lower, FNR_lower),
    upper = ifelse(error_type == "False Positive Rate", FPR_upper, FNR_upper),
    # Create label with race/ethnicity and n
    race_short = recode(race_ethnicity,
                        "American Indian or Alaska Native" = "AIAN"),
    race_label = paste0(race_short, "\n(", n, ")")
  )

# Plot with error bars
ggplot(error_rates_long, aes(x = race_label, y = rate, fill = error_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  labs(title = "False Positive and False Negative Rates by Race/Ethnicity in Held-Out Data",
       subtitle = NULL,
       x = NULL,
       y = NULL,
       fill = "", 
       caption = "Predictions vs. actual in held-out test data (47,357 cases, 3,757 with errors).") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+ ylim(0,10)

table(predictions$payment_error_i)
ggsave(filename="RF_error_vs_flag_by_raceeth_hypothetical.png", device="png", width = 8, height = 5, units = "in", dpi=300)

# Simple formatted table
conf_matrix_detailed <- predictions %>%
  mutate(
    actual = factor(payment_error_i, levels = c("No", "Yes")),
    predicted = factor(.pred_class, levels = c("No", "Yes"))
  ) %>%
  count(actual, predicted) %>%
  group_by(actual) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = sprintf("%d (%.1f%%)", n, pct)
  ) %>%
  ungroup() %>%
  select(actual, predicted, label) %>%
  pivot_wider(names_from = predicted, values_from = label, values_fill = "0 (0.0%)") %>%
  mutate(actual = recode(actual, "No" = "No Error", "Yes" = "Error"))


conf_matrix_detailed %>%
  gt() %>%
  tab_header(
    title = "Confusion Matrix: Model Prediction > .583",
    subtitle = "Predicted vs. Actual Payment Errors"
  ) %>%
  cols_label(
    actual = "Actual",
    No = "Pred: No Error",
    Yes = "Pred: Error"
  ) %>%
  cols_align(
    align = "center",
    columns = c(No, Yes)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = actual)
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14
  )

ggsave(filename="PR_conf_matrix_RF.png", device="png", width = 4, height = 2,5, units = "in", dpi=300)

# Method 1: Using yardstick (cleaner for tidymodels workflow)
pr_curve_data <- predictions %>%
  pr_curve(truth = payment_error_i, .pred_Yes, event_level = "second")

# Plot with ggplot
ggplot(pr_curve_data, aes(x = recall, y = precision)) +
  geom_line(color = "#D55E00", size = 1) +
  geom_hline(yintercept = mean(predictions$payment_error_i == "Yes"), 
             linetype = "dashed", color = "gray") +
  annotate("text", x = 0.165, y = mean(predictions$payment_error_i == "Yes") + 0.05,
           label = paste0("Baseline = ", 
                          round(mean(predictions$payment_error_i == "Yes"), 3)),
           size = 3.5, , fontface = "bold") +
  labs(
    x = "Recall (Sensitivity)",
    y = "Precision",
    title = "Precision-Recall Curve"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_equal() +
  xlim(0, 1) + ylim(0, 1) + annotate("text", x = 0.43, y = 0.2,
                                     label = paste0("Area Under Curve (Precision-Recall) = ", round(pr_auc$.estimate, 3)),
                                     size = 3.5, fontface = "bold")

ggplot(pr_curve_data, aes(x = recall, y = precision)) +
  geom_line(color = "#D55E00", size = 1) +
  geom_hline(yintercept = mean(predictions$payment_error_i == "Yes"), 
             linetype = "dashed", color = "gray") +
  annotate("text", x = 0.165, y = mean(predictions$payment_error_i == "Yes") + 0.05,
           label = paste0("Baseline = ", 
                          round(mean(predictions$payment_error_i == "Yes"), 3)),
           size = 3.5, fontface = "bold") +
  # Add dashed gray circle around the point
  annotate("point", x = 0.389, y = 0.918, 
           size = 10, shape = 21, color = "gray", fill = NA, stroke = 1) +
  # Add the point itself
  annotate("point", x = 0.389, y = 0.918, 
           size = 1, color = "black") +
  # Add label for threshold (closer)
  annotate("text", x = 0.389 + 0.06, y = 0.920, 
           label = "threshold = 0.583", 
           hjust = 0, size = 3.5) +
  labs(
    x = "Recall (Sensitivity)",
    y = "Precision",
    title = "Precision-Recall Curve"
  ) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  coord_equal() +
  xlim(0, 1) + ylim(0, 1) + 
  annotate("text", x = 0.47, y = 0.2, 
           label = paste0("Area Under Curve (Precision-Recall) = ", round(pr_auc$.estimate, 3)),
           size = 3.5, fontface = "bold")
# Calculate Area Under Precision-Recall Curve (AUPRC)
pr_auc <- predictions %>%
  pr_auc(truth = payment_error_i, .pred_Yes, event_level = "second")

cat("AUPRC:", pr_auc$.estimate, "\n")


ggsave(filename="PRAUC_RF.png", device="png", width = 4, height = 4, units = "in", dpi=300)

# Find the point closest to your target (recall ~0.33, precision ~0.90)
target_point <- pr_curve_data %>%
  mutate(
    distance = sqrt((recall - 0.33)^2 + (precision - 0.90)^2)
  ) %>%
  arrange(distance) %>%
  slice(1)

print(target_point)

nearby_points <- pr_curve_data %>%
  filter(recall >= 0.3 & recall <= 0.4,
         precision >= 0.85 & precision <= 0.95) %>%
  arrange(desc(precision))
