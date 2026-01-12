#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(purrr)
library(forcats)


setwd("/Users/camillafeeley/Downloads/BGL/WorkingInR")
state_predictions <- read.csv("state_predictions.csv")

# DEDUCTION ERROR AT CCERTIFICATION
state_predictions_reduced <- state_predictions %>%
  group_by(other_states, 
           cert_HH_size_FS_n, 
           HH_head_LF_status_c, rawben_rel_max) %>%  # these should change based on variables in the chart
  summarize(
    .pred_yes = mean(`.pred_yes`)
  ) 

ggplot(state_predictions_reduced, aes(x = rawben_rel_max, y = `.pred_yes`, 
                                      color = other_states)) +
  facet_grid(HH_head_LF_status_c ~ cert_HH_size_FS_n) +
  geom_point(position = position_dodge(width = 0.02)) +
  scale_color_viridis_d() +
  labs(title = "Predicted Probability of Deduction Error at Cert by HH Characteristics: WA vs. Other States, 2017-2023",
       color = "Other States (F=WA)")

ggsave(filename="HH_Characteristics_WA_vs_others.png", device="png", width = 12, height = 12, units = "in", dpi=300)

# Difference Plot

# ---------------------------
# Bar chart
# ---------------------------
# Basic checks / cleaning
needed <- c("rawben_rel_max", "cert_HH_size_FS_n", "other_states", "HH_head_LF_status_c", ".pred_yes")
missing <- setdiff(needed, names(state_predictions))
if (length(missing) > 0) {
  stop("Missing required columns: ", paste(missing, collapse = ", "))
}


# Create WA group + bin rawben_rel_max
rng <- range(state_predictions$rawben_rel_max, na.rm = TRUE)

if (rng[1] >= 0 && rng[2] <= 1) {
  breaks <- seq(0, 1, by = 0.1)  # deciles
} else {
  breaks <- pretty(rng, n = 10)
}

plot_df <- state_predictions %>%
  mutate(
    wa_group = if_else(other_states, "Not WA", "WA"),
    hh_size  = cert_HH_size_FS_n,
    lf_status = HH_head_LF_status_c,
    rawben_bin = cut(rawben_rel_max, breaks = breaks, include.lowest = TRUE, right = TRUE)
  ) %>%
  filter(!is.na(rawben_bin), !is.na(hh_size), !is.na(lf_status), !is.na(wa_group), !is.na(.pred_yes)) %>%
  group_by(hh_size, lf_status, rawben_bin, wa_group) %>%
  summarise(
    pred_yes = mean(.pred_yes, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    rawben_bin = fct_inorder(as.factor(rawben_bin)),
    hh_size = as.factor(hh_size),
    lf_status = as.factor(lf_status)
  )

# Bar chart
p_bar <- ggplot(plot_df, aes(x = rawben_bin, y = pred_yes, fill = wa_group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_grid(
    lf_status ~ hh_size,
    labeller = labeller(
      hh_size = function(x) str_wrap(paste("Household size:", x), width = 18),
      lf_status = function(x) str_wrap(paste("LF status:", x), width = 18)
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("WA" = "#0072B2", "Not WA" = "#D55E00")
  ) +
  labs(
    title = "Predicted Probability of Deduction Error by Benefit Level (Binned)\nWA vs Not WA, by HH Size and LF Status",
    x = "Raw benefit relative to maximum (binned)",
    y = "Predicted probability",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.spacing = unit(1.0, "lines"),
    strip.text.y = element_text(size = 7),
    strip.text.x = element_text(size = 7)
  )

p_bar

# Same but line plot
get_mid <- function(bin_label) {
  nums <- stringr::str_extract_all(bin_label, "-?\\d+\\.?\\d*")[[1]]
  if (length(nums) < 2) return(NA_real_)
  mean(as.numeric(nums[1:2]))
}

plot_df_line <- plot_df %>%
  mutate(rawben_mid = purrr::map_dbl(as.character(rawben_bin), get_mid)) %>%
  filter(!is.na(rawben_mid))

p_line_mid <- ggplot(
  plot_df_line,
  aes(x = rawben_mid, y = pred_yes, color = wa_group, group = wa_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(lf_status ~ hh_size) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("WA" = "#0072B2", "Not WA" = "#D55E00")) +
  labs(
    title = "Predicted Probability of Deduction Error vs Benefit Level\nWA vs Not WA, by HH Size and LF Status",
    x = "Raw benefit relative to maximum (bin midpoint)",
    y = "Predicted probability",
    color = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing = unit(1.0, "lines"),
    strip.text.y = element_text(size = 7),
    strip.text.x = element_text(size = 7)
  )

p_line_mid


#-----------------------------------------
#  INCOME ERROR AT INITIAL CERTIFICATION - Eric's code
#-----------------------------------------
state_predictions$other_states <- as.factor(state_predictions$state_Oregon==0)

state_predictions_reduced <- state_predictions %>%
  group_by(other_states,
           cert_HH_size_FS_n,
           HH_head_LF_status_c, rawben_rel_max) %>%  # Need to track which row
  summarize(
    .pred_yes = mean(`.pred_yes`)
    #    .pred_lower = quantile(`.pred_yes`, 0.025),
    #    .pred_upper = quantile(`.pred_yes`, 0.975)
  )

ggplot(state_predictions_reduced, aes(x = rawben_rel_max, y = `.pred_yes`,
                                      color = other_states)) +
  facet_grid(HH_head_LF_status_c ~ cert_HH_size_FS_n) +
  geom_point(position = position_dodge(width = 0.02)) +
  #scale_color_viridis_d() +
  labs(title = "Predicted Probability of Income Error at Initial Cert by HH Characteristics
       and Benefit to Max: OR vs. Other States, 2017-2023",
       color = "Other States (F=OR)")

ggsave(filename="HH_Characteristics_OR_vs_others_by_incomeErrorsCounted_atCert.png", device="png", width = 12, height = 12, units = "in", dpi=300)


#-----------------------------------------
#  INCOME ERROR AT INITIAL CERTIFICATION - trying line plot
#-----------------------------------------

state_predictions$other_states <- as.factor(state_predictions$state_Oregon == 0)

# --- helper to get bin midpoint ---
get_mid <- function(bin_label) {
  nums <- stringr::str_extract_all(bin_label, "-?\\d+\\.?\\d*")[[1]]
  if (length(nums) < 2) return(NA_real_)
  mean(as.numeric(nums[1:2]))
}

# --- choose bins for rawben_rel_max ---
rng <- range(state_predictions$rawben_rel_max, na.rm = TRUE)
breaks <- if (rng[1] >= 0 && rng[2] <= 1) seq(0, 1, by = 0.1) else pretty(rng, n = 10)

# --- reduce to mean predicted prob within HH size x LF status x OR/Other x benefit-bin ---
state_predictions_reduced <- state_predictions %>%
  mutate(
    hh_size   = as.factor(cert_HH_size_FS_n),
    lf_status = as.factor(HH_head_LF_status_c),
    group_or  = if_else(other_states == TRUE, "Other states", "OR"),
    rawben_bin = cut(rawben_rel_max, breaks = breaks, include.lowest = TRUE, right = TRUE),
    rawben_mid = purrr::map_dbl(as.character(rawben_bin), get_mid)
  ) %>%
  filter(!is.na(hh_size), !is.na(lf_status), !is.na(group_or), !is.na(.pred_yes), !is.na(rawben_mid)) %>%
  group_by(hh_size, lf_status, group_or, rawben_bin, rawben_mid) %>%
  summarise(
    pred_yes = mean(.pred_yes, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#-----------------------------------------
# Plot: panels by HH size, trend lines by LF status
#   OR vs Other states shown with linetype
#-----------------------------------------

# this one doesn't help much
p_income_trends <- ggplot(
  state_predictions_reduced,
  aes(x = rawben_mid, y = pred_yes, color = lf_status)
) +
  geom_point(aes(shape = group_or), alpha = 0.55, size = 1.7) +
  geom_smooth(
    aes(linetype = group_or),
    method = "loess",
    span = 0.9,
    se = TRUE,
    linewidth = 1.1
  ) +
  facet_wrap(
    ~ hh_size,
    ncol = 1,
    labeller = labeller(hh_size = function(x) str_wrap(paste("Household size:", x), width = 18))
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Predicted Probability of Income Error at Initial Certification\nby Benefit Level, Employment Type, and Household Size (OR vs Other States), 2017–2023",
    x = "Raw benefit relative to maximum (bin midpoint)",
    y = "Predicted probability",
    color = "Employment type",
    linetype = "Group",
    shape = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing = unit(1.0, "lines"),
    axis.text.x = element_text(angle = 0),
    legend.position = "right"
  )

p_income_trends

ggsave(
  filename = "income_error_initial_cert_trends_OR_vs_others_byLF_byHHsize.png",
  plot = p_income_trends,
  device = "png",
  width = 12,
  height = 14,
  units = "in",
  dpi = 300
)

# FACET BY EMPLOYMENT TYPE, TOO
# helper to get bin midpoint
get_mid <- function(bin_label) {
  nums <- stringr::str_extract_all(bin_label, "-?\\d+\\.?\\d*")[[1]]
  if (length(nums) < 2) return(NA_real_)
  mean(as.numeric(nums[1:2]))
}

# OR vs others flag
state_predictions$other_states <- as.factor(state_predictions$state_Oregon == 0)

# bins for rawben_rel_max
rng <- range(state_predictions$rawben_rel_max, na.rm = TRUE)
breaks <- if (rng[1] >= 0 && rng[2] <= 1) seq(0, 1, by = 0.1) else pretty(rng, n = 10)

# reduced data
plot_df <- state_predictions %>%
  mutate(
    hh_size   = as.factor(cert_HH_size_FS_n),
    lf_status = as.factor(HH_head_LF_status_c),
    group_or  = if_else(other_states == TRUE, "Other states", "OR"),
    rawben_bin = cut(rawben_rel_max, breaks = breaks, include.lowest = TRUE, right = TRUE),
    rawben_mid = purrr::map_dbl(as.character(rawben_bin), get_mid)
  ) %>%
  filter(!is.na(hh_size), !is.na(lf_status), !is.na(group_or),
         !is.na(.pred_yes), !is.na(rawben_mid)) %>%
  filter(lf_status != "other") %>%   # <- drop "Other" category
  group_by(hh_size, lf_status, group_or, rawben_bin, rawben_mid) %>%
  summarise(
    pred_yes = mean(.pred_yes, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_income_trends <- ggplot(
  plot_df,
  aes(x = rawben_mid, y = pred_yes, color = group_or, group = group_or)
) +
  geom_point(alpha = 0.55, size = 1.6) +
  geom_smooth(
    method = "loess",
    span = 0.9,
    se = TRUE,
    linewidth = 1.1
  ) +
  facet_grid(
    lf_status ~ hh_size,
    labeller = labeller(
      lf_status = function(x) stringr::str_wrap(x, width = 18),
      hh_size   = function(x) stringr::str_wrap(paste("Household size:", x), width = 18)
    )
  ) +
  scale_color_manual(
    values = c(
      "OR" = "#0072B2",          # strong blue
      "Other states" = "#D55E00" # high-contrast orange
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Predicted Probability of Income Error at Initial Certification\nby Employment Type and Household Size (OR vs Other States)",
    x = "Raw benefit relative to maximum (bin midpoint)",
    y = "Predicted probability",
    color = "Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.spacing = unit(1.0, "lines"),
    legend.position = "right"
  )

p_income_trends

# BAR CHART INSTEAD
# Excluding "other"
plot_df_bar <- plot_df %>%
  filter(lf_status != "other") %>%   # <- drop "Other" category
  group_by(hh_size, lf_status, group_or) %>%
  summarise(
    pred_yes = mean(pred_yes, na.rm = TRUE),
    n = sum(n),
    .groups = "drop"
  )

p_income_bar <- ggplot(
  plot_df_bar,
  aes(x = lf_status, y = pred_yes, fill = group_or)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.75
  ) +
  facet_wrap(
    ~ hh_size,
    ncol = 1,
    labeller = labeller(
      hh_size = function(x)
        stringr::str_wrap(paste("Household size:", x), width = 18)
    )
  ) +
  scale_fill_manual(
    values = c(
      "OR" = "#0072B2",
      "Other states" = "#D55E00"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Predicted Probability of Income Error at Initial Certification\nby Household Size and Employment Type (OR vs Other States)",
    x = "Employment type",
    y = "Predicted probability",
    fill = "Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.spacing = unit(1.0, "lines"),
    legend.position = "right"
  )

p_income_bar



