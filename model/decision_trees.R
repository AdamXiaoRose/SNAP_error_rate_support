# Enhanced Decision Tree Analysis Functions
# Captures error rates, false positive rates, and enables tree plotting

library(dplyr)
library(rpart)
library(rpart.plot)

library(RColorBrewer)

# Get the exact RdYlGn palette and reverse it for GnYlRd
pal_colors <- brewer.pal(11, "RdYlGn")
gnylrd_colors <- rev(pal_colors)  # Reverse to get GnYlRd
gradient_colors <- colorRampPalette(pal_colors)(150)
gradient_colors <- gradient_colors[26:125]
gradient_colors <- unname(gradient_colors)

table(df_cert_only$element1)
#model_data <- model_data  %>% filter(rawben_rel_max<=1)
names(model_data)
model_data <- df_cert_only %>% select("payment_error_i",features,"rescaled_weight","race_ethnicity","total_error_amount", "unit_composition_error", "element1")


model_data <- model_data %>% select(-c(HH_head_LF_status_c, ReportingRequirements, BBCE, rescaled_weight, race_ethnicity, total_error_amount, unit_composition_error,
                                       gross_inc_to_poverty_FS, raw_gross, raw_net, wages_salaries, utilities, regioncd))
#
table(model_data$element1=="wages and salaries")
model_data <- model_data %>% filter(element1=="wages and salaries"| is.na(element1))
table(model_data$payment_error_i)

model_data <- model_data %>% filter(rawben_rel_max<=1)

#464 cases where rawben/max is greater than 1
model_data$rawben_rel_max[model_data$rawben_rel_max>1] <- 1
table(model_data$rawben_rel_max>1)
#model_data$rawben_rel_max <- NULL

#379 errors where rawben_rel_max is higher than 1, suggesting a HH composition error
#model_data$year <- NULL
model_data$element1 <- NULL

model_data <- drop_na(model_data) 
prop.test(table(model_data$cat_elig, model_data$payment_error_i))
model_data$cat_elig <- as.integer(model_data$cat_elig!="0")

model_data$non_elderly_disabled_i <- as.integer(model_data$non_elderly_disabled_i=="TRUE")
# model_data$children_i <- factor(model_data$children_i=="1")
# model_data$elderly_i <- factor(model_data$elderly_i=="1")
# model_data$married <- factor(model_data$married=="1")
# model_data$anyone_working <- factor(model_data$anyone_working=="1")
model_data$homeless <- as.integer(model_data$homeless=="TRUE")
model_data$rent_mortage <- NULL
summary(model_data)

#model_data <- model_data %>%
#  mutate(across(where(is.factor), as.character))


#model_data <- model_data %>%
#  mutate(across(where(is.factor), ~factor(., levels = c("FALSE", "TRUE"))))

summary(model_data)

# Enhanced Decision Tree Analysis Functions
# Captures error rates, false positive rates, and enables tree plotting

library(dplyr)
library(rpart)
library(rpart.plot)

# Function to calculate error capture by depth
calculate_error_capture_by_depth <- function(tree_model, data, outcome_var, max_depth = 5) {
  
  # Get predictions at each node depth
  frame <- tree_model$frame
  
  # Calculate total errors in full dataset
  # Handle factor outcome variable properly
  actual <- data[[outcome_var]]
  if (is.factor(actual)) {
    # Assume level 2 is the positive class (error = 1)
    # Or convert to character then numeric
    actual_char <- as.character(actual)
    actual_num <- as.numeric(actual_char)
    
    # If that fails, try assuming the second level is positive
    if (any(is.na(actual_num))) {
      actual_num <- as.numeric(actual) - 1  # Convert factor levels (1,2) to (0,1)
    }
  } else {
    actual_num <- as.numeric(actual)
  }
  
  # Check for NAs
  if (all(is.na(actual_num))) {
    return(rep(NA, max_depth))
  }
  
  total_errors <- sum(actual_num == 1, na.rm = TRUE)
  
  if (is.na(total_errors) || total_errors == 0) {
    return(rep(NA, max_depth))
  }
  
  # Get node assignments for each observation
  node_assignments <- as.numeric(rownames(tree_model$frame)[tree_model$where])
  
  # Calculate depth for each node
  # Depth is determined by the split sequence
  node_depths <- rep(0, nrow(frame))
  
  # Simple depth calculation based on node number
  # In rpart, node numbers follow a binary pattern
  for (i in 1:nrow(frame)) {
    node_num <- as.numeric(rownames(frame)[i])
    # Depth is floor(log2(node_num))
    node_depths[i] <- floor(log2(node_num))
  }
  
  # For each depth level, calculate what proportion of errors are captured
  error_capture <- numeric(max_depth)
  
  for (d in 1:max_depth) {
    # Get nodes at this depth or shallower
    nodes_at_depth <- which(node_depths <= d)
    
    # Find observations in these nodes that are predicted as errors (class 1)
    obs_in_nodes <- which(tree_model$where %in% nodes_at_depth)
    
    # Get the predicted class for these nodes
    predicted_classes <- predict(tree_model, data, type = "class")
    predicted_num <- as.numeric(as.character(predicted_classes))
    
    # If character conversion fails, use factor levels
    if (any(is.na(predicted_num))) {
      predicted_num <- as.numeric(predicted_classes) - 1
    }
    
    # Count errors that are correctly identified by splits up to this depth
    # This is approximate - more precise would require pruning to exact depth
    errors_captured <- sum(actual_num[obs_in_nodes] == 1 & predicted_num[obs_in_nodes] == 1, na.rm = TRUE)
    
    error_capture[d] <- errors_captured / total_errors
  }
  
  return(error_capture)
}

# Function to calculate false positive rate
calculate_fpr <- function(tree_model, data, outcome_var) {
  
  actual <- data[[outcome_var]]
  predicted <- predict(tree_model, data, type = "class")
  
  # Handle factor outcomes properly
  if (is.factor(actual)) {
    actual_char <- as.character(actual)
    actual_num <- as.numeric(actual_char)
    
    # If that fails, try assuming the second level is positive
    if (any(is.na(actual_num))) {
      actual_num <- as.numeric(actual) - 1
    }
  } else {
    actual_num <- as.numeric(actual)
  }
  
  if (is.factor(predicted)) {
    predicted_char <- as.character(predicted)
    predicted_num <- as.numeric(predicted_char)
    
    if (any(is.na(predicted_num))) {
      predicted_num <- as.numeric(predicted) - 1
    }
  } else {
    predicted_num <- as.numeric(predicted)
  }
  
  # Check for NAs
  if (all(is.na(actual_num)) || all(is.na(predicted_num))) {
    return(NA)
  }
  
  # False positives: predicted 1, actual 0
  false_positives <- sum(predicted_num == 1 & actual_num == 0, na.rm = TRUE)
  
  # True negatives: actual 0
  actual_negatives <- sum(actual_num == 0, na.rm = TRUE)
  
  if (is.na(actual_negatives) || actual_negatives == 0) {
    return(NA)
  }
  
  fpr <- false_positives / actual_negatives
  
  return(fpr)
}

# Enhanced function to extract split information with actual values
get_tree_split_info <- function(tree_model, n_top = 5) {
  
  frame <- tree_model$frame
  splits <- tree_model$splits
  
  # Check if splits exist
  if (is.null(splits) || length(splits) == 0 || nrow(frame) == 1) {
    # No splits - return empty dataframe with NA values
    split_df <- data.frame(
      split_order = 1:n_top,
      variable = NA,
      split_value = NA,
      node = NA,
      stringsAsFactors = FALSE
    )
    return(split_df)
  }
  
  # Get variables used in splits (exclude leaf nodes)
  split_vars <- frame$var[frame$var != "<leaf>"]
  
  # Get split points for each split
  split_info <- list()
  
  split_counter <- 1
  for (i in 1:nrow(frame)) {
    if (frame$var[i] != "<leaf>") {
      var_name <- frame$var[i]
      
      # Get the split value from the splits matrix
      # The splits matrix has one row per split
      if (!is.null(splits) && is.matrix(splits) && split_counter <= nrow(splits)) {
        split_point <- splits[split_counter, "index"]
        
        split_info[[split_counter]] <- list(
          variable = var_name,
          split_value = split_point,
          node = as.numeric(rownames(frame)[i])
        )
        
        split_counter <- split_counter + 1
      }
    }
  }
  
  # Take top n splits
  top_splits <- split_info[1:min(n_top, length(split_info))]
  
  # Convert to data frame
  if (length(top_splits) > 0) {
    split_df <- data.frame(
      split_order = 1:length(top_splits),
      variable = sapply(top_splits, function(x) x$variable),
      split_value = sapply(top_splits, function(x) x$split_value),
      node = sapply(top_splits, function(x) x$node),
      stringsAsFactors = FALSE
    )
  } else {
    split_df <- data.frame(
      split_order = integer(0),
      variable = character(0),
      split_value = numeric(0),
      node = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Pad with NA if fewer than n_top
  if (nrow(split_df) < n_top) {
    padding <- data.frame(
      split_order = (nrow(split_df) + 1):n_top,
      variable = NA,
      split_value = NA,
      node = NA,
      stringsAsFactors = FALSE
    )
    split_df <- rbind(split_df, padding)
  }
  
  return(split_df)
}

# Main function to fit trees and extract comprehensive information
fit_trees_comprehensive <- function(data, outcome_var, state_var = "state", n_top = 5, 
                                    maxdepth = 5, minsplit = 20, cp = 0.001) {
  
  results_list <- list()
  tree_models <- list()  # Store tree models for later plotting
  
  states <- unique(data[[state_var]])
  
  for (s in states) {
    cat("Processing state:", s, "\n")
    
    # Subset data for current state
    state_data <- data %>% filter(!!sym(state_var) == s)
    
    # Ensure outcome is a factor for classification
    state_data[[outcome_var]] <- as.factor(state_data[[outcome_var]])
    
    # Create formula
    predictor_vars <- setdiff(names(state_data), c(state_var, outcome_var))
    formula <- as.formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + ")))
    
    # Fit classification decision tree
    tree_model <- rpart(formula, 
                        data = state_data, 
                        method = "class",
                        control = rpart.control(cp = cp, 
                                                minsplit = minsplit, 
                                                maxdepth = maxdepth,
                                                xval = 10))
    
    # Diagnostic: check if tree has splits
    n_splits <- sum(tree_model$frame$var != "<leaf>")
    if (n_splits == 0) {
      cat("  Warning: No splits created for", s, 
          "- N =", nrow(state_data),
          "- Outcome proportions:", table(state_data[[outcome_var]]), "\n")
    }
    
    # Store the tree model
    tree_models[[s]] <- tree_model
    
    # Extract split information with values
    split_info <- get_tree_split_info(tree_model, n_top = n_top)
    
    # Calculate error capture by top 5 splits
    # Approximate by using depth (first 5 splits ≈ depth 2-3)
    error_capture <- calculate_error_capture_by_depth(tree_model, state_data, outcome_var, max_depth = 5)
    
    # Calculate false positive rate
    fpr <- calculate_fpr(tree_model, state_data, outcome_var)
    
    # Combine results
    results_list[[s]] <- split_info %>%
      mutate(
        state = s,
        fpr = fpr,
        error_capture_depth_1 = error_capture[1],
        error_capture_depth_2 = error_capture[2],
        error_capture_depth_3 = error_capture[3],
        error_capture_depth_4 = error_capture[4],
        error_capture_depth_5 = error_capture[5]
      ) %>%
      select(state, split_order, variable, split_value, node, fpr, 
             error_capture_depth_1:error_capture_depth_5)
  }
  
  # Combine all results
  results_df <- bind_rows(results_list)
  
  # Return both results and models
  return(list(
    results = results_df,
    tree_models = tree_models
  ))
}
# Function to plot a tree for a specific state
plot_state_tree <- function(tree_models, state_name, 
                            main_title = NULL,
                            save_path = NULL,
                            width_inches = 14,
                            height_inches = 10,
                            dpi = 300) {
  
  if (!state_name %in% names(tree_models)) {
    stop(paste("State", state_name, "not found in tree models"))
  }
  
  tree_model <- tree_models[[state_name]]
  
  if (is.null(main_title)) {
    main_title <- paste("Decision Tree for Wages and Salaries Errors Occuring at Certification in", state_name, "2017-2023")
  }
  
  # Save to file if path provided
  if (!is.null(save_path)) {
    # Calculate pixel dimensions from inches and dpi
    width_px <- width_inches * dpi
    height_px <- height_inches * dpi
    
    png(save_path, 
        width = width_px, 
        height = height_px, 
        res = dpi)
  }
  
  # Plot using rpart.plot with larger text
  rpart.plot(tree_model,
             main = main_title,
             type = 4,  # Show split labels
             extra = 1,  # 
             fallen.leaves = FALSE,
             branch.lty = 1,
             shadow.col = "gray",
             box.palette = "GnYlRd",
             cex = 1,  # Increase text size
             tweak = 1.2,
             node.fun = function(x, labs, digits, varlen) {
               gsub("^(Yes|No)\n", "", labs)
             }) 
  legend_x <- 0.025
  legend_y <- 0.975
  legend_width <- 0.015
  legend_height <- 0.2
  
  # Draw the gradient bar
  for (i in 1:100) {
    rect(legend_x, 
         legend_y - legend_height * (i/100),
         legend_x + legend_width,
         legend_y - legend_height * ((i-1)/100),
         col = gradient_colors[i],
         border = NA)
  }
  
  # Add border around gradient
  rect(legend_x, legend_y - legend_height, 
       legend_x + legend_width, legend_y, 
       border = "black")
  
  # Add labels
  text(legend_x + legend_width + 0.002, legend_y, 
       "Very high", pos = 4, cex = 1.1)
  text(legend_x + legend_width + 0.002, legend_y - legend_height, 
       "Very low", pos = 4, cex = 1.1)
  text(legend_x + legend_width/2, legend_y + 0.04, 
       "Share with errors", cex = 1.1, font = 2)
  
  text(x=.90,y=.98,
       'How to read this: "50  40" in a node means that 50 cases 
       have no error and 40 cases have an error. Note that this 
       only relies on public data that excludes ineligible cases.', cex = 1.1)
  text(legend_x + legend_width + 0.002, legend_y - legend_height, 
       "Very low", pos = 4, cex = 1.1)
  text(legend_x + legend_width/2, legend_y + 0.04, 
       "Share with errors", cex = 1.1, font = 2)
  
  if (!is.null(save_path)) {
    dev.off()
    cat("Tree plot saved to:", save_path, "\n")
  }
}

# Function to plot all trees
plot_all_trees <- function(tree_models, output_dir = "tree_plots",
                           width_inches = 55,
                           height_inches = 25,
                           dpi = 300) {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (state_name in names(tree_models)) {
    file_path <- file.path(output_dir, paste0(state_name, "_2017_2023_wages_salaries_drops_rawben_over1.png"))
    plot_state_tree(tree_models, state_name, 
                    save_path = file_path,
                    width_inches = width_inches,
                    height_inches = height_inches,
                    dpi = dpi)
  }
  
  cat("All tree plots saved to:", output_dir, "\n")
}

rn_model_data <- model_data
rn_model_data <- rn_model_data %>% rename(household_size = cert_HH_size_FS_n,
                                          HH_members_to_participating = HH_size_rel_cert_HH_size,
                                          #labor_force_status = HH_head_LF_status_c,
                                          children_present = children_i,
                                          total_deductions = total_deductions_fs,
                                          elderly_present = elderly_i,
                                          disabled_present = non_elderly_disabled_i,
                                          expedited = expedited_i, 
                                          categorical_eligibility = cat_elig,
                                          benefit_relative_to_max_allotment = rawben_rel_max,
                                          earned_income = earned_income,
                                          household_member_works = anyone_working, 
                                          shelter_expenses_relative_to_gross_income = shelter_to_gross_income_ratio,
                                          medical_expenses = med_expenses
                                          
  
)
rn_model_data_2023 <- rn_model_data %>% filter(year %in% "2023")
rn_model_data_2023$year <- NULL
rn_model_data_2017_2022 <- rn_model_data %>% filter(!year %in% "2023") 
rn_model_data_2017_2022$year <- NULL
rn_model_data_2017_2022$state <- NULL
rn_model_data$year <- NULL
rn_model_data$state <- NULL
rn_model_data$state <- "US"

# Usage example:
# ==============
results_output <- fit_trees_comprehensive(
  data = rn_model_data,
  outcome_var = "payment_error_i",
  state_var = "state",
  n_top = 5,
  maxdepth = 6,   # Maximum tree depth (default = 10)
  minsplit = 10,   # Minimum observations to split (default = 20)
  cp = 0.000001       # Complexity parameter (default = 0.001)
)



for(state in names(results_output$tree_models)) {
  results_output$tree_models[[state]]$frame$var <- 
    gsub("_", " ", results_output$tree_models[[state]]$frame$var)
}


# Access results dataframe
results_df <- results_output$results
write.csv(results_df, "state_split_vars.csv", row.names=F)

# Plot a specific state's tree (high quality)
plot_state_tree(results_output$tree_models, "Connecticut",
                save_path = "Connecticut_tree.png",
                width_inches = 30, height_inches = 20, dpi = 300)

# Plot all trees and save to files (high quality, 300 dpi, 14x12 inches)
plot_all_trees(results_output$tree_models,
               output_dir = "state_plots_2017_2023_simple")


errors <- df %>% filter(payment_error_i=="Yes")

table(errors$raw_benefit_amount - errors$benefit_amount_FS<5)
summary(errors$rawnet - errors$net_income_FS)
quantile(errors$rawnet - errors$net_income_FS, .05, na.rm=T)
summary(errors$earned_income - errors$fsearn)
summary(errors$earned_income - errors$fsearn)

wage_errors <- subset(errors, element1 == "wages and salaries")
summary(wage_errors$amount1 == wage_errors$fsgrinc - wage_errors$rawgross)
table(wage_errors$fsgrinc - wage_errors$rawgross==0, wage_errors$state)
table(wage_errors$amount1==0, wage_errors$state)

test_data <- data.frame(
  children_present = c("TRUE", "FALSE", "TRUE", "FALSE"),
  outcome = c(1, 0, 1, 0)
)

table(df$abwdst1, df$payment_error_i)

# Fit a simple tree
test_tree <- rpart(outcome ~ children_present, data = test_data)

# Plot it and see which way it splits
rpart.plot(test_tree)
ggsave(filename="test_tree.png", device="png", width = 8, height = 10, units = "in", dpi=300)

colnames(errs_state_year_n) <- c("state","year","total_errors")

errs_state_year_n %>% group_by(year) %>% summarize(mean(total_errors))
mean(errs_state_year_n$total_errors)

