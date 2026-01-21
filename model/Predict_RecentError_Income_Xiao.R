#############################################
### 0. Packages (NEW)
#############################################

library(tidyverse)
library(tidymodels)
library(readr)
library(vip)        # variable importance
library(themis)     # optional, for imbalance
library(parsnip)
library(stringr)
library(nnet)
library(xgboost)

tidymodels_prefer()

#############################################
### 1. Read & basic filter
#############################################

df <- read_csv("model_variables_new.csv", show_col_types = FALSE)
table(df$state)
names(df)
# error_type <- haven::read_dta("error_elements_prog_factors.csv")
error_type <- read_csv("error_elements_prog_factors.csv")

#standardizing to one kind of missing for element
error_type$element[error_type$element=="."] <- ""

error_type %>% filter(prog_factor==".") %>% group_by(element) %>% tally()

error_type %>% filter(element!="") %>% group_by(prog_factor) %>% tally()

element_type_map <- error_type %>% select(prog_factor, element) %>% distinct() 

case_details <- read_csv("case_details.csv")
names(case_details)

df <- merge(df, case_details, by="case_id")

state_policies <- read_csv("snap_state_options_2023.csv", show_col_types = FALSE)

state_policies <- state_policies[,c("State","ReportingRequirements","CertificationPeriods")]

df <- merge(df, state_policies, by.x="state", by.y="State")

#over threshold should only be for overissuance and error > 58, will correct next
table(df$over_threshold, df$status)
df <- df %>%
  filter(year > 2016) %>%
  filter(!is.na(status)) %>%
  mutate(overpayment_error_i = as.numeric(
    if_else(status == "overissuance" & over_threshold==1, 1, 0)))

#### variable clearning / recoding ###
df$overpayment_error_i <- as.factor(df$overpayment_error_i)

df$error_flag <- as.factor(as.character(df$error_flag))

df <- df %>% mutate(expedited_i = expedser<3) # 1 and 2 are for expedited, 2 is for not timely; 3 means not expedited 

#make a categorical outcome measure that indicates whether no error ("correct"), error is underpayment or overpayment under threshold, or overpayment over threshold
df$outcome <- "correct"
df$outcome[df$error_flag=="1"] <- "uncounted_error"
df$outcome[df$overpayment_error_i=="1"] <- "above_threshold_error"
df$outcome <- as.factor(df$outcome)
table(df$outcome)

df <- df %>% mutate(HH_head_LF_status_c = case_when(
  empsta1 == 1 ~ "not in labor force",
  empsta1 == 2 ~ "unemployed and searching",
  empsta1 %in% c(3,4,5,6) ~ "other",
  empsta1 == 7 ~ "self-employed",
  empsta1 == 8 ~ "employed by other",
  TRUE ~ NA
))

df <- df %>% mutate(non_elderly_disabled_i = as.numeric(fsndis>0)) 

df <- df %>% 
  rename(cert_HH_size_FS_n = fsusize, #FNS Tech Docs "Constucted certified unit size"
         benefit_amount_FS = fsben, # "Final calculated benefit"
         net_income_FS = fsnetinc, #Final net countable unit income
         gross_inc_to_poverty_FS = tpov, #"Gross income/poverty level ratio"
         raw_benefit_amount = rawben, #"Reported SNAP benefit received"
         maximum_benefit_for_HH_size = benmax, #Maximum benefit amount
         total_error_amount = amterr, #"Amount of benefit in error"
         children_i = children_present, 
         elderly_i = elderly_present,
         months_since_recert_n = lastcert, #Months since last SNAP certification
         months_recertification_period_n = certmth, # months in certification period
         status_c = status, #1 amount correct, 2 overissuance, 3 underissuance
         total_deductions_fs = fstotde2, #Total deductions
         total_assets_fs = fsasset, #total countable assets under state rules
         people_in_HH_n = ctprhh, #number of people in household
         action_type_c = actntype, #most recetn action type
         adjusted_allotment = benfix) %>%  #"benefit amount adjusted for errors"   
  mutate(state = as.factor(state),
         year = as.factor(year), 
         HH_head_LF_status_c = as.factor(HH_head_LF_status_c),
         cert_HH_size_FS_n = as.integer(cert_HH_size_FS_n),
         rawben_rel_max = raw_benefit_amount / maximum_benefit_for_HH_size, 
         HH_size_rel_recert_HH_size = people_in_HH_n / cert_HH_size_FS_n)

#df <-  df %>% mutate(HH_size_rel_recert_HH_size = people_in_HH_n / cert_HH_size_FS_n)

#############################################
### 2. Dataset split by error timing
#############################################

# 1) Errors at recert

#something like 80% of multi-error cases have same timing. This would only get us another 1300-1500 observations
table(df$timeper1, df$timeper2)[,2] %>% prop.table()

df_recert <- df %>%
  filter(timeper1 == "at time of most recent action by agency"  & action_type_c=="recertification" | status_c=="amount correct" & action_type_c=="recertification")



#######################################################
##### 3. define types of errors to be predicted  #####
#######################################################
table(error_type$element)
table(error_type$prog_factor)

#unit composition
error_is_unit_composition <- error_type %>% mutate(unit_composition_error_i = case_when(
  element=="unit composition" ~ 1,
  TRUE ~ 0))
names(error_type)
error_is_unit_composition <- error_is_unit_composition %>% select(case_id, unit_composition_error_i) %>% 
  group_by(case_id) %>% slice_max(unit_composition_error_i, n=1, with_ties=F) %>% distinct()

colnames(error_is_unit_composition) <- c("case_id", "error_is_unit_composition_i")
df_recert_unit_composition <- merge(error_is_unit_composition, df_recert, by="case_id")

#income
error_is_income <- error_type %>% mutate(income_error_i = case_when(
  prog_factor=="income" ~ 1,
  TRUE ~ 0), keep.all=TRUE) %>% select(case_id, income_error_i, element) %>% distinct()
names(error_is_income)

error_is_income_overall <- error_is_income %>% group_by(case_id) %>% slice_max(income_error_i, n=1, with_ties = F)

#this keeps those multiple deduction errors:
error_is_income_by_element <- error_is_income %>% group_by(case_id) %>% slice_max(income_error_i, with_ties = T)

df_recert_income <- merge(error_is_income_overall, df_recert, by="case_id")
df_recert_income <- df_recert_income %>% mutate(income_error_c = case_when(
  income_error_i==1 & outcome=="above_threshold_error" ~ "over_threshold", 
  income_error_i==1 & outcome=="uncounted_error" ~ "under_threshold", 
  income_error_i==0 & outcome=="above_threshold_error" ~ "non_income_error", 
  income_error_i==0 & outcome=="uncounted_error" ~ "non_income_error",
  income_error_i==0 & outcome=="correct" ~ "no_error",
  TRUE ~ NA))

df_recert_income$income_error_c <- as.factor(df_recert_income$income_error_c)
table(df_recert_income$income_error_c)

table(df_recert_income$income_error_c, df_recert_income$outcome)


#only 2k deduction errors that are also overpayments. 
table(df_recert_income$overpayment_error_i, df_recert_income$income_error_i)

table(df_recert_income$ReportingRequirements)
df_recert_income_SandCreporting <- df_recert_income %>% filter(ReportingRequirements == "Simplified and change reporting")
df_recert_income_Sreporting <- df_recert_income %>% filter(ReportingRequirements == "Simplified reporting only")

#############################################
### 4. Feature set
#############################################

features <- c(
  "cert_HH_size_FS_n",            # certified household size
  #"people_in_HH_n", #hh size,
  "HH_size_rel_recert_HH_size",
  "children_i",   # children indicator
  "gross_inc_to_poverty_FS",
  #  "action_type_c",#most recent action type
  "elderly_i",    # elderly indicator
  #"months_since_recert_n",#months_since_last cert
  "non_elderly_disabled_i",# disabled indicator
  "HH_head_LF_status_c", #lfstatus
  "total_deductions_fs",           # total deductions
  #  "net_income_FS",           # net income (optional but useful)
  # "abwdst1",            # (keep if exists)
  #  "expedited_i",           # expedited service
  #  "cat_elig",           # categorical eligibility 
  #"amount_rel_max",
  "rawben_rel_max",
  #"raw_benefit_amount", #benefit amount
  #"ReportingRequirements",
  "CertificationPeriods",
  "state",
  "year"
)

model_data_SandC <- df_recert_income_SandCreporting %>% select("income_error_c",all_of(features))
model_data_SandC <- drop_na(model_data_SandC) 
model_data_SO <- df_recert_income_Sreporting %>% select("income_error_c",all_of(features))
model_data_SO <- drop_na(model_data_SO) 


#model_data$counted_income_error_i <- as.character(model_data$counted_income_error_i)

#model_data$counted_income_error_i[model_data$counted_income_error_i=="FALSE"] <- "no"
#model_data$counted_income_error_i[model_data$counted_income_error_i=="TRUE"] <- "yes"
#model_data$counted_income_error_i <- factor(model_data$counted_income_error_i, levels = c("yes","no"))
#levels(model_data$counted_income_error_i)

ols_data <- df_recert_income %>% select("income_error_c","element",all_of(features))

ols_data <- drop_na(ols_data) 
#############################################
### 4. Preprocessing recipe
#############################################

make_recipe <- function(data){
  recipe(
    income_error_c ~ ., 
    data = data %>% select(all_of(c("income_error_c", features)))) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) #%>%
  #step_normalize(all_numeric_predictors())
}


#############################################
### 5. Models (RF + Boosted Trees)
#############################################

cores <- parallel::detectCores()

set.seed(111)
cv_set <- vfold_cv(model_data_SO, v = 3)

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

rf_recipe <- recipe(income_error_c ~ ., data = model_data_SO)
names(model_data_SO)

rf_workflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)


#########################
rf_spec <- rand_forest(
  mtry = 12,          
  trees = 2500,        
  min_n = 26
) %>%
  set_engine("ranger", num.threads = cores, importance="impurity") %>%
  set_mode("classification")
# 
# boost_spec <- boost_tree(
#   trees = 1000,
#   tree_depth = 10,
#   learn_rate = tune(),
#   loss_reduction = tune()
# ) %>%
#   set_engine("xgboost",, num.threads = cores) %>%
#   set_mode("classification")


#these have gone through a reasonable amount of fine tuning and seems like a solid set of parameters:
boost_spec <- boost_tree(
  mtry = 15, trees = 5000, min_n = 18, tree_depth = 9,
  learn_rate = 0.0481  , loss_reduction = 0.0171, sample_size = 0.582,
  stop_iter = 40
) |>
  set_engine("xgboost", nthread=5) |>
  set_mode("classification") |>
  translate()

multinom_spec <- multinom_reg() %>% 
  set_engine("nnet", penalty=0) %>% 
  set_mode("classification")


#############################################
### 6. Workflow + CV function
#############################################

run_ml <- function(model_spec, recipe, split) {
  wf <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)
  
  fit <- fit(wf, data = training(split))
  
  preds <- predict(fit, testing(split), type = "prob") %>%
    bind_cols(testing(split) %>% select(income_error_c))
  
  # Build probability column list dynamically from truth levels
  lvls <- levels(testing(split)$income_error_c)
  prob_cols <- paste0(".pred_", lvls)
  
  # Keep only columns that actually exist (in case a class is missing)
  prob_cols <- prob_cols[prob_cols %in% names(preds)]
  
  auc <- roc_auc(
    data  = preds,
    truth = income_error_c,
    all_of(prob_cols)
  )
  
  list(fit = fit, auc = auc)
}

tune_ml <- function(model_spec, recipe, folds, grid = 20) {
  wf <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)
  
  tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(roc_auc)
  )
}

make_split <- function(data, prop = 3/4) {
  initial_split(data, prop = prop, strata = income_error_c)
}

make_folds <- function(split, v = 5) {
  vfold_cv(training(split), v = v, strata = income_error_c)
}


#############################################
### 7. Run models (Certification errors)
#############################################

# Build split/folds/recipe for each dataset
split_SO   <- make_split(model_data_SO, prop = 3/4)
folds_SO   <- make_folds(split_SO, v = 5)
rec_SO     <- make_recipe(training(split_SO))

split_SandC <- make_split(model_data_SandC, prop = 3/4)
folds_SandC <- make_folds(split_SandC, v = 5)
rec_SandC   <- make_recipe(training(split_SandC))

# Multinomial (SO)
multinom_recert_income <- run_ml(multinom_spec, rec_SO, split_SO)
multinom_recert_income$auc

# Random Forest (SO / SandC)
rf_recert_SO <- run_ml(rf_spec, rec_SO, split_SO)
rf_recert_SO$auc

rf_recert_SandC <- run_ml(rf_spec, rec_SandC, split_SandC)
rf_recert_SandC$auc

# variable importance (from SO model)
rf_fit_parsnip <- extract_fit_parsnip(rf_recert_SO$fit)
vip(rf_fit_parsnip, num_features = 100)

# Boost 
boost_recert_so <- run_ml(boost_spec, rec_SO, split_SO)
boost_recert_SandC <- run_ml(boost_spec, rec_SandC, split_SandC)

boost_recert_so$auc
boost_recert_SandC$auc

#### different approach would be to model specific common error types using buckets from FNS ####

hh_LF_statuses <- as.vector(unique(model_data_SO$HH_head_LF_status_c)[1:4])
states_list <- as.vector(unique(model_data_SO$state))
years <- as.vector(unique(model_data_SO$year))

prediction_grid <- expand.grid(
  HH_size_rel_recert_HH_size = 1,
  children_i = c(0),
  elderly_i = c(0),
  non_elderly_disabled_i = c(0),
  cert_HH_size_FS_n = c(1,2,3,4),
  #  state = states_list,
  year=years[c(6,7)],
  HH_head_LF_status_c = hh_LF_statuses,
  total_deductions_fs = c(411),
  rawben_rel_max = c(.25,.75,1.0),
  #expedited_i = c(0),
  #cat_elig = c(0,1,2),
  gross_inc_to_poverty_FS = c(0,40,80,100)#, 
  #ReportingRequirements = "Simplified reporting only", 
  #CertificationPeriods = "12 months only"
)

table(states_policies_for_grid$CertificationPeriods)
states_policies_for_grid <- model_data_SO %>% select(state, CertificationPeriods) %>% distinct()
prediction_grid <- merge(prediction_grid, states_policies_for_grid, all.x=T, all.y=T)

quantile(model_data_SO$rawben_rel_max, p=c(.29,.59))
#income ratio:
#under 50% of max, 50-99%, 100%


predictions <- predict(
  multinom_recert_income$fit, 
  new_data = prediction_grid,
  type = "prob"
)

pred_grid_preds <- cbind(prediction_grid, predictions)
table(pred_grid_preds$state)

summary(model_data_SO[model_data_SO$state=="Washington",])

state_predictions <- pred_grid_preds %>% 
  mutate(value = 1) %>%
  pivot_wider(names_from = state, 
              values_from = value, 
              values_fill = 0,
              names_prefix = "state_")


table(state_predictions$rawben_rel_max)
#creating a variable that will show true when not washington and false when washington
#write.csv(state_predictions, "state_predictions_income_errors.csv")
names(state_predictions)

quantile(state_predictions$gross_inc_to_poverty_FS)
state_predictions_sub <-  state_predictions %>% mutate(other_states=as.factor(state_Washington!=1), 
                                                       benefit_level = case_when(
                                                         rawben_rel_max < 0.5 ~ "under 50%",
                                                         rawben_rel_max >= 0.5 & rawben_rel_max < 1 ~ "50-100%",
                                                         rawben_rel_max >= 1.0 ~ "100%",
                                                         TRUE ~ NA
                                                       ), household_size = case_when(
                                                         cert_HH_size_FS_n == 1 ~ "1",
                                                         cert_HH_size_FS_n == 2 ~ "2",
                                                         cert_HH_size_FS_n > 2 ~ "3+",
                                                         TRUE ~ NA
                                                       ), income_to_poverty = case_when(
                                                         gross_inc_to_poverty_FS >= 0 & gross_inc_to_poverty_FS < 19 ~ "1st Q",
                                                         gross_inc_to_poverty_FS >= 19 & gross_inc_to_poverty_FS < 74 ~ "2nd Q",
                                                         gross_inc_to_poverty_FS >= 74 & gross_inc_to_poverty_FS < 92 ~ "3rd Q",
                                                         gross_inc_to_poverty_FS >= 92 ~ "4th Q",
                                                         TRUE  ~ NA
                                                       ))

table(state_predictions_sub$rawben_rel_max)
table(state_predictions_sub$benefit_level)
table(state_predictions_sub$income_to_poverty)

state_predictions_sub$benefit_level <- factor(state_predictions_sub$benefit_level, levels=c("under 50%","50-100%","100%"))
state_predictions_reduced <- state_predictions_sub %>%
  group_by(CertificationPeriods,
           household_size, income_to_poverty) %>%  # Need to track which row
  summarize(
    pred_no_error = mean(`.pred_no_error`),
    pred_non_income_error = mean(`.pred_non_income_error`),
    pred_over_threshold = mean(`.pred_over_threshold`),
    pred_under_threshold = mean(`.pred_under_threshold`),
    #    .pred_lower = quantile(`.pred_yes`, 0.025),
    #    .pred_upper = quantile(`.pred_yes`, 0.975)
  ) 

state_predictions_reduced$HH_head_LF_status_c <- factor(state_predictions_reduced$HH_head_LF_status_c, 
                                                        levels = c("employed by other","not in labor force","self-employed","unemployed and searching"))

ggplot(state_predictions_reduced, aes(x = income_to_poverty, y = pred_over_threshold, 
                                      color = CertificationPeriods)) +
  facet_grid(.~household_size) +
  geom_point(position = position_dodge(width = 0.04), size=2.5) +
  scale_color_viridis_d() +
  labs(title = "Predicted Probability of Income Error by HH Size and Income to Poverty Ratio: 
       Simplified Reporting States by Reporting Requirements, 2022-2023",
       color = NULL, y="probability of income error", x="income to poverty quartile", caption="multinomial model (n=68,986); AUC=.77. 
       n states: 12 & 24 mo: 8; 12 mo: 6; 4 to 24 mo: 17.") + ylim(0,.4) + theme_bw()

ggsave(filename="RECERT_multinomial_predicted_incError_HH_size_IncomeToPov_states_wSameOptions.png", device="png", width = 10, height = 4, units = "in", dpi=300)


### model works well for overall deduction errors, need to add specific deduction amounts
m7_st_el <- glm(income_error_i ~ children_i + elderly_i + non_elderly_disabled_i +
                  HH_head_LF_status_c + elderly_i * total_deductions_fs + non_elderly_disabled_i * total_deductions_fs + cert_HH_size_FS_n * poly(rawben_rel_max,3) + state + year, family=binomial, data=ols_data)
summary(m7_st_el)

test <- predict(m7_st_el, data=ols_data, type="response")

ols_data$test <- test
table(ols_data$test>.5, ols_data$income_error_i) %>% prop.table


#group of states with same reporting and cert reqs:
states_list <- c("Washington","Connecticut","Alaska","Indiana","Maine","South_Dakota")
model_data_sub <- model_data_SO %>% filter(state %in% states_list)

table(model_data_sub$income_error_c)
model_data_state <- model_data_sub %>% mutate(Washington= as.factor(state=="Washington"), 
                                              benefit_level = case_when(
                                                rawben_rel_max < 0.5 ~ "under 50%",
                                                rawben_rel_max >= 0.5 & rawben_rel_max < 1 ~ "50-100%",
                                                rawben_rel_max >= 1.0 ~ "100%",
                                                TRUE ~ NA
                                              )) %>% mutate(household_size = case_when(
                                                cert_HH_size_FS_n == 1 ~ "1",
                                                cert_HH_size_FS_n == 2 ~ "2",
                                                cert_HH_size_FS_n > 2 ~ "3+",
                                                TRUE ~ NA
                                              )) %>% mutate(income_to_poverty = case_when(
                                                gross_inc_to_poverty_FS >= 0 & gross_inc_to_poverty_FS < 19 ~ "1st Q",
                                                gross_inc_to_poverty_FS >= 19 & gross_inc_to_poverty_FS < 74 ~ "2nd Q",
                                                gross_inc_to_poverty_FS >= 74 & gross_inc_to_poverty_FS < 92 ~ "3rd Q",
                                                gross_inc_to_poverty_FS >= 92 ~ "4th Q",
                                                TRUE  ~ NA
                                              ))
table(model_data_state$income_to_poverty)
table(model_data_state$Washington)

#need to summarize by household size and by household head LF status separately, not together. Numbers get too small. 
table(model_data_state$benefit_level)

quantile(model_data_state$rawben_rel_max, .3)

table(model_data_state$income_error_c)
summary(model_data_state$gross_inc_to_poverty_FS)
quantile(model_data_state$gross_inc_to_poverty_FS, .99)
quantile(model_data_state$gross_inc_to_poverty_FS)

actual_differences <- model_data_state %>% filter(cert_HH_size_FS_n < 5 & HH_head_LF_status_c!="other" & 
                                                    year %in% c("2018","2019","2021","2022","2023"))  %>% 
  group_by(Washington,household_size, benefit_level) %>%  
  summarize(
    no_error = mean(as.numeric(income_error_c == "no_error")),
    non_income_error = mean(as.numeric(income_error_c == "non_income_error")),
    income_err_under_threshold = mean(as.numeric(income_error_c == "under_threshold")),
    income_err_over_threshold = mean(as.numeric(income_error_c == "over_threshold")), n=n()) 


actual_differences$benefit_level <- factor(actual_differences$benefit_level, levels=c("under 50%","50-100%","100%"))
actual_differences$household_size
ggplot(actual_differences[actual_differences$n>29,], aes(x = benefit_level, y = income_err_over_threshold, 
                                                         color = Washington)) +
  geom_point(position = position_dodge(width = 0.01), size=2.5) +  
  facet_grid(.~household_size) +
  scale_color_discrete(labels=c("Washington","AL CT IN ME SD")) +
  labs(title = "RECERT: Observed Income Error by HH Size and Benefit Level: 
       WA vs. States with Same Options, 2018-2023",
       color = NULL, y="probability of income error", x="benefit level") + theme_bw() + ylim(0,0.4)


ggsave(filename="RECERT_Observed_HHSize_WA_vs_States_withSameOptions_by_incomeErrorsCounted.png", device="png", width = 10, height = 4, units = "in", dpi=300)
