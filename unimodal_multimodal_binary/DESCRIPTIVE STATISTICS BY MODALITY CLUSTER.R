# ======================================================
# DESCRIPTIVE STATISTICS BY MODALITY CLUSTER
# ======================================================

# Clear workspace and load libraries
rm(list = ls())
library(dplyr)
library(tidyr)
library(kableExtra)

# Load data
actual_df <- read.csv("D:\\Project code\\006_hyperpath_ganesh_revised_csv_679_rev_rem_ipt_nmt (1).csv", header = TRUE)

# ======================================================
# DATA PREPARATION AND VARIABLE CREATION
# ======================================================

# Create binary modality classification
actual_df$unimodal <- ifelse(actual_df$modality_mar20 %in% c("multimodal SP", "unimodal", "unimodal same mode"), 1, 0)
actual_df$multimodal <- ifelse(actual_df$modality_mar20 %in% c("assumed to be multimodal", "multimodal") & 
                                 actual_df$Altmode_mar20 != -99, 1, 0)

# Create necessary variables
actual_df <- actual_df %>%
  mutate(
    # Vehicle ownership categories
    twcar = ifelse(n2w_mar20 > 0 & n4w_mar20 > 0, 1, 0),
    age45 = ifelse(Age >= 45, 1, 0),
    kids = ifelse(kid618 > 0, 1, 0),
    twhh = ifelse(n2w_mar20 > 0, 1, 0),
    novehicle = ifelse(n2w_mar20 == 0 & n4w_mar20 == 0, 1, 0),
    
    # Distance categories (in meters)
    wrkdis2 = ifelse(lwttdist_mar20 <= 2000, 1, 0),
    wrkdis5 = ifelse(lwttdist_mar20 <= 5000, 1, 0),
    wrkdis10 = ifelse(lwttdist_mar20 > 10000, 1, 0),
    
    # Demographic variables
    female = ifelse(gen == 0, 1, 0),
    drvknow = ifelse(Drvkn2w == 1 & Drvkn4w == 1, 1, 0),
    inc40_more = ifelse(hhinc >= 6, 1, 0),
    
    # Travel time and cost
    travel_time = case_when(
      AWM_mar20 == 1 ~ twlrtt1_mar20,
      AWM_mar20 == 2 ~ cartt1_mar20,
      AWM_mar20 == 3 ~ bustt1_mar20,
      AWM_mar20 == 4 ~ tratt1_mar20,
      AWM_mar20 == 5 ~ autott1_mar20,
      AWM_mar20 == 6 ~ shautt1_mar20,
      AWM_mar20 == 7 ~ shautt1_mar20,
      AWM_mar20 == 8 ~ cbustt1_mar20,
      AWM_mar20 == 9 ~ wlktt1_mar20,
      AWM_mar20 == 10 ~ cyctt1_mar20,
      TRUE ~ 0
    )
  ) %>%
  # Handle missing values
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# Check modality distribution
cat("Modality Distribution:\n")
print(table(actual_df$unimodal, actual_df$multimodal))

# ======================================================
# DESCRIPTIVE STATISTICS BY MODALITY CLUSTER
# ======================================================

# 1. Ensure the modality clusters are correctly defined
actual_df <- actual_df %>%
  mutate(Modality_Cluster = case_when(
    multimodal == 1 ~ "Multimodal",
    unimodal == 1 ~ "Unimodal",
    TRUE ~ "Other"
  ))

# Check cluster distribution
cat("\nModality Cluster Distribution:\n")
print(table(actual_df$Modality_Cluster))

# 2. Select key variables for profiling - FIXED SELECTION
# Use base R selection instead of any_of() to avoid compatibility issues
existing_vars <- c("Modality_Cluster", "female", "Age", "hhinc", "n2w_mar20", "n4w_mar20", 
                   "novehicle", "drvknow", "oddtim", "comoth", "PickDrop", "kids", "travel_time",
                   "BUSSECL", "STANDTRNH", "BUSRELL", "BUSCRWDM", "TRNCRWDM", "hbus500", 
                   "hurban", "wdur8", "wdur810", "wrkdis2")

# Check which variables actually exist in the dataframe
available_vars <- names(actual_df)
vars_to_keep <- existing_vars[existing_vars %in% available_vars]

cat("\nVariables to include in analysis:\n")
print(vars_to_keep)

# Select only variables that actually exist using base R
key_variables <- actual_df[, vars_to_keep, drop = FALSE]

# 3. Generate Summary Statistics for Each Cluster
descriptive_stats <- key_variables %>%
  group_by(Modality_Cluster) %>%
  summarise(across(
    everything(),
    list(
      N = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

# Pivot longer for better formatting
descriptive_stats_long <- descriptive_stats %>%
  pivot_longer(
    cols = -Modality_Cluster,
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )

# 4. Reshape the data for a clearer side-by-side comparison
comparison_table <- descriptive_stats_long %>%
  pivot_wider(
    id_cols = Variable,
    names_from = Modality_Cluster,
    values_from = c(Mean, SD, N),
    names_glue = "{Modality_Cluster}_{.value}"
  )

# 5. View the results
cat("\nDescriptive Statistics by Modality Cluster:\n")
print(comparison_table, n = Inf)

# 6. Create a nicely formatted table
if(requireNamespace("kableExtra", quietly = TRUE)) {
  formatted_table <- comparison_table %>%
    kableExtra::kbl(digits = 3, caption = "Descriptive Statistics by Modality Cluster") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
  
  print(formatted_table)
} else {
  cat("kableExtra package not available for formatted table\n")
}

# 7. Additional summary statistics
cat("\nAdditional Summary Statistics:\n")
summary_stats <- actual_df %>%
  group_by(Modality_Cluster) %>%
  summarise(
    n = n(),
    avg_distance = mean(lwttdist_mar20/1000, na.rm = TRUE),
    avg_travel_time = mean(travel_time/60, na.rm = TRUE),
    female_percent = mean(female, na.rm = TRUE) * 100,
    no_vehicle_percent = mean(novehicle, na.rm = TRUE) * 100,
    avg_age = mean(Age, na.rm = TRUE),
    avg_income = mean(hhinc, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

# 8. Check for any missing values in key variables
cat("\nMissing Values Check:\n")
missing_check <- sapply(key_variables, function(x) sum(is.na(x)))
print(missing_check)

# 9. Simple summary if the complex table fails
cat("\nSimple Summary by Modality Cluster:\n")
simple_summary <- actual_df %>%
  group_by(Modality_Cluster) %>%
  summarise(
    n = n(),
    avg_age = mean(Age, na.rm = TRUE),
    avg_income = mean(hhinc, na.rm = TRUE),
    percent_female = mean(female, na.rm = TRUE) * 100,
    avg_travel_time = mean(travel_time/60, na.rm = TRUE),
    avg_distance = mean(lwttdist_mar20/1000, na.rm = TRUE),
    .groups = 'drop'
  )
print(simple_summary)