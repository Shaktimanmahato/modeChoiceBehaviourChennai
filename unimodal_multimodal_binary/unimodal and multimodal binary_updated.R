# ======================================================
# UNIMODAL vs MULTIMODAL BINARY CHOICE MODEL - CORRECTED
# ======================================================

# Clear workspace and load libraries
rm(list = ls())
library(optimx)
library(dplyr)
library(numDeriv)

# Load data
actual_df = read.csv("D:\\Project code\\006_hyperpath_ganesh_revised_csv_679_rev_rem_ipt_nmt (1).csv", header=TRUE)

# ======================================================
# 1. DATA PREPARATION AND VARIABLE CREATION - CORRECTED
# ======================================================

# Create binary modality classification
actual_df$unimodal <- ifelse(actual_df$modality_mar20 %in% c("multimodal SP", "unimodal", "unimodal same mode"), 1, 0)
actual_df$multimodal <- ifelse(actual_df$modality_mar20 %in% c("assumed to be multimodal", "multimodal") & 
                                 actual_df$Altmode_mar20 != -99, 1, 0)

# Check modality distribution
table(actual_df$unimodal, actual_df$multimodal)

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
    ),
    
    car_cost_per_km = ifelse(cardist_google > 0, carco2_mar20 / cardist_google, 0),
    
    # Handle missing values
    across(where(is.numeric), ~ifelse(is.na(.), 0, .))
  )

# ======================================================
# 2. BINARY CHOICE MODEL ESTIMATION - CORRECTED
# ======================================================

# Define the log-likelihood function with ACTUAL variables
illkhd_binary <- function(pars) {
  # Utility for multimodal - using variables that actually exist
  U_multimodal <- pars[1] + 
    pars[2]*actual_df$BUSSECL + 
    pars[3]*actual_df$STANDTRNH + 
    pars[4]*actual_df$BUSRELL +
    pars[5]*actual_df$PPMISS + 
    pars[6]*actual_df$oddtim + 
    pars[7]*actual_df$PickDrop*actual_df$kids +
    pars[8]*actual_df$wdur8 + 
    pars[9]*actual_df$wdur810 + 
    pars[10]*actual_df$hbus500 + 
    pars[11]*actual_df$female +
    pars[12]*actual_df$novehicle*actual_df$wrkdis2 + 
    pars[13]*actual_df$hurban + 
    pars[14]*actual_df$info + 
    pars[15]*actual_df$TWRELH*actual_df$n2w_mar20 + 
    pars[16]*actual_df$BUSCRWDM*actual_df$TRNCRWDM +
    pars[17]*actual_df$age45*actual_df$DISCTRNH + 
    pars[18]*actual_df$drvknow + 
    pars[19]*actual_df$comoth*actual_df$female + 
    pars[20]*actual_df$comothm +
    pars[21]*actual_df$travel_time/100  # Scaled for better convergence
  
  # Probability calculations with numerical stability
  p_multimodal <- 1 / (1 + exp(-U_multimodal))
  p_unimodal <- 1 - p_multimodal
  
  # Log-likelihood with protection against log(0)
  ll <- actual_df$multimodal * log(p_multimodal + 1e-10) + 
    actual_df$unimodal * log(p_unimodal + 1e-10)
  
  return(-sum(ll))
}

# Initial parameter values
initial_pars <- rep(0, 21)

# Estimate model
binary_model <- optimx(initial_pars, illkhd_binary, method = "BFGS", 
                       control = list(trace = 1, maxit = 1000, reltol = 1e-8))

# Extract results
best_pars <- as.numeric(binary_model[1:21])
hessian_matrix <- hessian(illkhd_binary, best_pars)
cov_matrix <- solve(hessian_matrix)
stderr <- sqrt(diag(cov_matrix))
t_val <- best_pars / stderr

# Display results
results <- cbind(best_pars, stderr, t_val)
colnames(results) <- c("Estimate", "Std.Error", "t-value")
rownames(results) <- c("ASC", "BUSSECL", "STANDTRNH", "BUSRELL", "PPMISS", "oddtim", 
                       "PickDrop*kids", "wdur8", "wdur810", "hbus500", "female",
                       "novehicle*wrkdis2", "hurban", "info", "TWRELH*n2w", 
                       "BUSCRWDM*TRNCRWDM", "age45*DISCTRNH", "drvknow",
                       "comoth*female", "comothm", "travel_time")

print("Binary Model Results:")
print(results)

# ======================================================
# 3. MODEL VALIDATION AND PREDICTION - CORRECTED
# ======================================================

# Calculate predicted probabilities
actual_df$U_multimodal_pred <- best_pars[1] + 
  best_pars[2]*actual_df$BUSSECL + 
  best_pars[3]*actual_df$STANDTRNH + 
  best_pars[4]*actual_df$BUSRELL +
  best_pars[5]*actual_df$PPMISS + 
  best_pars[6]*actual_df$oddtim + 
  best_pars[7]*actual_df$PickDrop*actual_df$kids +
  best_pars[8]*actual_df$wdur8 + 
  best_pars[9]*actual_df$wdur810 + 
  best_pars[10]*actual_df$hbus500 + 
  best_pars[11]*actual_df$female +
  best_pars[12]*actual_df$novehicle*actual_df$wrkdis2 + 
  best_pars[13]*actual_df$hurban + 
  best_pars[14]*actual_df$info + 
  best_pars[15]*actual_df$TWRELH*actual_df$n2w_mar20 + 
  best_pars[16]*actual_df$BUSCRWDM*actual_df$TRNCRWDM +
  best_pars[17]*actual_df$age45*actual_df$DISCTRNH + 
  best_pars[18]*actual_df$drvknow + 
  best_pars[19]*actual_df$comoth*actual_df$female + 
  best_pars[20]*actual_df$comothm +
  best_pars[21]*actual_df$travel_time/100

actual_df$predicted_probs <- 1 / (1 + exp(-actual_df$U_multimodal_pred))

# Classification accuracy
actual_df$predicted_class <- ifelse(actual_df$predicted_probs > 0.5, 1, 0)
accuracy <- mean(actual_df$predicted_class == actual_df$multimodal, na.rm = TRUE)
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")

# Confusion matrix
confusion_matrix <- table(Actual = actual_df$multimodal, Predicted = actual_df$predicted_class)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate performance metrics
TP <- confusion_matrix[2, 2]
TN <- confusion_matrix[1, 1]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1_score, 3), "\n")

# ======================================================
# 4. POLICY IMPLICATIONS - CORRECTED
# ======================================================

# Create policy variables that actually exist
actual_df$PT_infra_quality <- ifelse(actual_df$busacc_tide <= 500 & actual_df$Railoff_tide <= 1500, 1, 0)
actual_df$integrated_ticketing <- ifelse(actual_df$TICKTBUS == 1 | actual_df$TICKTTRN == 1, 1, 0)
actual_df$multimodal_info <- ifelse(actual_df$info == 1, 1, 0)

# Calculate baseline probability
baseline_prob <- mean(actual_df$predicted_probs, na.rm = TRUE)

# Function to calculate policy effects
calculate_policy_effect <- function(policy_var, improvement_value = 1) {
  # Create improved scenario
  improved_data <- actual_df
  improved_data[[policy_var]] <- improvement_value
  
  # Recalculate utility with improved policy
  U_improved <- best_pars[1] + 
    best_pars[2]*improved_data$BUSSECL + 
    best_pars[3]*improved_data$STANDTRNH + 
    best_pars[4]*improved_data$BUSRELL +
    best_pars[5]*improved_data$PPMISS + 
    best_pars[6]*improved_data$oddtim + 
    best_pars[7]*improved_data$PickDrop*improved_data$kids +
    best_pars[8]*improved_data$wdur8 + 
    best_pars[9]*improved_data$wdur810 + 
    best_pars[10]*improved_data$hbus500 + 
    best_pars[11]*improved_data$female +
    best_pars[12]*improved_data$novehicle*improved_data$wrkdis2 + 
    best_pars[13]*improved_data$hurban + 
    best_pars[14]*improved_data$info + 
    best_pars[15]*improved_data$TWRELH*improved_data$n2w_mar20 + 
    best_pars[16]*improved_data$BUSCRWDM*improved_data$TRNCRWDM +
    best_pars[17]*improved_data$age45*improved_data$DISCTRNH + 
    best_pars[18]*improved_data$drvknow + 
    best_pars[19]*improved_data$comoth*improved_data$female + 
    best_pars[20]*improved_data$comothm +
    best_pars[21]*improved_data$travel_time/100
  
  improved_probs <- 1 / (1 + exp(-U_improved))
  improved_prob_mean <- mean(improved_probs, na.rm = TRUE)
  
  effect <- (improved_prob_mean - baseline_prob) * 100
  return(effect)
}

# Calculate effects for key policy variables
policy_effects <- c(
  info_availability = calculate_policy_effect("info", 1),
  ticketing_integration = calculate_policy_effect("integrated_ticketing", 1),
  PT_access_quality = calculate_policy_effect("PT_infra_quality", 1)
)

policy_implications <- data.frame(
  Policy_Variable = names(policy_effects),
  Effect_Percentage = policy_effects
)

print("Policy Effects (% change in multimodal probability):")
print(policy_implications)

# ======================================================
# 5. VISUALIZATION
# ======================================================

# Plot distribution of predicted probabilities
hist(actual_df$predicted_probs, breaks = 20, 
     main = "Distribution of Predicted Probabilities",
     xlab = "Predicted Probability of Multimodality",
     col = "lightblue")

# Scatter plot of distance vs predicted probability
plot(actual_df$lwttdist_mar20/1000, actual_df$predicted_probs, 
     col = ifelse(actual_df$multimodal == 1, "blue", "red"),
     xlab = "Distance to Work (km)", 
     ylab = "Predicted Probability of Multimodality",
     main = "Actual vs Predicted Modality Choice by Distance")
legend("topright", legend = c("Multimodal", "Unimodal"), 
       col = c("blue", "red"), pch = 1)

# ======================================================
# 6. SUMMARY STATISTICS
# ======================================================

# Summary statistics by modality
summary_stats <- actual_df %>%
  group_by(multimodal) %>%
  summarise(
    n = n(),
    avg_distance = mean(lwttdist_mar20/1000, na.rm = TRUE),
    avg_travel_time = mean(travel_time/60, na.rm = TRUE),
    female_percent = mean(female, na.rm = TRUE) * 100,
    no_vehicle_percent = mean(novehicle, na.rm = TRUE) * 100,
    .groups = 'drop'
  )

print("Summary Statistics by Modality:")
print(summary_stats)

# ======================================================
# 7. MODEL DIAGNOSTICS
# ======================================================

# Calculate log-likelihood and information criteria
final_ll <- -binary_model$value
n_params <- length(best_pars)
n_obs <- nrow(actual_df)

AIC <- 2 * n_params - 2 * final_ll
BIC <- n_params * log(n_obs) - 2 * final_ll

cat("Final Log-Likelihood:", round(final_ll, 2), "\n")
cat("AIC:", round(AIC, 2), "\n")
cat("BIC:", round(BIC, 2), "\n")
cat("Number of observations:", n_obs, "\n")