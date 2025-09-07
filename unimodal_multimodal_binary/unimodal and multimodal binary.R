# ======================================================
# UNIMODAL vs MULTIMODAL BINARY CHOICE MODEL
# ======================================================

# Clear workspace and load libraries
rm(list = ls())
library(optimx)
library(dplyr)
library(numDeriv)  # Added for Hessian calculation

# Load data
actual_df = read.csv("D:\\Project code\\006_hyperpath_ganesh_revised_csv_679_rev_rem_ipt_nmt (1).csv", header=TRUE)

# ======================================================
# 1. DATA PREPARATION AND VARIABLE CREATION
# ======================================================

# Create binary modality classification - FIXED to avoid overlap
actual_df$unimodal <- ifelse(actual_df$modality_mar20 %in% c("multimodal SP", "unimodal", "unimodal same mode"), 1, 0)
actual_df$multimodal <- ifelse(actual_df$modality_mar20 %in% c("assumed to be multimodal", "multimodal") & 
                                 actual_df$Altmode_mar20 != -99, 1, 0)

# Check if we have both classes and no overlap
table(actual_df$unimodal, actual_df$multimodal)

### Create necessary variables in a single, clean step
actual_df <- actual_df %>%
  mutate(
    # Vehicle ownership categories
    twcar = ifelse(n2w_mar20 > 0 & n4w_mar20 > 0, 1, 0),
    age45 = ifelse(Age >= 45, 1, 0),
    kids = ifelse(kid618 > 0, 1, 0),
    twhh = ifelse(n2w_mar20 > 0, 1, 0),
    onetwonly = ifelse(n2w_mar20 == 1 & n4w_mar20 == 0, 1, 0),
    moretwonly = ifelse(n2w_mar20 > 1 & n4w_mar20 == 0, 1, 0),
    onetwonecar = ifelse((n2w_mar20 == 1 & n4w_mar20 == 1) | (n4w_mar20 > 0 & n2w_mar20 == 0), 1, 0),
    moretwcar = ifelse((n2w_mar20 > 1 & n4w_mar20 == 1) | (n2w_mar20 == 1 & n4w_mar20 > 1) | (n2w_mar20 > 1 & n4w_mar20 > 1), 1, 0),
    novehicle = ifelse(n2w_mar20 == 0 & n4w_mar20 == 0, 1, 0),
    vehicleclass = case_when(
      onetwonly == 1 ~ 1,
      moretwonly == 1 ~ 2,
      onetwonecar == 1 ~ 3,
      moretwcar == 1 ~ 4,
      novehicle == 1 ~ 5,
      TRUE ~ 0
    ),
    
    # Distance categories
    wrkdis2 = ifelse(lwttdist_mar20 <= 2000, 1, 0),
    wrkdis5 = ifelse(lwttdist_mar20 <= 5000, 1, 0),
    wrkdis10 = ifelse(lwttdist_mar20 > 10000, 1, 0),
    wrkdis15 = ifelse(lwttdist_mar20 >= 15000, 1, 0),
    wrkdis510 = ifelse(lwttdist_mar20 > 5000 & lwttdist_mar20 <= 10000, 1, 0),
    wrkdis1015 = ifelse(lwttdist_mar20 > 10000 & lwttdist_mar20 <= 15000, 1, 0),
    
    # Demographic variables
    female = 1 - gen,
    drvknow = ifelse(Drvkn2w == 1 & Drvkn4w == 1, 1, 0),
    trainnear = ifelse(Railoff_tide <= 1300, 1, 0),
    inc40_more = ifelse(hhinc >= 6, 1, 0),
    
    # Other variables
    nmttt1 = (wlktt1_mar20 + cyctt1_mar20) / 2,
    ipt2co2_mar20 = ifelse(AWM_mar20 == 8, shauco2_mar20, cbusco2_mar20),
    onetw_alltime = ifelse(n2w_mar20 == 1 & veava_mar20 == 3 & vehicleclass != 0, 1, 0),
    numvehhh = n2w_mar20 + n4w_mar20,
    
    # Mode choice variables (CORRECTED LINE)
    # Using dplyr::select to avoid conflict with apollo::select
    pri_choice = apply(dplyr::select(., tw_p, car_p, bus_p, trn_p, ipt1_p, ipt2_p, nmt_p), 1, which.max),
    
    # Availability (assuming all modes available)
    tw_avail = 1,
    car_avail = 1,
    bus_avail = 1,
    trn_avail = 1,
    ipt1_avail = 1,
    ipt2_avail = 1,
    nmt_avail = 1
  )

# ----------------------------------------------------------------- #
# Extra Variables
# ----------------------------------------------------------------- #
actual_df$direct_bus <- ifelse(actual_df$bus_p == 1, 1, 0)
actual_df$info_bus <- ifelse(actual_df$info == 1, 1, 0)
actual_df$train_work_near <- ifelse(actual_df$railoffm <= 1500, 1, 0)
actual_df$tw_reliability <- ifelse(actual_df$TWRELH %in% c(2, 3), 1, 0)
actual_df$unplanned_trips <- ifelse(actual_df$nonwrktrips > 2, 1, 0)
actual_df$tw_reliability <- ifelse(actual_df$TWRELH == 1 | actual_df$TWRELM == 1, 1, 0)
actual_df$high_income <- ifelse(actual_df$hhinc >= 5, 1, 0)
actual_df$female_sharedipt <- ifelse(actual_df$female == 1 & actual_df$ipt2_s == 1, 1, 0)

actual_df$travel_time <- NA
actual_df$travel_time[actual_df$primarymode == 1]  <- actual_df$twlrtt1[actual_df$primarymode == 1]
actual_df$travel_time[actual_df$primarymode == 2]  <- actual_df$cartt1[actual_df$primarymode == 2]
actual_df$travel_time[actual_df$primarymode == 3]  <- actual_df$bustt1[actual_df$primarymode == 3]
actual_df$travel_time[actual_df$primarymode == 4]  <- actual_df$tratt1[actual_df$primarymode == 4]
actual_df$travel_time[actual_df$primarymode == 5]  <- actual_df$autott1[actual_df$primarymode == 5]
actual_df$travel_time[actual_df$primarymode == 6]  <- actual_df$shautt1[actual_df$primarymode == 6]
actual_df$travel_time[actual_df$primarymode == 7]  <- actual_df$apptt1[actual_df$primarymode == 7]
actual_df$travel_time[actual_df$primarymode == 8]  <- actual_df$cbustt1[actual_df$primarymode == 8]
actual_df$travel_time[actual_df$primarymode == 9]  <- actual_df$wlktt1[actual_df$primarymode == 9]
actual_df$travel_time[actual_df$primarymode == 10] <- actual_df$cyctt1[actual_df$primarymode == 10]
actual_df$travel_time[actual_df$primarymode == 11] <- actual_df$apptt1[actual_df$primarymode == 11]
actual_df$travel_time[is.na(actual_df$travel_time)] <- 0

actual_df$car_cost_per_km <- ifelse(actual_df$cardist_google > 0, actual_df$carco1 / actual_df$cardist_google, 0)

actual_df$combine_activities <- ifelse(
  (grepl("work", actual_df$petDes, ignore.case = TRUE) &
     (grepl("\\+", actual_df$petDes) | grepl("and", actual_df$petDes, ignore.case = TRUE) | grepl("with", actual_df$petDes, ignore.case = TRUE))
  ) | actual_df$comoth == 1, 1, 0)
actual_df$combine_activities[is.na(actual_df$combine_activities)] <- 0

# Check for missing values and handle them
actual_df <- actual_df %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))

# ======================================================
# 2. BINARY CHOICE MODEL ESTIMATION
# ======================================================

# Define the log-likelihood function with numerical stability
illkhd_binary <- function(pars) {
  # Utility for multimodal
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
    pars[17]*actual_df$age45*actual_df$DISCTRNH*actual_df$DISCBUSH + 
    pars[18]*actual_df$drvknow + 
    pars[19]*actual_df$comoth*actual_df$female + 
    pars[20]*actual_df$comothm + 
    pars[21]*actual_df$trainnear*actual_df$scaptive_mar20 +
    pars[22]*actual_df$direct_bus +
    pars[23]*actual_df$car_cost_per_km +
    pars[24]*actual_df$travel_time
  
  # Probability calculations with numerical stability
  p_multimodal <- 1 / (1 + exp(-U_multimodal))
  p_unimodal <- 1 - p_multimodal
  
  # Log-likelihood with protection against log(0)
  ll <- actual_df$multimodal * log(p_multimodal + 1e-10) + 
    actual_df$unimodal * log(p_unimodal + 1e-10)
  
  return(-sum(ll))  # Return negative log-likelihood for minimization
}

# Initial parameter values
initial_pars <- rep(0, 24)

# Estimate model using BFGS with better control settings
binary_model <- optimx(initial_pars, illkhd_binary, method = "BFGS", 
                       control = list(trace = 1, maxit = 1000, reltol = 1e-8))

# Extract results using numerical Hessian for standard errors
best_pars <- as.numeric(binary_model[1:24])
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
                       "BUSCRWDM*TRNCRWDM", "age45*DISCTRNH*DISCBUSH", "drvknow",
                       "comoth*female", "comothm", "trainnear*scaptive", "direct_bus",
                       "car_cost_per_km", "travel_time")
print("Binary Model Results:")
print(results)

# ======================================================
# 3. ALTERNATIVE SPECIFICATION WITH POLICY VARIABLES
# ======================================================

# Create additional policy variables
actual_df$PT_infra_quality <- ifelse(actual_df$busacc_tide <= 500 & actual_df$Railoff_tide <= 1500, 1, 0)
actual_df$integrated_ticketing <- ifelse(actual_df$TICKTBUS == 1 | actual_df$TICKTTRN == 1, 1, 0)
actual_df$multimodal_info <- ifelse(actual_df$info == 1, 1, 0)

# Enhanced model with policy-sensitive variables
illkhd_enhanced <- function(pars) {
  # Utility function with additional policy variables
  U_multimodal <- pars[1] + 
    pars[2]*actual_df$onetwonly + 
    pars[3]*actual_df$onetwonecar + 
    pars[4]*actual_df$veava_mar20*(1 - actual_df$novehicle) + 
    pars[5]*(1 - actual_df$Drvkn4w) + 
    pars[6]*actual_df$inc40_more + 
    pars[7]*actual_df$female + 
    pars[8]*actual_df$wrkdis2*actual_df$nmt_avail + 
    pars[9]*actual_df$wrkdis10 + 
    pars[10]*actual_df$cbus_p + 
    pars[11]*actual_df$nmt_avail*actual_df$wrkdis10 + 
    pars[12]*actual_df$PPMISS + 
    pars[13]*actual_df$BEASEGET*actual_df$wrkdis10 + 
    pars[14]*actual_df$info + 
    pars[15]*actual_df$REACHTRN + 
    pars[16]*actual_df$FWAUPOLLH + 
    pars[17]*actual_df$LnchHome +
    pars[18]*actual_df$PT_infra_quality +  # Added policy variable
    pars[19]*actual_df$integrated_ticketing +  # Added policy variable
    pars[20]*actual_df$multimodal_info  # Added policy variable
  
  # Probability calculations with numerical stability
  p_multimodal <- 1 / (1 + exp(-U_multimodal))
  p_unimodal <- 1 - p_multimodal
  
  # Log-likelihood with protection against log(0)
  ll <- actual_df$multimodal * log(p_multimodal + 1e-10) + 
    actual_df$unimodal * log(p_unimodal + 1e-10)
  
  return(-sum(ll))
}

# Estimate enhanced model
initial_pars_enhanced <- rep(0, 20)
enhanced_model <- optimx(initial_pars_enhanced, illkhd_enhanced, method = "BFGS", 
                         control = list(trace = 1, maxit = 1000, reltol = 1e-8))

# ======================================================
# 4. MODEL VALIDATION AND PREDICTION
# ======================================================

# Calculate utility for multimodal
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
  best_pars[17]*actual_df$age45*actual_df$DISCTRNH*actual_df$DISCBUSH + 
  best_pars[18]*actual_df$drvknow + 
  best_pars[19]*actual_df$comoth*actual_df$female + 
  best_pars[20]*actual_df$comothm + 
  best_pars[21]*actual_df$trainnear*actual_df$scaptive_mar20 +
  best_pars[22]*actual_df$direct_bus +
  best_pars[23]*actual_df$car_cost_per_km +
  best_pars[24]*actual_df$travel_time

# Calculate predicted probabilities with numerical stability
actual_df$predicted_probs <- 1 / (1 + exp(-actual_df$U_multimodal_pred))

# Classification accuracy
actual_df$predicted_class <- ifelse(actual_df$predicted_probs > 0.5, 1, 0)
accuracy <- mean(actual_df$predicted_class == actual_df$multimodal, na.rm = TRUE)
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")

# Confusion matrix
confusion_matrix <- table(Actual = actual_df$multimodal, Predicted = actual_df$predicted_class)
print("Confusion Matrix:")
print(confusion_matrix)

# ======================================================
# 5. SEGMENTATION ANALYSIS
# ======================================================

# Create predicted modality for segmentation
actual_df$pred_unimodal <- ifelse(actual_df$predicted_probs <= 0.5, 1, 0)
actual_df$pred_multimodal <- ifelse(actual_df$predicted_probs > 0.5, 1, 0)

# Analyze modality by different segments
segment_analysis <- actual_df %>%
  group_by(vehicle_class = vehicleclass, income_group = ifelse(hhinc < 5, "Low", "High")) %>%
  summarise(
    n = n(),
    actual_multimodal_rate = mean(multimodal, na.rm = TRUE) * 100,
    predicted_multimodal_rate = mean(predicted_probs, na.rm = TRUE) * 100,
    avg_distance = mean(lwttdist_mar20, na.rm = TRUE),
    .groups = 'drop'
  )

print("Segmentation Analysis:")
print(segment_analysis)

# ======================================================
# 6. POLICY IMPLICATIONS
# ======================================================

# Calculate policy effects
calculate_policy_effect <- function(variable, data, pars) {
  baseline_prob <- mean(data$predicted_probs, na.rm = TRUE)
  
  # Create improved scenario
  improved_data <- data
  improved_data[[variable]] <- 1  # Assume policy is implemented
  
  # Calculate new utility
  U_improved <- pars[1] + 
    pars[2]*improved_data$BUSSECL + 
    pars[3]*improved_data$STANDTRNH + 
    pars[4]*improved_data$BUSRELL +
    pars[5]*improved_data$PPMISS + 
    pars[6]*improved_data$oddtim + 
    pars[7]*improved_data$PickDrop*improved_data$kids +
    pars[8]*improved_data$wdur8 + 
    pars[9]*improved_data$wdur810 + 
    pars[10]*improved_data$hbus500 + 
    pars[11]*improved_data$female +
    pars[12]*improved_data$novehicle*improved_data$wrkdis2 + 
    pars[13]*improved_data$hurban + 
    pars[14]*improved_data$info + 
    pars[15]*improved_data$TWRELH*improved_data$n2w_mar20 + 
    pars[16]*improved_data$BUSCRWDM*improved_data$TRNCRWDM +
    pars[17]*improved_data$age45*improved_data$DISCTRNH*improved_data$DISCBUSH + 
    pars[18]*improved_data$drvknow + 
    pars[19]*improved_data$comoth*improved_data$female + 
    pars[20]*improved_data$comothm + 
    pars[21]*improved_data$trainnear*improved_data$scaptive_mar20 +
    pars[22]*improved_data$direct_bus +
    pars[23]*improved_data$car_cost_per_km +
    pars[24]*improved_data$travel_time
  
  improved_probs <- 1 / (1 + exp(-U_improved))
  improved_prob_mean <- mean(improved_probs, na.rm = TRUE)
  
  effect <- (improved_prob_mean - baseline_prob) * 100
  return(effect)
}

# Calculate effects for key policy variables
policy_vars <- c("PT_infra_quality", "integrated_ticketing", "multimodal_info")
policy_effects <- sapply(policy_vars, calculate_policy_effect, 
                         data = actual_df, pars = best_pars)

policy_implications <- data.frame(
  Policy_Variable = policy_vars,
  Effect_Percentage = policy_effects
)

print("Policy Effects (% change in multimodal probability):")
print(policy_implications)

# ======================================================
# 7. VISUALIZATION
# ======================================================

# Simple scatter plot
plot(actual_df$lwttdist_mar20, actual_df$predicted_probs, 
     col = ifelse(actual_df$multimodal == 1, "blue", "red"),
     xlab = "Distance (meters)", 
     ylab = "Predicted Probability of Multimodality",
     main = "Actual vs Predicted Modality Choice by Distance")
legend("topright", legend = c("Multimodal", "Unimodal"), 
       col = c("blue", "red"), pch = 1)

# Plot distribution of predicted probabilities
hist(actual_df$predicted_probs, breaks = 20, 
     main = "Distribution of Predicted Probabilities",
     xlab = "Predicted Probability of Multimodality",
     col = "lightblue")