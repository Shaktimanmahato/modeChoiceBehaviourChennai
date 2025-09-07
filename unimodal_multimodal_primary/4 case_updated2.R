# ======================================================
# FOUR CASES FOR SIGMA AND BETA EQUALITY TESTS
# FOR PRIMARY MODE CHOICE: UNIMODAL vs MULTIMODAL
# ======================================================

# Load required libraries
library(apollo)
library(dplyr)

# Clear memory
rm(list = ls())

# Load and prepare data
actual_df <- read.csv("D:\\Project code\\006_hyperpath_ganesh_revised_csv_679_rev_rem_ipt_nmt (1).csv")

actual_df <- actual_df %>%
  mutate(
    twhh = ifelse(n2w_mar20 > 0, 1, 0),
    female = 1 - gen,
    inc40_more = ifelse(hhinc >= 6, 1, 0),
    wrkdis10 = ifelse(lwttdist_mar20 > 10000, 1, 0),
    is_multimodal = ifelse(pred_multimodal == 1, 1, 0),
    nmttt1 = (wlktt1_mar20 + cyctt1_mar20) / 2,
    pri_choice = apply(dplyr::select(., tw_p, car_p, bus_p, trn_p, ipt1_p, ipt2_p, nmt_p), 1, which.max),
    tw_avail = 1, car_avail = 1, bus_avail = 1, trn_avail = 1,
    ipt1_avail = 1, ipt2_avail = 1, nmt_avail = 1
  )

# ======================================================
# CASE 1: SAME SIGMA AND SAME BETA (σ=1, β=1)
# ======================================================

cat("Case1:\n")
cat("1. Heteroscedastic model output , Value of sigma and beta , MODEL FIT\n")

apollo_initialise()
apollo_control <- list(
  modelName = "Case1_SameSigmaSameBeta",
  modelDescr = "Same sigma (σ=1) and same beta (β=1) for both groups",
  indivID = "id",
  outputDirectory = "output"
)

apollo_beta_case1 <- c(
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0
)

apollo_fixed_case1 <- c("asc_ipt1")
apollo_inputs_case1 <- apollo_validateInputs(apollo_beta_case1, apollo_fixed_case1, database = actual_df)

apollo_probabilities_case1 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Same utility specification for both groups (same beta, σ=1)
  V[["tw"]] = asc_tw + b_tt*twlrtt1_mar20 + b_tw_los1*(TWRELM + TWRELH)*twhh + b_tw_pick*PickDrop
  V[["car"]] = asc_car + b_tt*cartt1_mar20 + b_co1*carco2_mar20 + b_car_pick*PickDrop
  V[["bus"]] = asc_bus + b_tt*bustt1_mar20 + b_co1*busco2_mar20 + b_bus_cap*captive_mar20 + b_bus_scap*scaptive_mar20 + b_inc_pt*inc40_more + b_los1_pt*BEASEGET + b_bus_los2*dbus
  V[["trn"]] = asc_trn + b_tt*tratt1_mar20 + b_co1*traco2_mar20 + b_wdis10_pt*wrkdis10 + b_trn_scap*scaptive_mar20 + b_inc_pt*inc40_more + b_los1_pt*TEASEGET + b_trn_los2*Railhom_tide + b_trn_los3*Railoff_tide + b_cont1_trn*vismp_new
  V[["ipt1"]] = asc_ipt1 + b_co1*autoco2_mar20 + b_fem_ipt*female
  V[["ipt2"]] = asc_ipt2 + b_ipt2_scap*scaptive_mar20 + b_fem_ipt*female + b_ipt2_odd*oddtim
  V[["nmt"]] = asc_nmt + b_tt*nmttt1 + b_nmt_scap*scaptive_mar20 + b_nmt_odd*oddtim
  
  mnl_settings <- list(
    alternatives = c(tw=1, car=2, bus=3, trn=4, ipt1=5, ipt2=6, nmt=7),
    avail = list(tw=tw_avail, car=car_avail, bus=bus_avail, trn=trn_avail, ipt1=ipt1_avail, ipt2=ipt2_avail, nmt=nmt_avail),
    choiceVar = pri_choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

case1_model <- apollo_estimate(apollo_beta_case1, apollo_fixed_case1, apollo_probabilities_case1, apollo_inputs_case1)

# Display results for Case 1
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta: Same for both groups\n")
cat("MODEL FIT:\n")
print(case1_model$LLout)
cat("\n")

cat("2. Primary mode choice unimodal output ,Value of sigma and beta , MODEL FIT\n")
# For Case 1, unimodal and multimodal have same parameters
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta: Same as above\n")
cat("MODEL FIT: Same as above (combined model)\n")
cat("\n")

cat("3. Primary mode choice multimodal output ,Value of sigma and beta , MODEL FIT\n")
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta: Same as above\n")
cat("MODEL FIT: Same as above (combined model)\n")
cat("\n")

# ======================================================
# CASE 2: SAME SIGMA BUT DIFFERENT BETA (σ=1, β≠1)
# ======================================================

cat("Case2:\n")
cat("1. Heteroscedastic model output , Value of sigma and beta , MODEL FIT\n")

apollo_initialise()
apollo_control <- list(
  modelName = "Case2_SameSigmaDiffBeta",
  modelDescr = "Same sigma (σ=1) but different beta (β≠1) for multimodal",
  indivID = "id",
  outputDirectory = "output"
)

apollo_beta_case2 <- c(
  # Base parameters (for unimodal)
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0,
  
  # Differential parameters for multimodal (β_multi - β_uni)
  d_asc_tw = 0, d_asc_car = 0, d_asc_bus = 0, d_asc_trn = 0, 
  d_asc_ipt1 = 0, d_asc_ipt2 = 0, d_asc_nmt = 0,
  d_b_tt = 0, d_b_co1 = 0,
  d_b_tw_pick = 0, d_b_car_pick = 0,
  d_b_bus_cap = 0, d_b_bus_scap = 0, d_b_trn_scap = 0, d_b_nmt_scap = 0, 
  d_b_ipt2_scap = 0, d_b_fem_ipt = 0, d_b_inc_pt = 0, d_b_wdis10_pt = 0,
  d_b_ipt2_odd = 0, d_b_nmt_odd = 0, d_b_tw_los1 = 0, d_b_los1_pt = 0, 
  d_b_bus_los2 = 0, d_b_trn_los2 = 0, d_b_trn_los3 = 0, d_b_cont1_trn = 0
)

apollo_fixed_case2 <- c("asc_ipt1", "d_asc_ipt1")
apollo_inputs_case2 <- apollo_validateInputs(apollo_beta_case2, apollo_fixed_case2, database = actual_df)

apollo_probabilities_case2 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Utility for unimodal (base parameters)
  # Utility for multimodal (base + differential parameters)
  V[["tw"]] = asc_tw + d_asc_tw*is_multimodal + 
    (b_tt + d_b_tt*is_multimodal)*twlrtt1_mar20 + 
    (b_tw_los1 + d_b_tw_los1*is_multimodal)*(TWRELM + TWRELH)*twhh + 
    (b_tw_pick + d_b_tw_pick*is_multimodal)*PickDrop
  
  V[["car"]] = asc_car + d_asc_car*is_multimodal + 
    (b_tt + d_b_tt*is_multimodal)*cartt1_mar20 + 
    (b_co1 + d_b_co1*is_multimodal)*carco2_mar20 + 
    (b_car_pick + d_b_car_pick*is_multimodal)*PickDrop
  
  V[["bus"]] = asc_bus + d_asc_bus*is_multimodal + 
    (b_tt + d_b_tt*is_multimodal)*bustt1_mar20 + 
    (b_co1 + d_b_co1*is_multimodal)*busco2_mar20 + 
    (b_bus_cap + d_b_bus_cap*is_multimodal)*captive_mar20 + 
    (b_bus_scap + d_b_bus_scap*is_multimodal)*scaptive_mar20 + 
    (b_inc_pt + d_b_inc_pt*is_multimodal)*inc40_more + 
    (b_los1_pt + d_b_los1_pt*is_multimodal)*BEASEGET + 
    (b_bus_los2 + d_b_bus_los2*is_multimodal)*dbus
  
  V[["trn"]] = asc_trn + d_asc_trn*is_multimodal + 
    (b_tt + d_b_tt*is_multimodal)*tratt1_mar20 + 
    (b_co1 + d_b_co1*is_multimodal)*traco2_mar20 + 
    (b_wdis10_pt + d_b_wdis10_pt*is_multimodal)*wrkdis10 + 
    (b_trn_scap + d_b_trn_scap*is_multimodal)*scaptive_mar20 + 
    (b_inc_pt + d_b_inc_pt*is_multimodal)*inc40_more + 
    (b_los1_pt + d_b_los1_pt*is_multimodal)*TEASEGET + 
    (b_trn_los2 + d_b_trn_los2*is_multimodal)*Railhom_tide + 
    (b_trn_los3 + d_b_trn_los3*is_multimodal)*Railoff_tide + 
    (b_cont1_trn + d_b_cont1_trn*is_multimodal)*vismp_new
  
  V[["ipt1"]] = asc_ipt1 + d_asc_ipt1*is_multimodal + 
    (b_co1 + d_b_co1*is_multimodal)*autoco2_mar20 + 
    (b_fem_ipt + d_b_fem_ipt*is_multimodal)*female
  
  V[["ipt2"]] = asc_ipt2 + d_asc_ipt2*is_multimodal + 
    (b_ipt2_scap + d_b_ipt2_scap*is_multimodal)*scaptive_mar20 + 
    (b_fem_ipt + d_b_fem_ipt*is_multimodal)*female + 
    (b_ipt2_odd + d_b_ipt2_odd*is_multimodal)*oddtim
  
  V[["nmt"]] = asc_nmt + d_asc_nmt*is_multimodal + 
    (b_tt + d_b_tt*is_multimodal)*nmttt1 + 
    (b_nmt_scap + d_b_nmt_scap*is_multimodal)*scaptive_mar20 + 
    (b_nmt_odd + d_b_nmt_odd*is_multimodal)*oddtim
  
  mnl_settings <- list(
    alternatives = c(tw=1, car=2, bus=3, trn=4, ipt1=5, ipt2=6, nmt=7),
    avail = list(tw=tw_avail, car=car_avail, bus=bus_avail, trn=trn_avail, ipt1=ipt1_avail, ipt2=ipt2_avail, nmt=nmt_avail),
    choiceVar = pri_choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

case2_model <- apollo_estimate(apollo_beta_case2, apollo_fixed_case2, apollo_probabilities_case2, apollo_inputs_case2)

# Display results for Case 2
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta: Different for multimodal\n")
cat("MODEL FIT:\n")
print(case2_model$LLout)
cat("\n")

cat("2. Primary mode choice unimodal output ,Value of sigma and beta , MODEL FIT\n")
# Extract unimodal parameters (base parameters)
unimodal_params_case2 <- case2_model$estimate[!grepl("^d_", names(case2_model$estimate))]
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta:\n")
print(unimodal_params_case2)
cat("MODEL FIT: Part of combined model\n")
cat("\n")

cat("3. Primary mode choice multimodal output ,Value of sigma and beta , MODEL FIT\n")
# Extract multimodal parameters (base + differential)
multimodal_params_case2 <- case2_model$estimate
base_params <- multimodal_params_case2[!grepl("^d_", names(multimodal_params_case2))]
diff_params <- multimodal_params_case2[grepl("^d_", names(multimodal_params_case2))]

# Calculate multimodal parameters
for(param_name in names(diff_params)) {
  base_param_name <- gsub("^d_", "", param_name)
  if(base_param_name %in% names(base_params)) {
    multimodal_params_case2[base_param_name] <- base_params[base_param_name] + diff_params[param_name]
  }
}
multimodal_params_case2 <- multimodal_params_case2[!grepl("^d_", names(multimodal_params_case2))]

cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta:\n")
print(multimodal_params_case2)
cat("MODEL FIT: Part of combined model\n")
cat("\n")

# ======================================================
# CASE 3: DIFFERENT SIGMA BUT SAME BETA (σ≠1, β=1)
# ======================================================

cat("Case3:\n")
cat("1. Heteroscedastic model output , Value of sigma and beta , MODEL FIT\n")

apollo_initialise()
apollo_control <- list(
  modelName = "Case3_DiffSigmaSameBeta",
  modelDescr = "Different sigma (σ≠1) but same beta (β=1) for both groups",
  indivID = "id",
  outputDirectory = "output"
)

apollo_beta_case3 <- c(
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0,
  sigma_multi = 1  # Scale parameter for multimodal (σ_multi)
)

apollo_fixed_case3 <- c("asc_ipt1")
apollo_inputs_case3 <- apollo_validateInputs(apollo_beta_case3, apollo_fixed_case3, database = actual_df)

apollo_probabilities_case3 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Define scale parameter: 1 for unimodal, sigma_multi for multimodal
  scale_factor = 1 * (1 - is_multimodal) + sigma_multi * is_multimodal
  
  # Same utility specification for both groups (same beta)
  V[["tw"]] = (asc_tw + b_tt*twlrtt1_mar20 + b_tw_los1*(TWRELM + TWRELH)*twhh + b_tw_pick*PickDrop) * scale_factor
  V[["car"]] = (asc_car + b_tt*cartt1_mar20 + b_co1*carco2_mar20 + b_car_pick*PickDrop) * scale_factor
  V[["bus"]] = (asc_bus + b_tt*bustt1_mar20 + b_co1*busco2_mar20 + b_bus_cap*captive_mar20 + b_bus_scap*scaptive_mar20 + b_inc_pt*inc40_more + b_los1_pt*BEASEGET + b_bus_los2*dbus) * scale_factor
  V[["trn"]] = (asc_trn + b_tt*tratt1_mar20 + b_co1*traco2_mar20 + b_wdis10_pt*wrkdis10 + b_trn_scap*scaptive_mar20 + b_inc_pt*inc40_more + b_los1_pt*TEASEGET + b_trn_los2*Railhom_tide + b_trn_los3*Railoff_tide + b_cont1_trn*vismp_new) * scale_factor
  V[["ipt1"]] = (asc_ipt1 + b_co1*autoco2_mar20 + b_fem_ipt*female) * scale_factor
  V[["ipt2"]] = (asc_ipt2 + b_ipt2_scap*scaptive_mar20 + b_fem_ipt*female + b_ipt2_odd*oddtim) * scale_factor
  V[["nmt"]] = (asc_nmt + b_tt*nmttt1 + b_nmt_scap*scaptive_mar20 + b_nmt_odd*oddtim) * scale_factor
  
  mnl_settings <- list(
    alternatives = c(tw=1, car=2, bus=3, trn=4, ipt1=5, ipt2=6, nmt=7),
    avail = list(tw=tw_avail, car=car_avail, bus=bus_avail, trn=trn_avail, ipt1=ipt1_avail, ipt2=ipt2_avail, nmt=nmt_avail),
    choiceVar = pri_choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

case3_model <- apollo_estimate(apollo_beta_case3, apollo_fixed_case3, apollo_probabilities_case3, apollo_inputs_case3)

# Display results for Case 3
sigma_multi_case3 <- case3_model$estimate["sigma_multi"]
cat("Value of sigma:", sigma_multi_case3, "\n")
cat("Value of beta: Same for both groups\n")
cat("MODEL FIT:\n")
print(case3_model$LLout)
cat("\n")

cat("2. Primary mode choice unimodal output ,Value of sigma and beta , MODEL FIT\n")
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta: Same as above\n")
cat("MODEL FIT: Part of combined model\n")
cat("\n")

cat("3. Primary mode choice multimodal output ,Value of sigma and beta , MODEL FIT\n")
cat("Value of sigma:", sigma_multi_case3, "\n")
cat("Value of beta: Same as above\n")
cat("MODEL FIT: Part of combined model\n")
cat("\n")

# ======================================================
# CASE 4: DIFFERENT SIGMA AND DIFFERENT BETA (σ≠1, β≠1)
# ======================================================

cat("Case4:\n")
cat("1. Heteroscedastic model output , Value of sigma and beta , MODEL FIT\n")

apollo_initialise()
apollo_control <- list(
  modelName = "Case4_DiffSigmaDiffBeta",
  modelDescr = "Different sigma (σ≠1) and different beta (β≠1) for multimodal",
  indivID = "id",
  outputDirectory = "output"
)

apollo_beta_case4 <- c(
  # Base parameters (for unimodal)
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0,
  
  # Differential parameters for multimodal (β_multi - β_uni)
  d_asc_tw = 0, d_asc_car = 0, d_asc_bus = 0, d_asc_trn = 0, 
  d_asc_ipt1 = 0, d_asc_ipt2 = 0, d_asc_nmt = 0,
  d_b_tt = 0, d_b_co1 = 0,
  d_b_tw_pick = 0, d_b_car_pick = 0,
  d_b_bus_cap = 0, d_b_bus_scap = 0, d_b_trn_scap = 0, d_b_nmt_scap = 0, 
  d_b_ipt2_scap = 0, d_b_fem_ipt = 0, d_b_inc_pt = 0, d_b_wdis10_pt = 0,
  d_b_ipt2_odd = 0, d_b_nmt_odd = 0, d_b_tw_los1 = 0, d_b_los1_pt = 0, 
  d_b_bus_los2 = 0, d_b_trn_los2 = 0, d_b_trn_los3 = 0, d_b_cont1_trn = 0,
  
  sigma_multi = 1  # Scale parameter for multimodal (σ_multi)
)

apollo_fixed_case4 <- c("asc_ipt1", "d_asc_ipt1")
apollo_inputs_case4 <- apollo_validateInputs(apollo_beta_case4, apollo_fixed_case4, database = actual_df)

apollo_probabilities_case4 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Define scale parameter: 1 for unimodal, sigma_multi for multimodal
  scale_factor = 1 * (1 - is_multimodal) + sigma_multi * is_multimodal
  
  # Utility specification with differential parameters for multimodal
  V[["tw"]] = (asc_tw + d_asc_tw*is_multimodal + 
                 (b_tt + d_b_tt*is_multimodal)*twlrtt1_mar20 + 
                 (b_tw_los1 + d_b_tw_los1*is_multimodal)*(TWRELM + TWRELH)*twhh + 
                 (b_tw_pick + d_b_tw_pick*is_multimodal)*PickDrop) * scale_factor
  
  V[["car"]] = (asc_car + d_asc_car*is_multimodal + 
                  (b_tt + d_b_tt*is_multimodal)*cartt1_mar20 + 
                  (b_co1 + d_b_co1*is_multimodal)*carco2_mar20 + 
                  (b_car_pick + d_b_car_pick*is_multimodal)*PickDrop) * scale_factor
  
  V[["bus"]] = (asc_bus + d_asc_bus*is_multimodal + 
                  (b_tt + d_b_tt*is_multimodal)*bustt1_mar20 + 
                  (b_co1 + d_b_co1*is_multimodal)*busco2_mar20 + 
                  (b_bus_cap + d_b_bus_cap*is_multimodal)*captive_mar20 + 
                  (b_bus_scap + d_b_bus_scap*is_multimodal)*scaptive_mar20 + 
                  (b_inc_pt + d_b_inc_pt*is_multimodal)*inc40_more + 
                  (b_los1_pt + d_b_los1_pt*is_multimodal)*BEASEGET + 
                  (b_bus_los2 + d_b_bus_los2*is_multimodal)*dbus) * scale_factor
  
  V[["trn"]] = (asc_trn + d_asc_trn*is_multimodal + 
                  (b_tt + d_b_tt*is_multimodal)*tratt1_mar20 + 
                  (b_co1 + d_b_co1*is_multimodal)*traco2_mar20 + 
                  (b_wdis10_pt + d_b_wdis10_pt*is_multimodal)*wrkdis10 + 
                  (b_trn_scap + d_b_trn_scap*is_multimodal)*scaptive_mar20 + 
                  (b_inc_pt + d_b_inc_pt*is_multimodal)*inc40_more + 
                  (b_los1_pt + d_b_los1_pt*is_multimodal)*TEASEGET + 
                  (b_trn_los2 + d_b_trn_los2*is_multimodal)*Railhom_tide + 
                  (b_trn_los3 + d_b_trn_los3*is_multimodal)*Railoff_tide + 
                  (b_cont1_trn + d_b_cont1_trn*is_multimodal)*vismp_new) * scale_factor
  
  V[["ipt1"]] = (asc_ipt1 + d_asc_ipt1*is_multimodal + 
                   (b_co1 + d_b_co1*is_multimodal)*autoco2_mar20 + 
                   (b_fem_ipt + d_b_fem_ipt*is_multimodal)*female) * scale_factor
  
  V[["ipt2"]] = (asc_ipt2 + d_asc_ipt2*is_multimodal + 
                   (b_ipt2_scap + d_b_ipt2_scap*is_multimodal)*scaptive_mar20 + 
                   (b_fem_ipt + d_b_fem_ipt*is_multimodal)*female + 
                   (b_ipt2_odd + d_b_ipt2_odd*is_multimodal)*oddtim) * scale_factor
  
  V[["nmt"]] = (asc_nmt + d_asc_nmt*is_multimodal + 
                  (b_tt + d_b_tt*is_multimodal)*nmttt1 + 
                  (b_nmt_scap + d_b_nmt_scap*is_multimodal)*scaptive_mar20 + 
                  (b_nmt_odd + d_b_nmt_odd*is_multimodal)*oddtim) * scale_factor
  
  mnl_settings <- list(
    alternatives = c(tw=1, car=2, bus=3, trn=4, ipt1=5, ipt2=6, nmt=7),
    avail = list(tw=tw_avail, car=car_avail, bus=bus_avail, trn=trn_avail, ipt1=ipt1_avail, ipt2=ipt2_avail, nmt=nmt_avail),
    choiceVar = pri_choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

case4_model <- apollo_estimate(apollo_beta_case4, apollo_fixed_case4, apollo_probabilities_case4, apollo_inputs_case4)

# Display results for Case 4
sigma_multi_case4 <- case4_model$estimate["sigma_multi"]
cat("Value of sigma:", sigma_multi_case4, "\n")
cat("Value of beta: Different for multimodal\n")
cat("MODEL FIT:\n")
print(case4_model$LLout)
cat("\n")

cat("2. Primary mode choice unimodal output ,Value of sigma and beta , MODEL FIT\n")
# Extract unimodal parameters (base parameters)
unimodal_params_case4 <- case4_model$estimate[!grepl("^d_", names(case4_model$estimate))]
unimodal_params_case4 <- unimodal_params_case4[names(unimodal_params_case4) != "sigma_multi"]
cat("Value of sigma: 1 (fixed)\n")
cat("Value of beta:\n")
print(unimodal_params_case4)
cat("MODEL FIT: Part of combined model\n")
cat("\n")

cat("3. Primary mode choice multimodal output ,Value of sigma and beta , MODEL FIT\n")
# Extract multimodal parameters (base + differential)
multimodal_params_case4 <- case4_model$estimate
base_params <- multimodal_params_case4[!grepl("^d_", names(multimodal_params_case4))]
diff_params <- multimodal_params_case4[grepl("^d_", names(multimodal_params_case4))]

# Calculate multimodal parameters
for(param_name in names(diff_params)) {
  base_param_name <- gsub("^d_", "", param_name)
  if(base_param_name %in% names(base_params)) {
    multimodal_params_case4[base_param_name] <- base_params[base_param_name] + diff_params[param_name]
  }
}
multimodal_params_case4 <- multimodal_params_case4[!grepl("^d_", names(multimodal_params_case4))]

cat("Value of sigma:", sigma_multi_case4, "\n")
cat("Value of beta:\n")
print(multimodal_params_case4)
cat("MODEL FIT: Part of combined model\n")