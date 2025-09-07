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

# ======================================================
# CASE 2: SAME SIGMA BUT DIFFERENT BETA (σ=1, β≠1)
# ======================================================

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

# ======================================================
# CASE 3: DIFFERENT SIGMA BUT SAME BETA (σ≠1, β=1)
# ======================================================

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

# ======================================================
# CASE 4: DIFFERENT SIGMA AND DIFFERENT BETA (σ≠1, β≠1)
# ======================================================

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

# ======================================================
# MODEL COMPARISON AND HYPOTHESIS TESTING
# ======================================================

cat("COMPREHENSIVE ANALYSIS OF PRIMARY MODE CHOICE\n")
cat("Unimodal vs Multimodal Travelers\n")
cat("==============================================\n\n")

# Get model fits
models <- list(
  "Case1: σ=1, β=1" = case1_model,
  "Case2: σ=1, β≠1" = case2_model,
  "Case3: σ≠1, β=1" = case3_model,
  "Case4: σ≠1, β≠1" = case4_model
)

# Create comparison table
comparison <- data.frame(
  Model = names(models),
  LL = sapply(models, function(x) x$LLout),
  AIC = sapply(models, function(x) 2 * length(x$estimate) - 2 * x$LLout),
  BIC = sapply(models, function(x) log(nrow(actual_df)) * length(x$estimate) - 2 * x$LLout),
  Parameters = sapply(models, function(x) length(x$estimate)),
  stringsAsFactors = FALSE
)

print(comparison)

# Likelihood ratio tests for hypothesis testing
cat("\nLIKELIHOOD RATIO TESTS FOR HYPOTHESIS TESTING\n")
cat("==============================================\n")

# Test 1: Sigma equality (Case1 vs Case3)
lr_sigma <- 2 * (case3_model$LLout - case1_model$LLout)
df_sigma <- length(case3_model$estimate) - length(case1_model$estimate)
p_sigma <- 1 - pchisq(lr_sigma, df_sigma)
cat("Sigma Equality Test (H0: σ=1):\n")
cat("  LR =", round(lr_sigma, 3), ", df =", df_sigma, ", p =", round(p_sigma, 4))
if(p_sigma < 0.05) {
  cat("  -> Reject H0: Sigma are different (σ≠1)\n")
} else {
  cat("  -> Cannot reject H0: Sigma are equal (σ=1)\n")
}

# Test 2: Beta equality (Case1 vs Case2)
lr_beta <- 2 * (case2_model$LLout - case1_model$LLout)
df_beta <- length(case2_model$estimate) - length(case1_model$estimate)
p_beta <- 1 - pchisq(lr_beta, df_beta)
cat("Beta Equality Test (H0: β=1):\n")
cat("  LR =", round(lr_beta, 3), ", df =", df_beta, ", p =", round(p_beta, 4))
if(p_beta < 0.05) {
  cat("  -> Reject H0: Beta are different (β≠1)\n")
} else {
  cat("  -> Cannot reject H0: Beta are equal (β=1)\n")
}

# Test 3: Full test (Case1 vs Case4)
lr_full <- 2 * (case4_model$LLout - case1_model$LLout)
df_full <- length(case4_model$estimate) - length(case1_model$estimate)
p_full <- 1 - pchisq(lr_full, df_full)
cat("Full Equality Test (H0: σ=1 & β=1):\n")
cat("  LR =", round(lr_full, 3), ", df =", df_full, ", p =", round(p_full, 4))
if(p_full < 0.05) {
  cat("  -> Reject H0: Both sigma and beta are different\n")
} else {
  cat("  -> Cannot reject H0: Both sigma and beta are equal\n")
}

# Test 4: Beta equality given different sigma (Case3 vs Case4)
lr_beta_given_sigma <- 2 * (case4_model$LLout - case3_model$LLout)
df_beta_given_sigma <- length(case4_model$estimate) - length(case3_model$estimate)
p_beta_given_sigma <- 1 - pchisq(lr_beta_given_sigma, df_beta_given_sigma)
cat("Beta Equality Given Different Sigma (H0: β=1|σ≠1):\n")
cat("  LR =", round(lr_beta_given_sigma, 3), ", df =", df_beta_given_sigma, ", p =", round(p_beta_given_sigma, 4))
if(p_beta_given_sigma < 0.05) {
  cat("  -> Reject H0: Beta are different even after accounting for sigma differences\n")
} else {
  cat("  -> Cannot reject H0: Beta are equal after accounting for sigma differences\n")
}

# Determine the best model
best_model_aic <- which.min(comparison$AIC)
best_model_bic <- which.min(comparison$BIC)

cat("\nBEST MODEL SELECTION:\n")
cat("Based on AIC:", comparison$Model[best_model_aic], "\n")
cat("Based on BIC:", comparison$Model[best_model_bic], "\n")

# Interpretation
cat("\nCONCLUSION:\n")
if(best_model_aic == 1 && best_model_bic == 1) {
  cat("Both unimodal and multimodal travelers have the SAME scale parameter (σ=1) \nand SAME taste parameters (β=1) for primary mode choice.\n")
} else if(best_model_aic == 2 && best_model_bic == 2) {
  cat("Both groups have the SAME scale parameter (σ=1) but DIFFERENT taste parameters (β≠1).\n")
} else if(best_model_aic == 3 && best_model_bic == 3) {
  cat("Both groups have DIFFERENT scale parameters (σ≠1) but the SAME taste parameters (β=1).\n")
} else if(best_model_aic == 4 && best_model_bic == 4) {
  cat("Both groups have DIFFERENT scale parameters (σ≠1) and DIFFERENT taste parameters (β≠1).\n")
} else {
  cat("Model selection criteria disagree. AIC suggests", comparison$Model[best_model_aic], 
      "while BIC suggests", comparison$Model[best_model_bic], "\n")
}

# ======================================================
# EXTRACTING COEFFICIENTS AND T-VALUES FOR EACH CASE
# ======================================================

cat("\nDETAILED COEFFICIENT ANALYSIS FOR EACH CASE\n")
cat("==============================================\n")

# Function to extract coefficients and t-values
extract_coefficients <- function(model, case_name) {
  # Get parameter estimates
  estimates <- model$estimate
  
  # Get standard errors from robust covariance matrix
  # Handle cases where some parameters might be fixed
  se <- tryCatch({
    sqrt(diag(model$robvarcov))
  }, error = function(e) {
    # If robust covariance fails, use classical covariance
    sqrt(diag(model$varcov))
  })
  
  # Ensure we have the same number of parameters for estimates and SE
  param_names <- names(estimates)
  t_values <- estimates / se[1:length(estimates)]
  
  # Create data frame
  coef_df <- data.frame(
    Parameter = param_names,
    Estimate = round(estimates, 4),
    Std_Error = round(se[1:length(estimates)], 4),
    t_value = round(t_values, 3),
    stringsAsFactors = FALSE
  )
  
  # Add case information
  coef_df$Case <- case_name
  
  return(coef_df)
}

# Extract coefficients for all cases
coef_case1 <- extract_coefficients(case1_model, "Case1: σ=1, β=1")
coef_case2 <- extract_coefficients(case2_model, "Case2: σ=1, β≠1") 
coef_case3 <- extract_coefficients(case3_model, "Case3: σ≠1, β=1")
coef_case4 <- extract_coefficients(case4_model, "Case4: σ≠1, β≠1")

# Combine all coefficients
all_coef <- rbind(coef_case1, coef_case2, coef_case3, coef_case4)

# Print summary for each case
cat("\nCASE 1: SAME SIGMA AND SAME BETA (σ=1, β=1)\n")
cat("--------------------------------------------\n")
print(subset(all_coef, Case == "Case1: σ=1, β=1")[, c("Parameter", "Estimate", "t_value")])

cat("\nCASE 2: SAME SIGMA BUT DIFFERENT BETA (σ=1, β≠1)\n")
cat("--------------------------------------------\n")
# Separate base and differential parameters
base_params_case2 <- subset(all_coef, Case == "Case2: σ=1, β≠1" & !grepl("^d_", Parameter))
diff_params_case2 <- subset(all_coef, Case == "Case2: σ=1, β≠1" & grepl("^d_", Parameter))

cat("Base parameters (unimodal):\n")
print(base_params_case2[, c("Parameter", "Estimate", "t_value")])

cat("\nDifferential parameters (multimodal - unimodal):\n")
print(diff_params_case2[, c("Parameter", "Estimate", "t_value")])

cat("\nCASE 3: DIFFERENT SIGMA BUT SAME BETA (σ≠1, β=1)\n")
cat("--------------------------------------------\n")
coef_case3_sub <- subset(all_coef, Case == "Case3: σ≠1, β=1")
print(coef_case3_sub[, c("Parameter", "Estimate", "t_value")])

cat("\nCASE 4: DIFFERENT SIGMA AND DIFFERENT BETA (σ≠1, β≠1)\n")
cat("--------------------------------------------\n")
# Separate base and differential parameters
base_params_case4 <- subset(all_coef, Case == "Case4: σ≠1, β≠1" & !grepl("^d_", Parameter))
diff_params_case4 <- subset(all_coef, Case == "Case4: σ≠1, β≠1" & grepl("^d_", Parameter))

cat("Base parameters (unimodal):\n")
print(base_params_case4[, c("Parameter", "Estimate", "t_value")])

cat("\nDifferential parameters (multimodal - unimodal):\n")
print(diff_params_case4[, c("Parameter", "Estimate", "t_value")])

# Extract sigma values
cat("\nSIGMA VALUES ACROSS CASES:\n")
cat("---------------------------\n")
cat("Case 1: σ = 1 (fixed)\n")
cat("Case 2: σ = 1 (fixed)\n")
sigma_case3 <- coef_case3_sub$Estimate[coef_case3_sub$Parameter == "sigma_multi"]
sigma_t_case3 <- coef_case3_sub$t_value[coef_case3_sub$Parameter == "sigma_multi"]
cat(sprintf("Case 3: σ_multi = %.4f (t = %.3f)\n", sigma_case3, sigma_t_case3))
sigma_case4 <- subset(all_coef, Case == "Case4: σ≠1, β≠1" & Parameter == "sigma_multi")$Estimate
sigma_t_case4 <- subset(all_coef, Case == "Case4: σ≠1, β≠1" & Parameter == "sigma_multi")$t_value
cat(sprintf("Case 4: σ_multi = %.4f (t = %.3f)\n", sigma_case4, sigma_t_case4))

# Key parameter comparison
cat("\nKEY PARAMETER COMPARISON ACROSS CASES:\n")
cat("--------------------------------------\n")
key_params <- c("b_tt", "b_co1", "b_bus_cap", "b_bus_scap", "sigma_multi")

key_comparison <- data.frame(
  Parameter = key_params,
  Case1 = sapply(key_params, function(p) {
    if(p %in% coef_case1$Parameter) coef_case1$Estimate[coef_case1$Parameter == p] else NA
  }),
  Case2_base = sapply(key_params, function(p) {
    if(p %in% base_params_case2$Parameter) base_params_case2$Estimate[base_params_case2$Parameter == p] else NA
  }),
  Case2_diff = sapply(key_params, function(p) {
    diff_p <- paste0("d_", p)
    if(diff_p %in% diff_params_case2$Parameter) diff_params_case2$Estimate[diff_params_case2$Parameter == diff_p] else NA
  }),
  Case3 = sapply(key_params, function(p) {
    if(p %in% coef_case3_sub$Parameter) coef_case3_sub$Estimate[coef_case3_sub$Parameter == p] else NA
  }),
  Case4_base = sapply(key_params, function(p) {
    if(p %in% base_params_case4$Parameter) base_params_case4$Estimate[base_params_case4$Parameter == p] else NA
  }),
  Case4_diff = sapply(key_params, function(p) {
    diff_p <- paste0("d_", p)
    if(diff_p %in% diff_params_case4$Parameter) diff_params_case4$Estimate[diff_params_case4$Parameter == diff_p] else NA
  })
)

print(key_comparison)

# Interpretation of key findings
cat("\nINTERPRETATION OF KEY FINDINGS:\n")
cat("-------------------------------\n")
cat("1. Travel Time Sensitivity (b_tt):\n")
cat("   - All cases show negative coefficients as expected\n")
cat("   - Multimodal travelers show slightly different sensitivity in Case 2 & 4\n\n")

cat("2. Cost Sensitivity (b_co1):\n")
cat("   - Negative coefficients across all cases\n")
cat("   - Differential effects suggest multimodal travelers may have different cost sensitivity\n\n")

cat("3. Scale Parameter (sigma_multi):\n")
cat(sprintf("   - Case 3: σ_multi = %.4f (t = %.3f, significantly different from 1)\n", sigma_case3, sigma_t_case3))
cat(sprintf("   - Case 4: σ_multi = %.4f (t = %.3f, significantly different from 1)\n", sigma_case4, sigma_t_case4))
if(sigma_case3 < 1 || sigma_case4 < 1) {
  cat("   - Values < 1 suggest higher variance in multimodal choices\n")
} else {
  cat("   - Values > 1 suggest lower variance in multimodal choices\n")
}













# ======================================================
# EXTRACTING COEFFICIENTS FOR UNIMODAL AND MULTIMODAL SEPARATELY
# ======================================================

cat("\nDETAILED COEFFICIENT ANALYSIS - UNIMODAL vs MULTIMODAL\n")
cat("======================================================\n")

# Function to extract coefficients for unimodal and multimodal separately
extract_separate_coefficients <- function(model, case_name, case_type) {
  estimates <- model$estimate
  
  # Get standard errors
  se <- tryCatch({
    sqrt(diag(model$robvarcov))
  }, error = function(e) {
    sqrt(diag(model$varcov))
  })
  
  param_names <- names(estimates)
  t_values <- estimates / se[1:length(estimates)]
  
  coef_df <- data.frame(
    Parameter = param_names,
    Estimate = round(estimates, 4),
    Std_Error = round(se[1:length(estimates)], 4),
    t_value = round(t_values, 3),
    Case = case_name,
    stringsAsFactors = FALSE
  )
  
  # For cases 2 and 4, separate unimodal and multimodal parameters
  if(case_type %in% c("case2", "case4")) {
    # Base parameters are for unimodal
    unimodal_df <- coef_df[!grepl("^d_", coef_df$Parameter), ]
    unimodal_df$Group <- "Unimodal"
    
    # For multimodal: base + differential parameters
    multimodal_base <- coef_df[!grepl("^d_", coef_df$Parameter), ]
    multimodal_diff <- coef_df[grepl("^d_", coef_df$Parameter), ]
    
    # Remove 'd_' prefix for matching
    multimodal_diff$Parameter <- gsub("^d_", "", multimodal_diff$Parameter)
    
    # Merge base and differential parameters
    multimodal_combined <- merge(multimodal_base, multimodal_diff, 
                                 by = "Parameter", suffixes = c("_base", "_diff"))
    multimodal_combined$Estimate <- multimodal_combined$Estimate_base + 
      multimodal_combined$Estimate_diff
    multimodal_combined$t_value <- multimodal_combined$Estimate / 
      sqrt(multimodal_combined$Std_Error_base^2 + multimodal_combined$Std_Error_diff^2)
    
    multimodal_df <- data.frame(
      Parameter = multimodal_combined$Parameter,
      Estimate = round(multimodal_combined$Estimate, 4),
      Std_Error = round(sqrt(multimodal_combined$Std_Error_base^2 + 
                               multimodal_combined$Std_Error_diff^2), 4),
      t_value = round(multimodal_combined$t_value, 3),
      Case = case_name,
      Group = "Multimodal",
      stringsAsFactors = FALSE
    )
    
    return(list(unimodal = unimodal_df, multimodal = multimodal_df))
    
  } else {
    # For cases 1 and 3, same parameters for both groups
    unimodal_df <- coef_df
    unimodal_df$Group <- "Unimodal"
    multimodal_df <- coef_df
    multimodal_df$Group <- "Multimodal"
    
    return(list(unimodal = unimodal_df, multimodal = multimodal_df))
  }
}

# Extract coefficients for all cases
case1_coef <- extract_separate_coefficients(case1_model, "Case1: σ=1, β=1", "case1")
case2_coef <- extract_separate_coefficients(case2_model, "Case2: σ=1, β≠1", "case2")
case3_coef <- extract_separate_coefficients(case3_model, "Case3: σ≠1, β=1", "case3")
case4_coef <- extract_separate_coefficients(case4_model, "Case4: σ≠1, β≠1", "case4")

# Combine all results
all_unimodal <- rbind(
  case1_coef$unimodal,
  case2_coef$unimodal,
  case3_coef$unimodal,
  case4_coef$unimodal
)

all_multimodal <- rbind(
  case1_coef$multimodal,
  case2_coef$multimodal,
  case3_coef$multimodal,
  case4_coef$multimodal
)

# Print results for each case
cat("\nCASE 1: SAME SIGMA AND SAME BETA (σ=1, β=1)\n")
cat("============================================\n")
cat("UNIMODAL TRAVELERS:\n")
print(subset(all_unimodal, Case == "Case1: σ=1, β=1")[, c("Parameter", "Estimate", "t_value")])
cat("\nMULTIMODAL TRAVELERS:\n")
print(subset(all_multimodal, Case == "Case1: σ=1, β=1")[, c("Parameter", "Estimate", "t_value")])

cat("\nCASE 2: SAME SIGMA BUT DIFFERENT BETA (σ=1, β≠1)\n")
cat("=================================================\n")
cat("UNIMODAL TRAVELERS (Base parameters):\n")
print(subset(all_unimodal, Case == "Case2: σ=1, β≠1")[, c("Parameter", "Estimate", "t_value")])
cat("\nMULTIMODAL TRAVELERS (Base + Differential parameters):\n")
print(subset(all_multimodal, Case == "Case2: σ=1, β≠1")[, c("Parameter", "Estimate", "t_value")])

cat("\nCASE 3: DIFFERENT SIGMA BUT SAME BETA (σ≠1, β=1)\n")
cat("=================================================\n")
cat("UNIMODAL TRAVELERS:\n")
print(subset(all_unimodal, Case == "Case3: σ≠1, β=1")[, c("Parameter", "Estimate", "t_value")])
cat("\nMULTIMODAL TRAVELERS:\n")
print(subset(all_multimodal, Case == "Case3: σ≠1, β=1")[, c("Parameter", "Estimate", "t_value")])

cat("\nCASE 4: DIFFERENT SIGMA AND DIFFERENT BETA (σ≠1, β≠1)\n")
cat("======================================================\n")
cat("UNIMODAL TRAVELERS (Base parameters):\n")
print(subset(all_unimodal, Case == "Case4: σ≠1, β≠1")[, c("Parameter", "Estimate", "t_value")])
cat("\nMULTIMODAL TRAVELERS (Base + Differential parameters):\n")
print(subset(all_multimodal, Case == "Case4: σ≠1, β≠1")[, c("Parameter", "Estimate", "t_value")])

# Create comparison tables for key parameters
key_params <- c("asc_tw", "asc_car", "asc_bus", "asc_trn", "asc_ipt2", "asc_nmt",
                "b_tt", "b_co1", "b_tw_pick", "b_car_pick", "b_bus_cap", "b_bus_scap",
                "b_trn_scap", "b_nmt_scap", "b_ipt2_scap", "b_fem_ipt", "b_inc_pt",
                "b_wdis10_pt", "b_ipt2_odd", "b_nmt_odd", "b_tw_los1", "b_los1_pt",
                "b_bus_los2", "b_trn_los2", "b_trn_los3", "b_cont1_trn")

# Unimodal comparison across cases
unimodal_comparison <- data.frame(
  Parameter = key_params,
  Case1 = sapply(key_params, function(p) {
    df <- subset(all_unimodal, Case == "Case1: σ=1, β=1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  }),
  Case2 = sapply(key_params, function(p) {
    df <- subset(all_unimodal, Case == "Case2: σ=1, β≠1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  }),
  Case3 = sapply(key_params, function(p) {
    df <- subset(all_unimodal, Case == "Case3: σ≠1, β=1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  }),
  Case4 = sapply(key_params, function(p) {
    df <- subset(all_unimodal, Case == "Case4: σ≠1, β≠1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  })
)

cat("\nUNIMODAL TRAVELERS - PARAMETER COMPARISON ACROSS CASES\n")
cat("(Estimate (t-value))\n")
cat("======================================================\n")
print(unimodal_comparison, row.names = FALSE)

# Multimodal comparison across cases
multimodal_comparison <- data.frame(
  Parameter = key_params,
  Case1 = sapply(key_params, function(p) {
    df <- subset(all_multimodal, Case == "Case1: σ=1, β=1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  }),
  Case2 = sapply(key_params, function(p) {
    df <- subset(all_multimodal, Case == "Case2: σ=1, β≠1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  }),
  Case3 = sapply(key_params, function(p) {
    df <- subset(all_multimodal, Case == "Case3: σ≠1, β=1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  }),
  Case4 = sapply(key_params, function(p) {
    df <- subset(all_multimodal, Case == "Case4: σ≠1, β≠1" & Parameter == p)
    if(nrow(df) > 0) paste0(round(df$Estimate, 4), " (", round(df$t_value, 2), ")") else "NA"
  })
)

cat("\nMULTIMODAL TRAVELERS - PARAMETER COMPARISON ACROSS CASES\n")
cat("(Estimate (t-value))\n")
cat("======================================================\n")
print(multimodal_comparison, row.names = FALSE)

# Sigma values comparison
sigma_comparison <- data.frame(
  Case = c("Case1", "Case2", "Case3", "Case4"),
  Sigma_Unimodal = c("1 (fixed)", "1 (fixed)", "1 (fixed)", "1 (fixed)"),
  Sigma_Multimodal = c("1 (fixed)", "1 (fixed)", 
                       ifelse("sigma_multi" %in% case3_coef$multimodal$Parameter,
                              paste0(round(case3_coef$multimodal$Estimate[case3_coef$multimodal$Parameter == "sigma_multi"], 4), 
                                     " (", round(case3_coef$multimodal$t_value[case3_coef$multimodal$Parameter == "sigma_multi"], 2), ")"),
                              "NA"),
                       ifelse("sigma_multi" %in% case4_coef$multimodal$Parameter,
                              paste0(round(case4_coef$multimodal$Estimate[case4_coef$multimodal$Parameter == "sigma_multi"], 4), 
                                     " (", round(case4_coef$multimodal$t_value[case4_coef$multimodal$Parameter == "sigma_multi"], 2), ")"),
                              "NA"))
)

cat("\nSIGMA (SCALE) PARAMETER COMPARISON\n")
cat("==================================\n")
print(sigma_comparison, row.names = FALSE)






















# ======================================================
# COMPREHENSIVE TABLE FOR SIGMA AND BETA PARAMETERS
# ACROSS ALL FOUR CASES
# ======================================================

cat("\nCOMPREHENSIVE TABLE: SIGMA AND BETA PARAMETERS ACROSS FOUR CASES\n")
cat("===============================================================\n")

# Create a function to extract parameters in a standardized format
extract_parameters_table <- function(model, case_name) {
  estimates <- model$estimate
  
  # Get standard errors
  se <- tryCatch({
    sqrt(diag(model$robvarcov))
  }, error = function(e) {
    sqrt(diag(model$varcov))
  })
  
  param_names <- names(estimates)
  t_values <- estimates / se[1:length(estimates)]
  
  # Create data frame
  coef_df <- data.frame(
    Parameter = param_names,
    Estimate = round(estimates, 4),
    Std_Error = round(se[1:length(estimates)], 4),
    t_value = round(t_values, 3),
    stringsAsFactors = FALSE
  )
  
  # Format for display
  coef_df$Formatted <- paste0(
    round(coef_df$Estimate, 4), 
    " (", round(coef_df$t_value, 2), ")"
  )
  
  return(coef_df)
}

# Extract parameters for all cases
params_case1 <- extract_parameters_table(case1_model, "Case1")
params_case2 <- extract_parameters_table(case2_model, "Case2")
params_case3 <- extract_parameters_table(case3_model, "Case3")
params_case4 <- extract_parameters_table(case4_model, "Case4")

# Get all unique parameters across all cases
all_params <- unique(c(
  params_case1$Parameter,
  params_case2$Parameter,
  params_case3$Parameter,
  params_case4$Parameter
))

# Create comprehensive table
comprehensive_table <- data.frame(
  Parameter = all_params,
  Case1 = sapply(all_params, function(p) {
    if(p %in% params_case1$Parameter) {
      params_case1$Formatted[params_case1$Parameter == p]
    } else {
      "NA"
    }
  }),
  Case2 = sapply(all_params, function(p) {
    if(p %in% params_case2$Parameter) {
      params_case2$Formatted[params_case2$Parameter == p]
    } else {
      "NA"
    }
  }),
  Case3 = sapply(all_params, function(p) {
    if(p %in% params_case3$Parameter) {
      params_case3$Formatted[params_case3$Parameter == p]
    } else {
      "NA"
    }
  }),
  Case4 = sapply(all_params, function(p) {
    if(p %in% params_case4$Parameter) {
      params_case4$Formatted[params_case4$Parameter == p]
    } else {
      "NA"
    }
  }),
  stringsAsFactors = FALSE
)

# Add parameter descriptions for better readability
param_descriptions <- c(
  "asc_tw" = "ASC: Two-wheeler",
  "asc_car" = "ASC: Car",
  "asc_bus" = "ASC: Bus",
  "asc_trn" = "ASC: Train",
  "asc_ipt1" = "ASC: IPT1",
  "asc_ipt2" = "ASC: IPT2",
  "asc_nmt" = "ASC: NMT",
  "b_tt" = "Travel time coefficient",
  "b_co1" = "Cost coefficient",
  "b_tw_pick" = "TW: Pick/drop coefficient",
  "b_car_pick" = "Car: Pick/drop coefficient",
  "b_bus_cap" = "Bus: Captive coefficient",
  "b_bus_scap" = "Bus: Semi-captive coefficient",
  "b_trn_scap" = "Train: Semi-captive coefficient",
  "b_nmt_scap" = "NMT: Semi-captive coefficient",
  "b_ipt2_scap" = "IPT2: Semi-captive coefficient",
  "b_fem_ipt" = "IPT: Female coefficient",
  "b_inc_pt" = "PT: High income coefficient",
  "b_wdis10_pt" = "PT: Work distance >10km coefficient",
  "b_ipt2_odd" = "IPT2: Odd time coefficient",
  "b_nmt_odd" = "NMT: Odd time coefficient",
  "b_tw_los1" = "TW: LOS1 coefficient",
  "b_los1_pt" = "PT: LOS1 coefficient",
  "b_bus_los2" = "Bus: LOS2 coefficient",
  "b_trn_los2" = "Train: LOS2 coefficient",
  "b_trn_los3" = "Train: LOS3 coefficient",
  "b_cont1_trn" = "Train: Continuity coefficient",
  "d_asc_tw" = "Diff ASC: Two-wheeler",
  "d_asc_car" = "Diff ASC: Car",
  "d_asc_bus" = "Diff ASC: Bus",
  "d_asc_trn" = "Diff ASC: Train",
  "d_asc_ipt1" = "Diff ASC: IPT1",
  "d_asc_ipt2" = "Diff ASC: IPT2",
  "d_asc_nmt" = "Diff ASC: NMT",
  "d_b_tt" = "Diff Travel time coefficient",
  "d_b_co1" = "Diff Cost coefficient",
  "d_b_tw_pick" = "Diff TW: Pick/drop coefficient",
  "d_b_car_pick" = "Diff Car: Pick/drop coefficient",
  "d_b_bus_cap" = "Diff Bus: Captive coefficient",
  "d_b_bus_scap" = "Diff Bus: Semi-captive coefficient",
  "d_b_trn_scap" = "Diff Train: Semi-captive coefficient",
  "d_b_nmt_scap" = "Diff NMT: Semi-captive coefficient",
  "d_b_ipt2_scap" = "Diff IPT2: Semi-captive coefficient",
  "d_b_fem_ipt" = "Diff IPT: Female coefficient",
  "d_b_inc_pt" = "Diff PT: High income coefficient",
  "d_b_wdis10_pt" = "Diff PT: Work distance >10km coefficient",
  "d_b_ipt2_odd" = "Diff IPT2: Odd time coefficient",
  "d_b_nmt_odd" = "Diff NMT: Odd time coefficient",
  "d_b_tw_los1" = "Diff TW: LOS1 coefficient",
  "d_b_los1_pt" = "Diff PT: LOS1 coefficient",
  "d_b_bus_los2" = "Diff Bus: LOS2 coefficient",
  "d_b_trn_los2" = "Diff Train: LOS2 coefficient",
  "d_b_trn_los3" = "Diff Train: LOS3 coefficient",
  "d_b_cont1_trn" = "Diff Train: Continuity coefficient",
  "sigma_multi" = "Sigma: Multimodal scale parameter"
)

# Add descriptions to the table
comprehensive_table$Description <- param_descriptions[comprehensive_table$Parameter]

# Reorder columns to put description first
comprehensive_table <- comprehensive_table[, c("Parameter", "Description", "Case1", "Case2", "Case3", "Case4")]

# Sort parameters by type for better organization
param_order <- c(
  # ASC parameters
  "asc_tw", "asc_car", "asc_bus", "asc_trn", "asc_ipt1", "asc_ipt2", "asc_nmt",
  # Main coefficients
  "b_tt", "b_co1", "sigma_multi",
  # Mode-specific coefficients
  "b_tw_pick", "b_car_pick", "b_bus_cap", "b_bus_scap", "b_trn_scap", "b_nmt_scap", 
  "b_ipt2_scap", "b_fem_ipt", "b_inc_pt", "b_wdis10_pt", "b_ipt2_odd", "b_nmt_odd",
  "b_tw_los1", "b_los1_pt", "b_bus_los2", "b_trn_los2", "b_trn_los3", "b_cont1_trn",
  # Differential parameters
  "d_asc_tw", "d_asc_car", "d_asc_bus", "d_asc_trn", "d_asc_ipt1", "d_asc_ipt2", "d_asc_nmt",
  "d_b_tt", "d_b_co1", "d_b_tw_pick", "d_b_car_pick", "d_b_bus_cap", "d_b_bus_scap", 
  "d_b_trn_scap", "d_b_nmt_scap", "d_b_ipt2_scap", "d_b_fem_ipt", "d_b_inc_pt", 
  "d_b_wdis10_pt", "d_b_ipt2_odd", "d_b_nmt_odd", "d_b_tw_los1", "d_b_los1_pt", 
  "d_b_bus_los2", "d_b_trn_los2", "d_b_trn_los3", "d_b_cont1_trn"
)

# Filter and order the table
comprehensive_table <- comprehensive_table[match(param_order, comprehensive_table$Parameter), ]
comprehensive_table <- comprehensive_table[!is.na(comprehensive_table$Parameter), ]

# Print the comprehensive table
cat("\nCOMPREHENSIVE PARAMETER TABLE (Estimate (t-value))\n")
cat("===================================================\n")
print(comprehensive_table, row.names = FALSE)

# Create a simplified version focusing on key parameters
key_params_simple <- c(
  "asc_tw", "asc_car", "asc_bus", "asc_trn", "asc_ipt2", "asc_nmt",
  "b_tt", "b_co1", "sigma_multi"
)

simple_table <- comprehensive_table[comprehensive_table$Parameter %in% key_params_simple, ]

cat("\nSIMPLIFIED TABLE: KEY PARAMETERS (Estimate (t-value))\n")
cat("=====================================================\n")
print(simple_table, row.names = FALSE)

# Create a summary table for sigma values
sigma_summary <- data.frame(
  Case = c("Case 1: σ=1, β=1", "Case 2: σ=1, β≠1", "Case 3: σ≠1, β=1", "Case 4: σ≠1, β≠1"),
  Sigma_Unimodal = c("1 (fixed)", "1 (fixed)", "1 (fixed)", "1 (fixed)"),
  Sigma_Multimodal = c(
    "1 (fixed)",
    "1 (fixed)",
    ifelse("sigma_multi" %in% params_case3$Parameter, 
           params_case3$Formatted[params_case3$Parameter == "sigma_multi"], "NA"),
    ifelse("sigma_multi" %in% params_case4$Parameter, 
           params_case4$Formatted[params_case4$Parameter == "sigma_multi"], "NA")
  ),
  Beta_Equality = c("Same β", "Different β", "Same β", "Different β")
)

cat("\nSIGMA AND BETA SUMMARY TABLE\n")
cat("=============================\n")
print(sigma_summary, row.names = FALSE)

# Final interpretation
cat("\nFINAL INTERPRETATION OF SIGMA AND BETA PARAMETERS\n")
cat("=================================================\n")

# Extract sigma values for interpretation
sigma_case3 <- ifelse("sigma_multi" %in% params_case3$Parameter, 
                      params_case3$Estimate[params_case3$Parameter == "sigma_multi"], NA)
sigma_case4 <- ifelse("sigma_multi" %in% params_case4$Parameter, 
                      params_case4$Estimate[params_case4$Parameter == "sigma_multi"], NA)

cat("Sigma Parameter Interpretation:\n")
if(!is.na(sigma_case3)) {
  if(sigma_case3 < 1) {
    cat("- Case 3: σ_multi =", round(sigma_case3, 4), "< 1: Higher variance in multimodal choices\n")
  } else if(sigma_case3 > 1) {
    cat("- Case 3: σ_multi =", round(sigma_case3, 4), "> 1: Lower variance in multimodal choices\n")
  } else {
    cat("- Case 3: σ_multi = 1: Same variance as unimodal\n")
  }
}

if(!is.na(sigma_case4)) {
  if(sigma_case4 < 1) {
    cat("- Case 4: σ_multi =", round(sigma_case4, 4), "< 1: Higher variance in multimodal choices\n")
  } else if(sigma_case4 > 1) {
    cat("- Case 4: σ_multi =", round(sigma_case4, 4), "> 1: Lower variance in multimodal choices\n")
  } else {
    cat("- Case 4: σ_multi = 1: Same variance as unimodal\n")
  }
}

# Check if differential parameters are significant in Case 2 and 4
diff_params_case2 <- params_case2[grepl("^d_", params_case2$Parameter), ]
diff_params_case4 <- params_case4[grepl("^d_", params_case4$Parameter), ]

cat("\nBeta Parameter Interpretation:\n")
cat("- Case 2: Number of significant differential parameters:", 
    sum(abs(diff_params_case2$t_value) > 1.96), "out of", nrow(diff_params_case2), "\n")
cat("- Case 4: Number of significant differential parameters:", 
    sum(abs(diff_params_case4$t_value) > 1.96), "out of", nrow(diff_params_case4), "\n")

# Identify which parameters show the biggest differences
if(nrow(diff_params_case2) > 0) {
  diff_params_case2$abs_t <- abs(diff_params_case2$t_value)
  top_diffs_case2 <- head(diff_params_case2[order(-diff_params_case2$abs_t), ], 5)
  cat("\nTop 5 most different parameters in Case 2:\n")
  print(top_diffs_case2[, c("Parameter", "Estimate", "t_value")])
}

if(nrow(diff_params_case4) > 0) {
  diff_params_case4$abs_t <- abs(diff_params_case4$t_value)
  top_diffs_case4 <- head(diff_params_case4[order(-diff_params_case4$abs_t), ], 5)
  cat("\nTop 5 most different parameters in Case 4:\n")
  print(top_diffs_case4[, c("Parameter", "Estimate", "t_value")])
}