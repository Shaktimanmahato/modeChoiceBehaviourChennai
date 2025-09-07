# ======================================================
# COMPREHENSIVE ANALYSIS OF MULTIMODAL TRAVEL BEHAVIOR
# PRIMARY vs SECONDARY MODE CHOICE IN MULTIMODAL TRAVELERS
# ======================================================

# Clear memory
rm(list = ls())

# Load required libraries
library(apollo)
library(dplyr)

# ======================================================
# 1. DATA PREPARATION
# ======================================================

# Load data
actual_df <- read.csv("D:\\Project code\\006_hyperpath_ganesh_revised_csv_679_rev_rem_ipt_nmt (1).csv")

# Create necessary variables
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
    wrkdis15 = ifelse(lwttdist_mar20 >= 15000, 1, 0),
    wrkdis10 = ifelse(lwttdist_mar20 > 10000, 1, 0),
    wrkdis5 = ifelse(lwttdist_mar20 <= 5000, 1, 0),
    wrkdis510 = ifelse(lwttdist_mar20 > 5000 & lwttdist_mar20 <= 10000, 1, 0),
    wrkdis1015 = ifelse(lwttdist_mar20 > 10000 & lwttdist_mar20 <= 15000, 1, 0),
    # Demographic variables
    female = 1 - gen,
    drvknow = ifelse(Drvkn2w == 1 & Drvkn4w == 1, 1, 0),
    trainnear = ifelse(Railoff_tide <= 1300, 1, 0),
    # Mode choice variables
    pri_choice = apply(dplyr::select(., tw_p, car_p, bus_p, trn_p, ipt1_p, ipt2_p, nmt_p), 1, which.max),
    sec_choice = apply(dplyr::select(., tw_s, car_s, bus_s, trn_s, ipt1_s, ipt2_s, nmt_s), 1, which.max),
    # Availability (assuming all modes available)
    tw_avail = 1,
    car_avail = 1,
    bus_avail = 1,
    trn_avail = 1,
    ipt1_avail = 1,
    ipt2_avail = 1,
    nmt_avail = 1,
    # Additional variables
    nmttt1 = (wlktt1_mar20 + cyctt1_mar20) / 2,
    ipt2co2_mar20 = ifelse(AWM_mar20 == 8, shauco2_mar20, cbusco2_mar20),
    inc40_more = ifelse(hhinc >= 6, 1, 0),
    numvehhh = n2w_mar20 + n4w_mar20
  )

# Filter only multimodal travelers
multimodal_df <- actual_df %>% filter(multimodal == 1)

# Create stacked dataset for joint modeling
primary_data <- multimodal_df %>%
  mutate(
    choice = pri_choice,
    choice_type = "primary",
    is_secondary = 0
  )

secondary_data <- multimodal_df %>%
  mutate(
    choice = sec_choice,
    choice_type = "secondary",
    is_secondary = 1
  )

stacked_data <- bind_rows(primary_data, secondary_data) %>%
  arrange(id)

database <- stacked_data

# ======================================================
# CASE 1: SAME SIGMA AND SAME BETA (σ=1, β=1)
# ======================================================

apollo_initialise()
apollo_control <- list(
  modelName = "Case1_SameSigmaSameBeta",
  modelDescr = "Same sigma (σ=1) and same beta (β=1) for primary and secondary choices",
  indivID = "id",
  outputDirectory = "output",
  panelData = TRUE
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
apollo_inputs_case1 <- apollo_validateInputs(apollo_beta_case1, apollo_fixed_case1, database = database)

apollo_probabilities_case1 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Same utility specification for both primary and secondary choices
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
    choiceVar = choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
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
  modelDescr = "Same sigma (σ=1) but different beta (β≠1) for secondary choices",
  indivID = "id",
  outputDirectory = "output",
  panelData = TRUE
)

apollo_beta_case2 <- c(
  # Base parameters (for primary choices)
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0,
  
  # Differential parameters for secondary choices (β_secondary - β_primary)
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
apollo_inputs_case2 <- apollo_validateInputs(apollo_beta_case2, apollo_fixed_case2, database = database)

apollo_probabilities_case2 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Utility specification with differential parameters for secondary choices
  V[["tw"]] = asc_tw + d_asc_tw*is_secondary + 
    (b_tt + d_b_tt*is_secondary)*twlrtt1_mar20 + 
    (b_tw_los1 + d_b_tw_los1*is_secondary)*(TWRELM + TWRELH)*twhh + 
    (b_tw_pick + d_b_tw_pick*is_secondary)*PickDrop
  
  V[["car"]] = asc_car + d_asc_car*is_secondary + 
    (b_tt + d_b_tt*is_secondary)*cartt1_mar20 + 
    (b_co1 + d_b_co1*is_secondary)*carco2_mar20 + 
    (b_car_pick + d_b_car_pick*is_secondary)*PickDrop
  
  V[["bus"]] = asc_bus + d_asc_bus*is_secondary + 
    (b_tt + d_b_tt*is_secondary)*bustt1_mar20 + 
    (b_co1 + d_b_co1*is_secondary)*busco2_mar20 + 
    (b_bus_cap + d_b_bus_cap*is_secondary)*captive_mar20 + 
    (b_bus_scap + d_b_bus_scap*is_secondary)*scaptive_mar20 + 
    (b_inc_pt + d_b_inc_pt*is_secondary)*inc40_more + 
    (b_los1_pt + d_b_los1_pt*is_secondary)*BEASEGET + 
    (b_bus_los2 + d_b_bus_los2*is_secondary)*dbus
  
  V[["trn"]] = asc_trn + d_asc_trn*is_secondary + 
    (b_tt + d_b_tt*is_secondary)*tratt1_mar20 + 
    (b_co1 + d_b_co1*is_secondary)*traco2_mar20 + 
    (b_wdis10_pt + d_b_wdis10_pt*is_secondary)*wrkdis10 + 
    (b_trn_scap + d_b_trn_scap*is_secondary)*scaptive_mar20 + 
    (b_inc_pt + d_b_inc_pt*is_secondary)*inc40_more + 
    (b_los1_pt + d_b_los1_pt*is_secondary)*TEASEGET + 
    (b_trn_los2 + d_b_trn_los2*is_secondary)*Railhom_tide + 
    (b_trn_los3 + d_b_trn_los3*is_secondary)*Railoff_tide + 
    (b_cont1_trn + d_b_cont1_trn*is_secondary)*vismp_new
  
  V[["ipt1"]] = asc_ipt1 + d_asc_ipt1*is_secondary + 
    (b_co1 + d_b_co1*is_secondary)*autoco2_mar20 + 
    (b_fem_ipt + d_b_fem_ipt*is_secondary)*female
  
  V[["ipt2"]] = asc_ipt2 + d_asc_ipt2*is_secondary + 
    (b_ipt2_scap + d_b_ipt2_scap*is_secondary)*scaptive_mar20 + 
    (b_fem_ipt + d_b_fem_ipt*is_secondary)*female + 
    (b_ipt2_odd + d_b_ipt2_odd*is_secondary)*oddtim
  
  V[["nmt"]] = asc_nmt + d_asc_nmt*is_secondary + 
    (b_tt + d_b_tt*is_secondary)*nmttt1 + 
    (b_nmt_scap + d_b_nmt_scap*is_secondary)*scaptive_mar20 + 
    (b_nmt_odd + d_b_nmt_odd*is_secondary)*oddtim
  
  mnl_settings <- list(
    alternatives = c(tw=1, car=2, bus=3, trn=4, ipt1=5, ipt2=6, nmt=7),
    avail = list(tw=tw_avail, car=car_avail, bus=bus_avail, trn=trn_avail, ipt1=ipt1_avail, ipt2=ipt2_avail, nmt=nmt_avail),
    choiceVar = choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
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
  modelDescr = "Different sigma (σ≠1) but same beta (β=1) for secondary choices",
  indivID = "id",
  outputDirectory = "output",
  panelData = TRUE
)

apollo_beta_case3 <- c(
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0,
  sigma_secondary = 1  # Scale parameter for secondary choices
)

apollo_fixed_case3 <- c("asc_ipt1")
apollo_inputs_case3 <- apollo_validateInputs(apollo_beta_case3, apollo_fixed_case3, database = database)

apollo_probabilities_case3 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Define scale parameter: 1 for primary, sigma_secondary for secondary
  scale_factor = 1 * (1 - is_secondary) + sigma_secondary * is_secondary
  
  # Same utility specification for both choice types
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
    choiceVar = choice,
    utilities = V
  )
  
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
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
  modelDescr = "Different sigma (σ≠1) and different beta (β≠1) for secondary choices",
  indivID = "id",
  outputDirectory = "output",
  panelData = TRUE
)

apollo_beta_case4 <- c(
  # Base parameters (for primary choices)
  asc_tw = 0, asc_car = 0, asc_bus = 0, asc_trn = 0, asc_ipt1 = 0, asc_ipt2 = 0, asc_nmt = 0,
  b_tt = 0, b_co1 = 0,
  b_tw_pick = 0, b_car_pick = 0,
  b_bus_cap = 0, b_bus_scap = 0, b_trn_scap = 0, b_nmt_scap = 0, 
  b_ipt2_scap = 0, b_fem_ipt = 0, b_inc_pt = 0, b_wdis10_pt = 0,
  b_ipt2_odd = 0, b_nmt_odd = 0, b_tw_los1 = 0, b_los1_pt = 0, 
  b_bus_los2 = 0, b_trn_los2 = 0, b_trn_los3 = 0, b_cont1_trn = 0,
  
  # Differential parameters for secondary choices (β_secondary - β_primary)
  d_asc_tw = 0, d_asc_car = 0, d_asc_bus = 0, d_asc_trn = 0, 
  d_asc_ipt1 = 0, d_asc_ipt2 = 0, d_asc_nmt = 0,
  d_b_tt = 0, d_b_co1 = 0,
  d_b_tw_pick = 0, d_b_car_pick = 0,
  d_b_bus_cap = 0, d_b_bus_scap = 0, d_b_trn_scap = 0, d_b_nmt_scap = 0, 
  d_b_ipt2_scap = 0, d_b_fem_ipt = 0, d_b_inc_pt = 0, d_b_wdis10_pt = 0,
  d_b_ipt2_odd = 0, d_b_nmt_odd = 0, d_b_tw_los1 = 0, d_b_los1_pt = 0, 
  d_b_bus_los2 = 0, d_b_trn_los2 = 0, d_b_trn_los3 = 0, d_b_cont1_trn = 0,
  
  sigma_secondary = 1  # Scale parameter for secondary choices
)

apollo_fixed_case4 <- c("asc_ipt1", "d_asc_ipt1")
apollo_inputs_case4 <- apollo_validateInputs(apollo_beta_case4, apollo_fixed_case4, database = database)

apollo_probabilities_case4 <- function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Define scale parameter: 1 for primary, sigma_secondary for secondary
  scale_factor = 1 * (1 - is_secondary) + sigma_secondary * is_secondary
  
  # Utility specification with differential parameters for secondary choices
  V[["tw"]] = (asc_tw + d_asc_tw*is_secondary + 
                 (b_tt + d_b_tt*is_secondary)*twlrtt1_mar20 + 
                 (b_tw_los1 + d_b_tw_los1*is_secondary)*(TWRELM + TWRELH)*twhh + 
                 (b_tw_pick + d_b_tw_pick*is_secondary)*PickDrop) * scale_factor
  
  V[["car"]] = (asc_car + d_asc_car*is_secondary + 
                  (b_tt + d_b_tt*is_secondary)*cartt1_mar20 + 
                  (b_co1 + d_b_co1*is_secondary)*carco2_mar20 + 
                  (b_car_pick + d_b_car_pick*is_secondary)*PickDrop) * scale_factor
  
  V[["bus"]] = (asc_bus + d_asc_bus*is_secondary + 
                  (b_tt + d_b_tt*is_secondary)*bustt1_mar20 + 
                  (b_co1 + d_b_co1*is_secondary)*busco2_mar20 + 
                  (b_bus_cap + d_b_bus_cap*is_secondary)*captive_mar20 + 
                  (b_bus_scap + d_b_bus_scap*is_secondary)*scaptive_mar20 + 
                  (b_inc_pt + d_b_inc_pt*is_secondary)*inc40_more + 
                  (b_los1_pt + d_b_los1_pt*is_secondary)*BEASEGET + 
                  (b_bus_los2 + d_b_bus_los2*is_secondary)*dbus) * scale_factor
  
  V[["trn"]] = (asc_trn + d_asc_trn*is_secondary + 
                  (b_tt + d_b_tt*is_secondary)*tratt1_mar20 + 
                  (b_co1 + d_b_co1*is_secondary)*traco2_mar20 + 
                  (b_wdis10_pt + d_b_wdis10_pt*is_secondary)*wrkdis10 + 
                  (b_trn_scap + d_b_trn_scap*is_secondary)*scaptive_mar20 + 
                  (b_inc_pt + d_b_inc_pt*is_secondary)*inc40_more + 
                  (b_los1_pt + d_b_los1_pt*is_secondary)*TEASEGET + 
                  (b_trn_los2 + d_b_trn_los2*is_secondary)*Railhom_tide + 
                  (b_trn_los3 + d_b_trn_los3*is_secondary)*Railoff_tide + 
                  (b_cont1_trn + d_b_cont1_trn*is_secondary)*vismp_new) * scale_factor
  

                    
   V[["ipt1"]] = (asc_ipt1 + d_asc_ipt1*is_secondary + 
                                     (b_co1 + d_b_co1*is_secondary)*autoco2_mar20 + 
                                     (b_fem_ipt + d_b_fem_ipt*is_secondary)*female) * scale_factor
                    
   V[["ipt2"]] = (asc_ipt2 + d_asc_ipt2*is_secondary + 
                                     (b_ipt2_scap + d_b_ipt2_scap*is_secondary)*scaptive_mar20 + 
                                     (b_fem_ipt + d_b_fem_ipt*is_secondary)*female + 
                                     (b_ipt2_odd + d_b_ipt2_odd*is_secondary)*oddtim) * scale_factor
                    
    V[["nmt"]] = (asc_nmt + d_asc_nmt*is_secondary + 
                                    (b_tt + d_b_tt*is_secondary)*nmttt1 + 
                                    (b_nmt_scap + d_b_nmt_scap*is_secondary)*scaptive_mar20 + 
                                    (b_nmt_odd + d_b_nmt_odd*is_secondary)*oddtim) * scale_factor
                    
          mnl_settings <- list(
                      alternatives = c(tw=1, car=2, bus=3, trn=4, ipt1=5, ipt2=6, nmt=7),
                      avail = list(tw=tw_avail, car=car_avail, bus=bus_avail, trn=trn_avail, ipt1=ipt1_avail, ipt2=ipt2_avail, nmt=nmt_avail),
                      choiceVar = choice,
                      utilities = V
                    )
                    
                    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
                    P <- apollo_panelProd(P, apollo_inputs, functionality)
                    P <- apollo_prepareProb(P, apollo_inputs, functionality)
                    return(P)
}

case4_model <- apollo_estimate(apollo_beta_case4, apollo_fixed_case4, apollo_probabilities_case4, apollo_inputs_case4)

# ======================================================
# MODEL COMPARISON AND OUTPUT GENERATION
# ======================================================

cat("COMPREHENSIVE ANALYSIS OF MULTIMODAL TRAVEL BEHAVIOR\n")
cat("Primary vs Secondary Mode Choice in Multimodal Travelers\n")
cat("=======================================================\n\n")

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
  BIC = sapply(models, function(x) log(nrow(database)) * length(x$estimate) - 2 * x$LLout),
  Parameters = sapply(models, function(x) length(x$estimate)),
  Observations = nrow(database),
  Rho_sq = sapply(models, function(x) 1 - x$LLout/x$LL0),
  Adj_Rho_sq = sapply(models, function(x) 1 - (x$LLout - length(x$estimate))/x$LL0),
  stringsAsFactors = FALSE
)

print(comparison)

# Function to extract coefficients and t-values
extract_coefficients <- function(model, case_name) {
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
    t_value = round(t_values, 3),
    Case = case_name,
    stringsAsFactors = FALSE
  )
  
  return(coef_df)
}

# Extract coefficients for all cases
coef_case1 <- extract_coefficients(case1_model, "Case1: σ=1, β=1")
coef_case2 <- extract_coefficients(case2_model, "Case2: σ=1, β≠1") 
coef_case3 <- extract_coefficients(case3_model, "Case3: σ≠1, β=1")
coef_case4 <- extract_coefficients(case4_model, "Case4: σ≠1, β≠1")

# Function to extract parameters for primary and secondary separately
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
    t_value = round(t_values, 3),
    Case = case_name,
    stringsAsFactors = FALSE
  )
  
  # For cases 2 and 4, separate primary and secondary parameters
  if(case_type %in% c("case2", "case4")) {
    # Base parameters are for primary
    primary_df <- coef_df[!grepl("^d_", coef_df$Parameter), ]
    primary_df$Choice_Type <- "Primary"
    
    # For secondary: base + differential parameters
    secondary_base <- coef_df[!grepl("^d_", coef_df$Parameter), ]
    secondary_diff <- coef_df[grepl("^d_", coef_df$Parameter), ]
    
    # Remove 'd_' prefix for matching
    secondary_diff$Parameter <- gsub("^d_", "", secondary_diff$Parameter)
    
    # Merge base and differential parameters
    secondary_combined <- merge(secondary_base, secondary_diff, 
                                by = "Parameter", suffixes = c("_base", "_diff"))
    secondary_combined$Estimate <- secondary_combined$Estimate_base + 
      secondary_combined$Estimate_diff
    secondary_combined$t_value <- secondary_combined$Estimate / 
      sqrt(secondary_combined$t_value_base^(-2) + secondary_combined$t_value_diff^(-2))
    
    secondary_df <- data.frame(
      Parameter = secondary_combined$Parameter,
      Estimate = round(secondary_combined$Estimate, 4),
      t_value = round(secondary_combined$t_value, 3),
      Case = case_name,
      Choice_Type = "Secondary",
      stringsAsFactors = FALSE
    )
    
    return(list(primary = primary_df, secondary = secondary_df))
    
  } else {
    # For cases 1 and 3, same parameters for both choice types
    primary_df <- coef_df
    primary_df$Choice_Type <- "Primary"
    secondary_df <- coef_df
    secondary_df$Choice_Type <- "Secondary"
    
    return(list(primary = primary_df, secondary = secondary_df))
  }
}

# Extract coefficients for all cases
case1_coef <- extract_separate_coefficients(case1_model, "Case1: σ=1, β=1", "case1")
case2_coef <- extract_separate_coefficients(case2_model, "Case2: σ=1, β≠1", "case2")
case3_coef <- extract_separate_coefficients(case3_model, "Case3: σ≠1, β=1", "case3")
case4_coef <- extract_separate_coefficients(case4_model, "Case4: σ≠1, β≠1", "case4")

# Combine all results
all_primary <- rbind(
  case1_coef$primary,
  case2_coef$primary,
  case3_coef$primary,
  case4_coef$primary
)

all_secondary <- rbind(
  case1_coef$secondary,
  case2_coef$secondary,
  case3_coef$secondary,
  case4_coef$secondary
)

# ======================================================
# OUTPUT FOR EACH CASE AS REQUESTED
# ======================================================

for (case_num in 1:4) {
  case_labels <- c("Case1: σ=1, β=1", "Case2: σ=1, β≠1", "Case3: σ≠1, β=1", "Case4: σ≠1, β≠1")
  case_name <- case_labels[case_num]
  
  cat("\n", rep("=", 50), "\n", sep="")
  cat("CASE", case_num, "OUTPUT\n")
  cat(rep("=", 50), "\n\n", sep="")
  
  # a) Heteroscedastic output with sigma and beta values
  cat("a) HETEROSCEDASTIC OUTPUT:\n")
  cat(rep("-", 30), "\n", sep="")
  
  current_coef <- get(paste0("coef_case", case_num))
  
  # Extract sigma value if exists
  sigma_param <- current_coef[current_coef$Parameter %in% c("sigma_secondary"), ]
  if(nrow(sigma_param) > 0) {
    cat("Sigma (Scale) Parameter:\n")
    print(sigma_param[, c("Parameter", "Estimate", "t_value")])
    cat("\n")
  }
  
  # Show all parameters
  print(current_coef[, c("Parameter", "Estimate", "t_value")])
  cat("\n")
  
  # b) Primary mode choice coefficients
  cat("b) PRIMARY MODE CHOICE COEFFICIENTS:\n")
  cat(rep("-", 40), "\n", sep="")
  
  primary_coef <- all_primary[all_primary$Case == case_name, ]
  print(primary_coef[, c("Parameter", "Estimate", "t_value")])
  cat("\n")
  
  # c) Secondary mode choice coefficients
  cat("c) SECONDARY MODE CHOICE COEFFICIENTS:\n")
  cat(rep("-", 42), "\n", sep="")
  secondary_coef <- all_secondary[all_secondary$Case == case_name, ]
  print(secondary_coef[, c("Parameter", "Estimate", "t_value")])
  cat("\n")
}

# ======================================================
# SUMMARY TABLE WITH ALL REQUESTED METRICS
# ======================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("SUMMARY TABLE: ALL CASES COMPARISON\n")
cat(rep("=", 80), "\n\n", sep="")

# Function to safely extract sigma values
get_sigma_value <- function(coef_df) {
  sigma_row <- coef_df[coef_df$Parameter == "sigma_secondary", ]
  if(nrow(sigma_row) > 0) {
    return(round(sigma_row$Estimate, 4))
  } else {
    return("1 (fixed)")
  }
}

# Function to safely extract sigma t-values
get_sigma_tvalue <- function(coef_df) {
  sigma_row <- coef_df[coef_df$Parameter == "sigma_secondary", ]
  if(nrow(sigma_row) > 0) {
    return(round(sigma_row$t_value, 3))
  } else {
    return("NA")
  }
}

summary_table <- data.frame(
  Case = c("Case 1: σ=1, β=1", "Case 2: σ=1, β≠1", "Case 3: σ≠1, β=1", "Case 4: σ≠1, β≠1"),
  Observations = rep(nrow(database), 4),
  Initial_LL = sapply(models, function(x) x$LL0),
  Final_LL = sapply(models, function(x) x$LLout),
  Rho_squared = sapply(models, function(x) round(1 - x$LLout/x$LL0, 4)),
  Adj_Rho_squared = sapply(models, function(x) round(1 - (x$LLout - length(x$estimate))/x$LL0, 4)),
  Num_Indep_Vars = sapply(models, function(x) length(x$estimate)),
  Sigma_Value = c(
    get_sigma_value(coef_case1),
    get_sigma_value(coef_case2),
    get_sigma_value(coef_case3),
    get_sigma_value(coef_case4)
  ),
  Sigma_t_Value = c(
    get_sigma_tvalue(coef_case1),
    get_sigma_tvalue(coef_case2),
    get_sigma_tvalue(coef_case3),
    get_sigma_tvalue(coef_case4)
  ),
  stringsAsFactors = FALSE
)

print(summary_table, row.names = FALSE)

# ======================================================
# LIKELIHOOD RATIO TESTS FOR HYPOTHESIS TESTING
# ======================================================

cat("\n", rep("=", 60), "\n", sep="")
cat("LIKELIHOOD RATIO TESTS FOR HYPOTHESIS TESTING\n")
cat(rep("=", 60), "\n\n", sep="")

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

# ======================================================
# FINAL MODEL SELECTION AND RECOMMENDATIONS
# ======================================================

cat("\n", rep("=", 60), "\n", sep="")
cat("FINAL MODEL SELECTION AND RECOMMENDATIONS\n")
cat(rep("=", 60), "\n\n", sep="")

# Find best model based on BIC (preferred for model comparison)
bic_values <- sapply(models, function(x) log(nrow(database)) * length(x$estimate) - 2 * x$LLout)
best_model_idx <- which.min(bic_values)
best_model_name <- names(models)[best_model_idx]

cat("Based on Bayesian Information Criterion (BIC):\n")
cat("  Best model:", best_model_name, "\n")
cat("  BIC value:", round(bic_values[best_model_idx], 2), "\n\n")

cat("RECOMMENDATIONS:\n")
cat("1. ", best_model_name, " provides the best fit to the data\n")
cat("2. This suggests that ")

if(best_model_name == "Case1: σ=1, β=1") {
  cat("primary and secondary mode choices follow the same decision process\n")
  cat("   with identical scale and preference parameters\n")
} else if(best_model_name == "Case2: σ=1, β≠1") {
  cat("primary and secondary mode choices have different preference structures\n")
  cat("   but similar scale parameters\n")
} else if(best_model_name == "Case3: σ≠1, β=1") {
  cat("primary and secondary mode choices have different scale parameters\n")
  cat("   but similar preference structures\n")
} else if(best_model_name == "Case4: σ≠1, β≠1") {
  cat("primary and secondary mode choices are fundamentally different\n")
  cat("   in both scale and preference parameters\n")
}

cat("3. Policy interventions should consider these differences when targeting\n")
cat("   multimodal travelers for mode shift programs\n")

# ======================================================
# SAVE RESULTS TO FILE
# ======================================================

# Create output directory if it doesn't exist
if(!dir.exists("output")) {
  dir.create("output")
}

# Save model comparison
write.csv(comparison, "output/model_comparison.csv", row.names = FALSE)

# Save detailed coefficients
all_coefficients <- rbind(coef_case1, coef_case2, coef_case3, coef_case4)
write.csv(all_coefficients, "output/all_coefficients.csv", row.names = FALSE)

# Save primary and secondary coefficients separately
write.csv(all_primary, "output/primary_coefficients.csv", row.names = FALSE)
write.csv(all_secondary, "output/secondary_coefficients.csv", row.names = FALSE)

# Save summary table
write.csv(summary_table, "output/summary_table.csv", row.names = FALSE)

# Save likelihood ratio test results
lr_results <- data.frame(
  Test = c("Sigma Equality", "Beta Equality", "Full Equality", "Beta given Sigma"),
  LR_Statistic = c(lr_sigma, lr_beta, lr_full, lr_beta_given_sigma),
  Degrees_of_Freedom = c(df_sigma, df_beta, df_full, df_beta_given_sigma),
  P_Value = c(p_sigma, p_beta, p_full, p_beta_given_sigma),
  Conclusion = c(
    ifelse(p_sigma < 0.05, "Reject H0", "Cannot reject H0"),
    ifelse(p_beta < 0.05, "Reject H0", "Cannot reject H0"),
    ifelse(p_full < 0.05, "Reject H0", "Cannot reject H0"),
    ifelse(p_beta_given_sigma < 0.05, "Reject H0", "Cannot reject H0")
  )
)
write.csv(lr_results, "output/likelihood_ratio_tests.csv", row.names = FALSE)

cat("\nAnalysis completed successfully!\n")
cat("Results saved to 'output/' directory\n")