# Settings, constants and lookup tables - 
# Nothing in this file reads a file or does any computation - it just
# keeeps everything in one place instead of scattered through script.
# Some of it isn't used, but I tried to keep what I expect is necessary
# at the moment.

library(data.table)
# regex/string cleaning only - data.table handles the tables
library(stringr)   
# library(grid)
# library(ragg)

cfg <- list()

cfg$report <- list(
  report_date = Sys.Date(),
  extract_dttm = "202609021608"
)

cfg$data <- list(
  
  verbose = TRUE,
  
  # exports + removal list - be explicit, more robust than relying on parsing
  data_folder = paste0("data/dummy/", cfg$report$extract_dttm),           
  # excluded from all counts
  test_site_codes  = c("DS9"),    
  
  d_file_desc = data.table(
    prefix = c(
      "Domain eligibility antibiotic choice export",
      "Domain eligibility antibiotic duration Rev 1 export",
      "Domain eligibility antibiotic duration Rev 2 export",
      "Domain eligibility surgical export",
      "Participant Eligibility export",
      "Participant list export",
      "Withdrawal - Trial export",
      "eligibility-codes-for-removal"
    ),
    name = c(
      "elig_d4",
      "elig_d2",
      "elig_d3",
      "elig_d1",
      "elig_plt",
      "pt_listing",
      "withdraw",
      "removals"
    ),
    desc = c(
      "Reveal status for d4?",
      "Reveal status for d2?",
      "Reveal status for d3",
      "Reveal status for d1",
      "Platform entry eligibility assessment",
      "Assignment status all domains",
      "Withdrawal status",
      "Custom removals"
    )
  ),
  
  # PT_Assigned_Domain*/participant-list arm codes -> display labels.
  arm_labels = c(
    SD01 = "DAIR",                   
    SD02 = "Revision",
    AC01 = "No Rifampicin",          
    AC02 = "Rifampicin",
    DA01 = "6 week",                 
    DA02 = "12 week",
    DB01 = "No Extended Prophylaxis", 
    DB02 = "Extended Prophylaxis"
  ),
  
  # Protocol exclusion criteria 1-8, and the eligibility-export flag column
  # that records each one.
  d_excl_flags_spec = data.table(
    column = c(
      "EL_Excl_IndexJoint",
      "EL_Excl_PreviousParticipation_IndexJoint",
      "EL_Excl_PreviousParticipation_OtherJoint",
      "EL_Excl_ImminentDeath",
      "EL_Excl_CurativeIntentTreatment",
      "EL_Excl_NoDefinedSilo",
      "EL_Excl_PtUnavailable12mths",
      "EL_Excl_TreatingTeam"
    ),
    reason = c(
      "#1 Index joint: shoulder/elbow/wrist/ankle",
      "#2 Prev. ROADMAP for index joint",
      "#3 Prev. ROADMAP other joint <12mo",
      "#4 Death imminent and inevitable",
      "#5 Treatment not with curative intent",
      "#6 Not classifiable into three silos",
      "#7 Unlikely accessible for 12mo follow-up",
      "#8 Not in patient's best interests"
    )
  ),
  
  
  l_domain_spec = list(
    SD = list(
      label = "Surgical", file = "elig_d1", 
      arm_col = "PT_Assigned_DomainSD",
      consent_column = "CON_DomainSD",
      consent_pending = TRUE
    ),
    AC = list(
      label = "AB Choice", file = "elig_d4", 
      arm_col = "PT_Assigned_DomainAC",
      consent_column = "CON_DomainAC",
      consent_pending = TRUE
    ),
    DA = list(
      label = "AB Duration A", file = "elig_d2", 
      arm_col = "PT_Assigned_DomainDA",
      consent_column = "CON_DomainDA_DB",
      consent_pending = FALSE
    ),
    DB = list(
      label = "AB Duration B", file = "elig_d3", 
      arm_col = "PT_Assigned_DomainDB",
      consent_column = "CON_DomainDA_DB",
      consent_pending = FALSE
    )
  )
  
)

# mandatory cols for specific data extracts
cfg$data$d_file_desc[, cols := vector("list", .N)]

cfg$data$d_file_desc[
  name == "elig_plt", cols := c(
    "EligibilityCode", "EL_SiteCode", "EL_Outcome", "StudyPtID", 
    "EL_Incl_PhysicallyPresent", "RegistryPtID")]
cfg$data$d_file_desc[
  name == "removals", cols := c(
    "eligibility-code", "platform-code", "registry-code")]
cfg$data$d_file_desc[
  name == "withdraw", cols := c(
    "StudyPtID", "WD_Total_WithdrawAll", "WD_Reason")]
cfg$data$d_file_desc[
  name == "pt_listing", cols := c(
    "StudyPtID")]
cfg$data$d_file_desc[
  name == "elig_d1", cols := c(
    "StudyPtID", "EL_Outcome")]
cfg$data$d_file_desc[
  name == "elig_d2", cols := c(
    "StudyPtID", "EL_Outcome")]
cfg$data$d_file_desc[
  name == "elig_d3", cols := c(
    "StudyPtID", "EL_Outcome")]
cfg$data$d_file_desc[
  name == "elig_d4", cols := c(
    "StudyPtID", "EL_Outcome")]



cfg$fig_spec <- list(
  
  #
  # All this is for fig outputs, and I basically ignore for now
  # 
  # PNG / CSV outputs go here
  output_folder    = "outputs",           
  # EB Garamond files
  font_folder      = "assets/fonts/eb-garamond",  
  # FALSE for mock/staging data: ID mismatches warn instead of stopping.
  strict_id_checks = FALSE,
  # "sans" if the font files are missing
  diagram_font     = "EB Garamond"
)



