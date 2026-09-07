# Settings, constants and lookup tables - 
#
# Nothing in this file reads a file or does any computation - it just
# keeeps everything in one place instead of scattered through script.
# Some of it isn't used, but I tried to keep what I expect is necessary
# at the moment.

# Prefer data.table. 
# i find that tidy changes too much which makes long term maint a bit of a 
# headachec. I use some of the more stable tidy pkgs, e.g. here, stringr...
library(data.table)
# regex/string cleaning only - data.table handles the tables
library(stringr)   
# library(grid)
# library(ragg)

cfg <- list()

cfg$report <- list(
  report_date = Sys.Date(),
  extract_dttm = "202609021607"
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
      "rand_status",
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
  
  # Recode screening outcomes with alt descriptors
  d_valid_outcomes = data.table(
    EL_Outcome = c(
      "randomised", "randomised pending", 
      "registry", 
      "not eligible", "excluded", "not included",
      NA
    ),
    outcome = c(
      "randomised", NA_character_,
      "registry", 
      "not eligible", "not eligible", "not eligible",
      NA_character_
    )
  ),
  
  # Protocol exclusion criteria 1-8, and the eligibility-export flag column
  # that records each one.
  d_excl_flags_spec <- data.table(
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
  )
)


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



