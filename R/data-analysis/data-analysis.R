

library(data.table)
# regex/string cleaning only - data.table handles the tables
library(stringr)   
# library(grid)
# library(ragg)
library(here)
library(lubridate)



# If you want to source to use any of the functions but dont want to run the 
# data processing to run, set DATA_SOURCE_ONLY to T
# Sys.setenv(DATA_SOURCE_ONLY = T)
Sys.setenv(DATA_SOURCE_ONLY = F)

# Config ------


l_cfg <- list()

l_cfg$version <- list(
  report_date = Sys.Date(),
  extract_dttm = "202609021608"
)
# date of the data extract (the data cut), taken from the extract label
l_cfg$version$dt_extract <- as.Date(
  substr(l_cfg$version$extract_dttm, 1, 8), format = "%Y%m%d"
)
l_cfg$extract <- list(
  data_dir = paste0("data/dummy/", l_cfg$version$extract_dttm),
  num_files = 16
)
l_cfg$extract$cols <- list()

l_cfg$extract$clean_cols[["plt-elig"]] <- c(
  "EligibilityCode",
  "CON_DomainSD",
  "CON_DomainAC",
  "CON_DomainDA_DB"
)

# domain definition giving mappings etc.
l_cfg$dm_defn <- list(
  SD = list(
    dm = "SD",
    ref = "elig_sd",
    label = "Surgical",
    trt_col = "PT_Assigned_DomainSD",
    consent_col = "CON_DomainSD",
    consent_pend = T,
    dur_dom = F,
    trt_map = list(SD01 = "DAIR", SD02 = "Revision")
  ),
  
  AC = list(
    dm = "AC",
    ref = "elig_ac",
    label = "AB Choice",
    trt_col = "PT_Assigned_DomainAC",
    consent_col = "CON_DomainAC",
    consent_pend = T,
    dur_dom = F,
    trt_map = list(AC01 = "No Rif", AC02 = "Rif")
  ),
  
  DA = list(
    dm = "DA",
    ref = "elig_da",
    label = "AB Dur A",
    trt_col = "PT_Assigned_DomainDA",
    consent_col = "CON_DomainDA_DB",
    consent_pend = F,
    dur_dom = T,
    trt_map = list(DA01 = "6 wk", DA02 = "12 wk")
  ),
  
  DB = list(
    dm = "DB",
    ref = "elig_db",
    label = "AB Dur B",
    trt_col = "PT_Assigned_DomainDB",
    consent_col = "CON_DomainDA_DB",
    consent_pend = F,
    dur_dom = T,
    trt_map = list(DB01 = "No Ext Proph", DB02 = "Ext Proph")
  )
)

# Baseline data dictionary: one row per baseline variable that is reported, in
# the order it is reported. It drives the cleaning in baseline() and the layout
# of the baseline table in the report, so a variable is added in one place.
#   name   - variable name in the derived data (l_bl$d_bl)
#   src    - column in the raw baseline extract
#   label  - plain text label used in the report
#   type   - num (a number), cat (one of `levels`) or bin (Yes/No)
#   levels - the allowed values of a cat variable; anything else is set to NA
#   group  - optional heading the variable is reported under
bl_var <- function(name, src, label, type, levels = NULL, group = NA_character_){
  data.table(
    name = name, src = src, label = label, type = type, 
    levels = list(levels), group = group
  )
}

l_cfg$bl <- list(
  vars = rbind(
    bl_var("age", "BAS_Dem_Age", "Age, years", "num"),
    bl_var("sex", "BAS_Dem_Sex", "Sex", "cat", c("Female", "Male")),
    bl_var("joint", "BAS_DIP_IJSite", "Index joint", "cat", c("Hip", "Knee")),
    bl_var("side", "BAS_DIP_IJSide", "Index joint side", "cat", c("Left", "Right")),
    bl_var("prosthesis_type", "BAS_DIP_ProthesisType", "Prosthesis type", "cat", 
           c("Primary", "Revision")),
    
    bl_var("cmb_diabetes", "BAS_Comorb_DiabetesMellitus", 
           "Diabetes mellitus", "bin", group = "Comorbidities"),
    bl_var("cmb_renal", "BAS_Comorb_ChronicRenalDisease",
           "Chronic renal disease (eGFR < 50, not on dialysis)", "bin", group = "Comorbidities"),
    bl_var("cmb_esrf", "BAS_Comorb_EndStageRenalFailure",
           "End stage renal failure (haemodialysis or CAPD)", "bin", group = "Comorbidities"),
    bl_var("cmb_malignancy", "BAS_Comorb_MalignancyPast12M",
           "Malignancy in past 12 months", "bin", group = "Comorbidities"),
    bl_var("cmb_cirrhosis", "BAS_Comorb_Cirrhosis",
           "Cirrhosis", "bin", group = "Comorbidities"),
    bl_var("cmb_ra", "BAS_Comorb_RheumatoidArthritis",
           "Rheumatoid arthritis", "bin", group = "Comorbidities"),
    bl_var("cmb_steroid", "BAS_Comorb_CorticosteroidPast6M",
           "Systemic corticosteroid use in past 6 months", "bin", group = "Comorbidities"),
    bl_var("cmb_immunosupp", "BAS_Comorb_IatrogenicImmunoSupp",
           "Iatrogenic immunosuppression", "bin", group = "Comorbidities"),
    bl_var("cmb_cardiac", "BAS_Comorb_Cardiac",
           "Congestive cardiac failure", "bin", group = "Comorbidities"),
    bl_var("cmb_obesity", "BAS_Comorb_Obesity",
           "Obesity (BMI >= 30)", "bin", group = "Comorbidities"),
    
    bl_var("asa", "BAS_CQI_ASAClassification", "ASA classification", "cat",
           c("ASA I", "ASA II", "ASA III", "ASA IV", "ASA V", "ASA VI", "Not documented")),
    bl_var("organism_identified", "BAS_Microbio_CausativeOrganismIdentified",
           "Causative organism identified", "bin"),
    
    bl_var("bld_crp", "BAS_Bloods_CRP_mgL", 
           "C-reactive protein, mg/L", "num", group = "Blood tests"),
    bl_var("bld_creatinine", "BAS_Bloods_Antibio_SerumCreatinine_umolL",
           "Serum creatinine, umol/L", "num", group = "Blood tests"),
    bl_var("bld_alt", "BAS_Bloods_Antibio_ALT_Entered", 
           "ALT", "num", group = "Blood tests"),
    bl_var("bld_ast", "BAS_Bloods_Antibio_AST_Entered", 
           "AST", "num", group = "Blood tests"),
    bl_var("bld_bilirubin", "BAS_Bloods_Antibio_Bilirubin_umolL",
           "Bilirubin, umol/L", "num", group = "Blood tests"),
    
    bl_var("ohs", "BAS_OHS_Score", 
           "Oxford Hip Score (0-48)", "num", group = "Patient reported outcomes"),
    bl_var("oks", "BAS_OKS_Score",
           "Oxford Knee Score (0-48)", "num", group = "Patient reported outcomes"),
    bl_var("eq5d_score", "BAS_EQ5D5L_Score",
           "EQ-5D-5L score", "num", group = "Patient reported outcomes"),
    bl_var("eq5d_vas", "BAS_EQ5D5L_HealthToday",
           "EQ-5D-5L health today (0-100)", "num", group = "Patient reported outcomes")
  )
)

# Primary outcome: treatment success at day 365 after platform randomisation,
# assessed from `day` - `window` onwards. 
l_cfg$d365 <- list(day = 365, window = 30)


l_cfg$test_codes = c("DS9")


# Utils --------

fname_to_label <- function(x = NULL){
  if(is.null(x)) return(NULL)
  if(is.na(x)) return(NULL)
  x_out <- sub("\\.csv$", "", x)
  x_out <- gsub("-", "_", x_out, fixed = TRUE)
  x_out
}


# Trim/squish whitespace and turn tokens into NA. 
# Replaces all internal gaps with single spaces.
clean_text <- function(
    x, 
    tokens_to_na = "", 
    case_sensitive = FALSE
) {
  
  x <- stringr::str_squish(as.character(x))
  
  if(!case_sensitive){
    x[!is.na(x) & str_to_lower(x) %in% tokens_to_na] <- NA_character_
  } else {
    x[!is.na(x) & x %in% tokens_to_na] <- NA_character_
  }
  x
}

drop_na <- function(x) {
  x[!is.na(x)]
}


stop_duplicate <- function(x, msg = ""){
  if (any(duplicated(x))) {
    stop(paste0("Duplicated code", msg))
  }
}


push_l <- function(l, value){
  l[length(l)+1] <- value
  l
}

pull_last <- function(l){
  l[length(l)]
}

last_1m <- function(v){
  if(length(v)<2) return(NULL)
  v[length(v)-1]
}

# Parse dates such as "23-Jan-2026", optionally followed by a time. The month
# abbreviation is matched against month.abb (always English) rather than using
# as.Date(format = "%b") so the result does not depend on the session locale.
parse_dmy_mon <- function(x){
  x <- clean_text(x)
  p <- str_match(x, "^(\\d{1,2})-([A-Za-z]{3})-(\\d{4})")
  mon <- match(str_to_lower(p[, 3]), str_to_lower(month.abb))
  as.Date(ISOdate(as.integer(p[, 4]), mon, as.integer(p[, 2])))
}

# Tokens in the extracts that mean "no value"
na_tokens <- c("", "n/a", "na", "null")

# Yes/No text to logical; anything else is NA.
yn <- function(x){
  x <- str_to_lower(clean_text(x))
  fcase(x == "yes", TRUE, x == "no", FALSE, default = NA)
}

# Text to a number. "No value" tokens become NA quietly. Any other text that is
# not a number also becomes NA, but is reported so that it is not lost.
to_num <- function(x, what = "value"){
  x <- clean_text(x, tokens_to_na = na_tokens)
  out <- suppressWarnings(as.numeric(x))
  n_bad <- sum(is.na(out) & !is.na(x))
  if(n_bad){
    message("Clean: ", what, ": ", n_bad, " non-numeric values set to NA")
  }
  out
}

# Text to a factor with the expected levels. A value that is not one of the 
# levels becomes NA and is reported.
to_factor <- function(x, levels, what = "value"){
  x <- clean_text(x, tokens_to_na = na_tokens)
  bad <- !is.na(x) & !(x %in% levels)
  if(any(bad)){
    message(
      "Clean: ", what, ": ", sum(bad), " values not in expected levels set to NA (",
      paste(unique(x[bad]), collapse = ", "), ")"
    )
    x[bad] <- NA_character_
  }
  factor(x, levels = levels)
}

read_extract <- function(l_cfg){
  
  data_dir <- here::here(l_cfg$extract$data_dir)
  
  v_fnames <- list.files(
    data_dir,  pattern = "\\.csv$", full.names = F
  )
  
  stopifnot(length(v_fnames) == l_cfg$extract$num_files)
  
  l_raw <- list()
  
  i = 1
  for(i in seq_along(v_fnames)){
    f <- v_fnames[i]
    
    lab <- fname_to_label(f)
    
    l_raw[[lab]] <- fread(file = here(data_dir, basename(f)))
    
    # check_columns(
    #   d_file_desc$prefix[i],
    #   cfg$data$d_file_desc$cols[i][[1]],
    #   names(l_data_raw[[d_file_desc$name[i]]])
    # )
    
  }
  
  # if(cfg$data$verbose){
  #   
  #   message("Extract data date: ", 
  #           lubridate::as_date(substr(basename(dir_data_raw), 1, 8)), 
  #           " label ",
  #           cfg$report$extract_dttm)
  #   
  #   dat_names <- names(l_data_raw)
  #   for(i in 1:length(l_data_raw)){
  #     message("Dataset: ", dat_names[i], " ncol ", ncol(l_data_raw[[i]]), 
  #             " nrow ", nrow(l_data_raw[[i]]))
  #   }
  #   
  # }
  
  l_raw
  
}



# Processing ---------

# Each removal file has three cols. My understanding is that these relate to
# screening rows, platform participants and registry participants and detail
# those pt ids that need to be dropped.
# c("eligibility-code", "platform-code", "registry-code")
proc_removals <- function(
    d_rm,
    cols = c("eligibility-code", "platform-code", "registry-code")
){
  
  # trim and replace specified tokens with true NA
  d_rm[, (cols) := lapply(.SD, function(z){
    clean_text(z, tokens_to_na = c("", "na", "n/a", "null"))
  }), .SDcols = cols]
  
  # remove test site data
  l_rm <- list()
  l_rm[["elig_code"]] <- drop_na(d_rm$`eligibility-code`)
  l_rm[["plt_code"]] <- drop_na(d_rm$`platform-code`)
  l_rm[["reg_code"]] <- drop_na(d_rm$`registry-code`)
  
  stop_duplicate(l_rm[["elig_code"]], "Removals: duplicate elig code")
  stop_duplicate(l_rm[["plt_code"]], "Removals: duplicate plt code")
  stop_duplicate(l_rm[["reg_code"]], "Removals: duplicate reg code")
  
  message("RM: ", 
          length(l_rm[["elig_code"]]), " eligibility code(s), ",
          length(l_rm[["plt_code"]]), " platform code(s), ",
          length(l_rm[["reg_code"]]), " registry code(s)")
  
  l_rm
}


# Clean and recode the withdrawal data, reporting on duplicate participant
# entries.
proc_withdrawals <- function(
    d_wd,
    cols = c("StudyPtID", "WD_Total_WithdrawAll", "WD_Reason")
){
  
  # trim and replace specified tokens with true NA
  message("WD: originally has ", nrow(d_wd), " rows.")
  
  d_wd[, StudyPtID := clean_text(StudyPtID, tokens_to_na = "")]
  # Indicator for Withdraw from all aspects of the ROADMAP Trial
  d_wd[, wd_all := str_to_lower(clean_text(WD_Total_WithdrawAll))]
  d_wd[, reason := str_to_lower(clean_text(WD_Reason))]
  # recode the reason
  d_wd[, reason_grp := fcase(
    str_starts(reason, "enrolled in error"), "invalid_enrolment",
    str_starts(reason, "invalid consent"), "invalid_enrolment",
    str_starts(reason, "consent withdrawn"), "consent_withdrawal",
    str_starts(reason, "appeared eligible"), "found_ineligible_later",
    default = "other"
  )]
  
  # 
  n_dup <- d_wd[, .N, keyby = StudyPtID][N > 1, .N]
  if(n_dup){
    message("WD: contains ", n_dup, " duplicate StudyPtIDs")
  }
  
  message("WD: Taking first record for each StudyPtID and dropping any StudyPtID set to NA")
  d_wd <- d_wd[!is.na(StudyPtID), .SD[1], keyby = StudyPtID]
  message("WD: data now has ", nrow(d_wd), " rows.")
  
  list(
    d_wd = d_wd
  )
}

# Clean up the screening (platform eligibility) data
# The intended structure of the screening steps is opaque. I have largely followed
# what I believe the intent of the consort generation script did from Jess Massie.
screening <- function(
    d_scr, d_rm, d_wd,
    test_codes = c("DS9")
                      ){
  l_dat <- list()
  
  # in contrast to the JM consort script, first I identify the removal records
  # and withdrawals before processing the screen (plt_elig) data
  
  # removals (step) - picks up the eligibility, platform and registry codes
  # for removal (if there are any)
  l_dat[["rm"]] <- proc_removals(d_rm)
  
  # withdrawals - identifies those that have withdrawn
  l_dat[["wd"]] <- proc_withdrawals(d_wd)
  
  v_n <- c(nrow(d_scr))
  message("Screening: original has ",   last(v_n), " rows.")
  # d_scr[1, EL_SiteCode := NA_character_]
  
  # clean/check cols - same as line 159 in JM code.
  cols <- c(
    "EligibilityCode", 
    "EL_SiteCode", 
    "EL_Outcome", 
    "StudyPtID", 
    "EL_Incl_PhysicallyPresent"
  )
  d_scr[, (cols) := lapply(.SD, function(z){ 
    clean_text(z) 
  }), .SDcols = cols]
  
  # remove test site data - same as line 166 in JM code.
  d_scr <- d_scr[!(EL_SiteCode %in% test_codes), ]
  v_n <- c(v_n, nrow(d_scr))
  message("Screening: site test codes removed leaves ", last(v_n), 
          " rows, ", last_1m(v_n) - last(v_n), " dropped")
  d_scr[, id_prefix := tstrsplit(StudyPtID, "-", fixed = T, keep = 1)]
  d_scr <- d_scr[!(id_prefix %in% test_codes)]
  v_n <- c(v_n, nrow(d_scr))
  message("Screening: pt id test codes removed leaves ", last(v_n), 
          " rows, ", last_1m(v_n) - last(v_n), " dropped")
  
  # clean eligibility codes (line 174 in JM)
  message("Screening: remove NA elig codes...")
  d_scr <- d_scr[!is.na(EligibilityCode)]
  v_n <- c(v_n, nrow(d_scr))
  message("Screening: NA elig removed leaves ", last(v_n), 
          " rows, ", last_1m(v_n) - last(v_n), " dropped")
  
  stop_duplicate(d_scr$EligibilityCode, "Screening: duplicate elig code")
  
  
  # standardising screening outcomes per line 188 in JM code
  # EL_Outcome is a summary of the status of the elig assessemnt. Valid values:
  # randomised
  # randomised pending
  # registry
  # not eligible
  # excluded
  # not included
  d_scr[, outcome := str_to_lower(EL_Outcome)]
  
  n_invalid_outcome <- d_scr[!(outcome %in% c(
    c("randomised", "randomised pending", "registry", 
      "not eligible", "excluded", "not included", NA)
  )), .(StudyPtID, EL_Outcome)]
  stopifnot(n_invalid_outcome == 0)
  
  d_scr[, outcome := fcase(
    outcome == "randomised", "Randomised",
    outcome == "registry", "Registry",
    outcome %in% c("not eligible", "excluded", "not included"), "Not eligible",
    # blank and "Randomised Pending"
    default = NA_character_  
  )]
  
  # EL_Incl_PhysicallyPresent indicates whether the pt was physically present
  # which is necessary for elig to be confirmed.
  d_scr[, presence := str_to_lower(EL_Incl_PhysicallyPresent)]
  d_scr[, presence := fcase(
    is.na(presence), NA_character_,
    presence == "yes", "Yes",
    presence == "no", "No",
    default = "<unrecognised>"
  )]
  
  # <unrecognised> not permitted
  stopifnot(!any(d_scr$presence %in% "<unrecognised>"))
  # completed outcome but no physical-presence value wouldn't make sense so stop
  stopifnot(!any(
    is.na(d_scr$presence) &  # presence was not set
      !is.na(d_scr$outcome)  # but they had an outcome other than NA
    ))
  
  # check for duplicated StudyPtID (possible and valid under re-rand)
  n_dup <- d_scr[!is.na(StudyPtID) & duplicated(StudyPtID), .N]
  if(n_dup){
    message("Screening: found ", n_dup, " non-NA StudyPtID duplicates")
  }
  
  
  # Only a listed EligibilityCode removes a screening row. Physical presence
  # "No" remains in the denominator unless that row is listed.
  message("Screening: elig code removals ...")
  d_scr <- d_scr[!(EligibilityCode %in% l_dat[["rm"]]$elig_code)]
  v_n <- c(v_n, nrow(d_scr))
  message("Screening: elig code removals leaves ", last(v_n), 
          " rows, ", last_1m(v_n) - last(v_n), " dropped")
  
  
  # Now check the withdrawals for the existence of invalid ids. These are
  # ids that were listed in the removals.csv file presumably independently
  # from what is in the spiral db.
  ids_invalid_from_wd <- l_dat[["wd"]]$d_wd[
    reason_grp == "invalid_enrolment" & wd_all == "yes", StudyPtID]
  # Select all the records from WD that were tagged as invalid_enrolment and
  # all those from the removals file platform-code field
  id_invalid_enrl <- union(ids_invalid_from_wd, l_dat[["rm"]]$plt_code)
  
  if(length(id_invalid_enrl)){
    n_found <- nrow(d_scr[StudyPtID %in% id_invalid_enrl, .SD[1], keyby = StudyPtID])
    message("Screening: invalid enrolments ", length(id_invalid_enrl), 
            " found ", n_found, " reclass outcome as Not eligible")
    d_scr[StudyPtID %in% id_invalid_enrl, outcome := "Not eligible" ]
    
    # l_dat$d_rand <- l_dat$d_rand[]
  }
  
  
  # registry removals
  if(length(l_dat[["rm"]]$reg_code)){
    n_found <- nrow(d_scr[
      RegistryPtID %in% l_dat[["rm"]]$reg_code, .SD[1], keyby = StudyPtID])
    message("Screening: number of registry removals ", 
            length(l_dat[["rm"]]$reg_code), " found ", n_found, 
            " reclass outcome as Not eligible")
    d_scr[RegistryPtID %in% l_dat[["rm"]]$reg_code, outcome := "Not eligible" ]
  }
  
  # original participant IDs and randomised set
  l_dat$d_rand <- d_scr[
    !is.na(StudyPtID) &
      !(EligibilityCode %in% l_dat[["rm"]]$elig_code) &
      !(StudyPtID %in% ids_invalid_from_wd), 
    .SD[1], keyby = StudyPtID]
  
  
  stopifnot(nrow(l_dat$d_rand) == sum(d_scr$outcome == "Randomised", na.rm = TRUE))
  
  
  l_dat$n_screened <- nrow(d_scr)                                            # D
  l_dat$n_randomised <- sum(d_scr$outcome == "Randomised", na.rm = TRUE)     # Y
  l_dat$n_registry <- sum(d_scr$outcome == "Registry", na.rm = TRUE)         # N
  l_dat$n_not_eligible <- sum(d_scr$outcome == "Not eligible", na.rm = TRUE) # A
  l_dat$n_pending <- sum(is.na(d_scr$outcome))                               # P
  
  
  # reconciliation check
  if (l_dat$n_randomised + l_dat$n_registry + l_dat$n_not_eligible + 
      l_dat$n_pending != l_dat$n_screened) {
    stop("Screening: Reconciliation failed: ", 
         l_dat$n_randomised, " + ", 
         l_dat$n_registry, " + ", 
         l_dat$n_not_eligible, " + ", 
         l_dat$n_pending, " ne ", 
         l_dat$n_screened, 
         call. = FALSE)
  }
  
  # report 1
  message("Screening: OK: ", 
          l_dat$n_randomised, " rand + ", 
          l_dat$n_registry, " registry + ", 
          l_dat$n_not_eligible, " inelig + ", 
          l_dat$n_pending, " pending = ", 
          l_dat$n_screened)
  
  # StudyPtID links randomised participants to the list and domain exports.
  l_dat$id_pt_plt <- d_scr[
    outcome == "Randomised" & !is.na(StudyPtID), 
    .SD[1], keyby = StudyPtID]$StudyPtID
  
  attr(l_dat$id_pt_plt, "label") <- paste(
    "Vector of randomised participants after removals have been processed",
    "for the screening data"
  )
  # attr(l_dat$id_pt_plt, "label")
  
  l_dat$d_screening <- copy(d_scr)
  
  l_dat
}


# Todo check interpretation....
# The participants data contains the domain level randomisation and the 
# consent, baseline, day 100, day 365  completion indicator as well as
# the withdrawal form status.
participants <- function(
    d_pt_list, d_rand, dt_extract,
    test_codes = c("DS9")
    ){
  
  
  v_n <- c(nrow(d_pt_list))
  message("Pts: raw data has ",   last(v_n), " rows.")
  
  cols <- c(
    "StudyPtID"
  )
  d_pt_list[, (cols) := lapply(.SD, function(z){ 
    clean_text(z) 
  }), .SDcols = cols]
  
  message("Pts: remove test codes...")
  d_pt_list[, id_prefix := tstrsplit(StudyPtID, "-", fixed = T, keep = 1)]
  d_pt_list <- d_pt_list[!(id_prefix %in% test_codes)]
  v_n <- c(v_n, nrow(d_pt_list))
  message("Pts: pt id test codes removed leaves ", last(v_n), " rows, ", last_1m(v_n) - last(v_n), " dropped")
  
  # Stop if duplicates
  n_duplicates <- d_pt_list[!is.na(StudyPtID)][duplicated(StudyPtID), .N]
  if(n_duplicates){
    message("Pts: found StudyPtID duplicates ", n_duplicates, " rows.")
    stop()
  }
  
  # Missing from platform list
  ids_pt <- d_pt_list[!is.na(StudyPtID), unique(StudyPtID)]
  ids_rand <- d_rand[!is.na(StudyPtID), unique(StudyPtID)]
  
  # elements that are in the first dataset but NOT in the second dataset.
  missing_from_list <- setdiff(ids_rand, ids_pt)
  if(length(missing_from_list)){
    msg <- paste(
      "Pts:", 
      length(missing_from_list), 
      "pts from screening not in pt list"
    )
    msg <- paste(msg, "(", paste(missing_from_list, collapse = ", "), ")")
    message(msg)
  }
  
  # The reporting population: randomised participants that are in the 
  # participant list because some randomised participants are missing from
  # the list, per message above. All of the processing that follows 
  # (baseline, primary outcome, deviations, domains) is restricted to these 
  # participants so every table in the report has the same denominator.
  d_pt <- d_pt_list[StudyPtID %in% ids_rand]
  d_pt[, site := id_prefix]
  d_pt[, dt_rand := as.Date(PT_RandomisationFirstLocal, format = "%d/%m/%Y")]
  stopifnot(!anyNA(d_pt$dt_rand))
  # days from platform randomisation to the data extract
  d_pt[, days_rand := as.integer(dt_extract - dt_rand)]
  stopifnot(all(d_pt$days_rand >= 0L))
  message(
    "Pts: ", nrow(d_pt), " randomised pts in reporting population"
  )
  
  # return processed sets:
  #   d_pt_list: participant list with test sites removed
  #   d_pt:      reporting population with site, randomisation date and days
  #              since randomisation added
  list(
    d_pt_list = d_pt_list,
    d_pt = d_pt
  )
  
}


domain <- function(
    d_dm, l_dm_def, 
    d_rand, d_pt_list,
    test_codes = c("DS9")
){
  
  # no na allowed in rand set
  n_na <- d_rand[is.na(StudyPtID), .N]
  stopifnot(n_na == 0)
  
  v_n <- c(nrow(d_dm))
  message("Domain: ", l_dm_def$label, " has ",   last(v_n), " rows.")
  
  cols <- c(
    "StudyPtID",  
    "EL_Outcome"
  )
  d_dm[, (cols) := lapply(.SD, function(z){ 
    clean_text(z) 
  }), .SDcols = cols]
  # NA is assumed to be pending
  # table(d_dm$EL_Outcome, useNA = "always") 
  
  # remove test codes
  d_dm[, id_prefix := tstrsplit(StudyPtID, "-", fixed = T, keep = 1)]
  d_dm <- d_dm[!(id_prefix %in% test_codes)]
  v_n <- c(v_n, nrow(d_dm))
  message("Domain: pt id test_codes ", last(v_n), 
          " rows remain, ", last_1m(v_n) - last(v_n), " dropped")
  
  # check for dodgy outcome levels
  n_unexpected <- d_dm[!is.na(EL_Outcome)][!(EL_Outcome %in% c(
    "Randomised", "Not Included", "Excluded", "Eliminated"
  )), .N]
  stopifnot(n_unexpected == 0)
  
  
  n_excl <- d_dm[!(StudyPtID %in% unique(d_rand$StudyPtID)), .N]
  if(n_excl){
    message("Domain: exclude ", n_excl, " pts not in rand data")
    d_dm <- d_dm[StudyPtID %in% unique(d_rand$StudyPtID)]
  }
  
  
  # are there any multiple outcomes per pt?
  d_dm_tmp <- unique(d_dm[, .(StudyPtID, EL_Outcome)])
  n_mult_outcome <- d_dm_tmp[, .N, keyby = StudyPtID][N>1, .N]
  if(n_mult_outcome){
    message("Domain: ", n_mult_outcome, " pts have distinct outcomes")
  }
  
  
  # for duration domains
  # if the outcome is set to eliminated then it means that the pt was
  # assigned to the other revision type and so should be dropped from 
  # this domain population
  if(l_dm_def$dur_dom){
    n_elim <- d_dm[(EL_Outcome %in% c("Eliminated")), .N]
    
    if(n_elim){
      message("Domain: ", n_elim, " pts eliminated - alt rev type applies")
      d_dm <- d_dm[!(EL_Outcome %in% c("Eliminated"))]
    }
    
  }
  
  
  # participant list gives arm split
  d_pt_list[, (l_dm_def$trt_col) := lapply(.SD, function(z){ 
    clean_text(z) 
  }), .SDcols = l_dm_def$trt_col]
  # d_pt_list[[l_dm_def$trt_col]]
  
  # merge in the trt allocation arm:
  d_dm <- base::merge(
    d_dm, d_pt_list[, c("StudyPtID", l_dm_def$trt_col), with = F], 
    by = "StudyPtID", all.x = T
    )
  trt_lu <- unlist(l_dm_def$trt_map)
  d_dm[, trt := unname(trt_lu[match(d_dm[[l_dm_def$trt_col]], names(trt_lu))])]
  
  n_mis_alloc <- d_dm[EL_Outcome == "Randomised" & is.na(trt), .N]
  if(n_mis_alloc){
    message("Domain: ", n_mis_alloc, " randomised but missing allocation")
  }
  
  n_mis_out <- d_dm[!(EL_Outcome %in% "Randomised") & !is.na(trt), .N]
  if(n_mis_out){
    message("Domain: ", n_mis_out, " outcome not rand but has allocation")
    
    message("Domain: * Setting allocation to NA")
    
    d_dm[!(EL_Outcome %in% "Randomised") & !is.na(trt), trt := NA_character_]
  }
  
  n_dm <- d_dm[, .N]
  n_dm_rand <- d_dm[EL_Outcome %in% "Randomised", .N]
  # all those from pt list with non NA value for domain allocation.
  d_pt_list_dm <- d_pt_list[ 
    !is.na(get(l_dm_def$trt_col)), c("StudyPtID", l_dm_def$trt_col), with = F]
  n_dm_rand_pt_list <- nrow(d_pt_list_dm)
  
  # These are in the pt list but do not appear in the domain data
  d_pt_list_mis <- d_pt_list_dm[!(StudyPtID %in% d_dm[EL_Outcome == "Randomised", StudyPtID])]
  n_dm_rand_mis <- nrow(d_pt_list_mis)

  msg <- paste(
    "Domain:", 
    n_dm, "remain in domain", 
    n_dm_rand, "of these are revealed to rand",
    n_dm_rand_pt_list, "in the pt list for this domain",
    "(", n_dm_rand_mis, "missing)"
  )
  message(msg)
  
  for(j in seq_len(nrow(d_pt_list_mis))){
    cat("Pt list id missing from domain ", d_pt_list_mis[j, StudyPtID], "\n")
  }
  
  d_dm_smry <- d_dm[, .N, keyby = .(EL_Outcome, trt)]
  for(j in seq_len(nrow(d_dm_smry))){
    if(is.na(d_dm_smry$trt[j])){
      message("Domain: ", d_dm_smry$EL_Outcome[j], ": ", d_dm_smry$N[j])
    } else {
      message("Domain: ", d_dm_smry$EL_Outcome[j], 
              " (", d_dm_smry$trt[j], ") : ", d_dm_smry$N[j])
    }
  }
  message("\n")
  
  d_dm
  
}


# Participants randomised within each domain: one row per participant and
# domain, with the arm that was revealed in the domain. 
domain_rand <- function(l_dm, id_pop){
  
  d_dm_rand <- rbindlist(lapply(names(l_dm), function(dm_code){
    l_dm[[dm_code]][
      EL_Outcome %in% "Randomised" & StudyPtID %in% id_pop,
      .(dm = dm_code, StudyPtID, arm = trt)
    ]
  }))
  # While re-randomisation is technically possible, stop on any duplicates for now.
  stopifnot(!anyDuplicated(d_dm_rand[, .(StudyPtID, dm)]))
  
  # an arm should always be recorded once randomised. Say so rather than 
  # silently dropping the rows later
  if(anyNA(d_dm_rand$arm)){
    message("Domain: ", sum(is.na(d_dm_rand$arm)), " randomised pts with no arm")
  }
  
  d_dm_rand
}

# Participants allocated to each domain at platform randomisation: one row per
# participant and domain, with the arm. This is the allocation held in the
# participant list. The allocation is only revealed once entry into the domain
# has been confirmed and consented, so these rows include allocations that are
# not yet revealed. `revealed` is TRUE where the participant is in d_pt_dm.
domain_alloc <- function(d_pt, d_pt_dm, l_dm_defn){
  
  d_alloc <- rbindlist(lapply(l_dm_defn, function(l_def){
    trt_lu <- unlist(l_def$trt_map)
    d <- data.table(
      dm = l_def$dm,
      StudyPtID = d_pt$StudyPtID,
      code = clean_text(d_pt[[l_def$trt_col]])
    )[!is.na(code)]
    d[, arm := unname(trt_lu[match(code, names(trt_lu))])]
    
    if(anyNA(d$arm)){
      message(
        "Domain: ", l_def$label, ": ", sum(is.na(d$arm)), 
        " allocation codes not in the arm mapping"
      )
    }
    d[, code := NULL]
  }))
  
  d_alloc[, revealed := FALSE]
  d_alloc[d_pt_dm, on = .(StudyPtID, dm), revealed := TRUE]
  
  # a revealed allocation should always be in the allocation held in the list
  n_not_alloc <- d_pt_dm[!d_alloc, on = .(StudyPtID, dm), .N]
  if(n_not_alloc){
    message("Domain: ", n_not_alloc, " revealed allocations are not in the pt list")
  }
  message(
    "Domain: ", uniqueN(d_alloc$StudyPtID), " pts allocated to one or more domains, ",
    uniqueN(d_alloc[revealed == TRUE, StudyPtID]), " with an allocation revealed"
  )
  
  d_alloc
}

# Primary outcome: treatment success at day 365 after platform randomisation.
# One row per participant in d_pt (the reporting population).
#   success - alive, no clinical or microbiological evidence of infection of the
#             index joint and the prosthesis is in place
#   failure - dead, evidence of infection, or the prosthesis is not in place
# A missing or unknown component only gives a failure if another component is 
# a failure, so success and failure can both be FALSE when the outcome is not
# known. status then separates:
#   Outcome missing - the window has opened (day 365 - window) and the outcome
#                     is not known
#   Not yet due     - the window has not opened
d365 <- function(
    d_d365, d_pt, l_d365_cfg
){
  
  # critical fields for primary outcome
  # StudyPtID
  # D365_Complete
  # D365_VitalStatus
  # D365_VitalStatus_IJInfection_ClinicalEvidence
  # D365_VitalStatus_IJInfection_MicrobioEvidence
  # D365_VitalStatus_ProsthesisInPlace
  
  d_o <- d_d365[, .(
    StudyPtID       = clean_text(StudyPtID),
    alive           = str_to_lower(clean_text(D365_VitalStatus)),
    infc_clin       = str_to_lower(clean_text(D365_VitalStatus_IJInfection_ClinicalEvidence)),
    infc_micro      = str_to_lower(clean_text(D365_VitalStatus_IJInfection_MicrobioEvidence)),
    prosth_in_place = str_to_lower(clean_text(D365_VitalStatus_ProsthesisInPlace))
  )]
  d_o <- d_o[!is.na(StudyPtID)]
  stopifnot(!anyDuplicated(d_o$StudyPtID))
  
  # are there any with outcome that are not in ther reporting set?
  n_out <- d_o[!(StudyPtID %in% d_pt$StudyPtID), .N]
  message("D365: ", nrow(d_o), " rows, ", n_out, " not in the reporting population")
  
  d_po <- merge(
    d_pt[, .(StudyPtID, days_rand)], 
    d_o[StudyPtID %in% d_pt$StudyPtID], 
    by = "StudyPtID", all.x = TRUE
  )
  
  d_po[, success := alive %in% "alive" & infc_clin %in% "no" & 
         infc_micro %in% "no" & prosth_in_place %in% "yes"]
  d_po[, failure := alive %in% "dead" | infc_clin %in% "yes" | 
         infc_micro %in% "yes" | prosth_in_place %in% "no"]
  
  win_open <- l_d365_cfg$day - l_d365_cfg$window
  d_po[, status := fcase(
    success,                "Success",
    failure,                "Failure",
    days_rand >= win_open,  "Outcome missing",
    default =               "Not yet due"
  )]
  d_po[, status := factor(
    status, levels = c("Success", "Failure", "Outcome missing", "Not yet due")
  )]
  
  d_po_smry <- d_po[, .N, keyby = status]
  for(j in seq_len(nrow(d_po_smry))){
    message("D365: ", d_po_smry$status[j], ": ", d_po_smry$N[j])
  }
  message("\n")
  
  list(d_po = d_po)
}

# Baseline data for the participants in id_pop (the reporting population).
#   d_bl:    one row per participant, with the variables in the data dictionary
#            (l_cfg$bl$vars) cleaned and typed. A participant with no baseline
#            record is kept with has_bl = FALSE and the values NA.
#   d_micro: the causative organisms in long format, one row per participant
#            and organism. A participant has zero or more rows.
baseline <- function(d_bl, id_pop, l_bl_cfg){
  
  d_bl[, StudyPtID := clean_text(StudyPtID)]
  d_bl <- d_bl[StudyPtID %in% id_pop]
  stopifnot(!anyDuplicated(d_bl$StudyPtID))
  message("Baseline: ", nrow(d_bl), " records for ", length(id_pop), " pts")
  
  # clean each of the variables in the data dictionary
  vars <- l_bl_cfg$vars
  stopifnot("organism_identified" %in% vars$name)
  
  d_clean <- d_bl[, .(
    StudyPtID,
    has_bl      = TRUE,
    bl_complete = yn(BAS_Complete),
    org_amount  = to_num(BAS_Microbio_CausativeOrganismAmount, "organism amount")
  )]
  for(i in seq_len(nrow(vars))){
    v <- vars[i]
    set(d_clean, j = v$name, value = switch(
      v$type,
      num = to_num(d_bl[[v$src]], v$name),
      cat = to_factor(d_bl[[v$src]], v$levels[[1]], v$name),
      bin = to_factor(d_bl[[v$src]], c("No", "Yes"), v$name),
      stop("Baseline: unknown variable type ", v$type)
    ))
  }
  
  # keep everyone in the population, with has_bl = FALSE if there is no record
  d_bl_out <- merge(
    data.table(StudyPtID = id_pop), d_clean, by = "StudyPtID", all.x = TRUE
  )
  d_bl_out[is.na(has_bl), has_bl := FALSE]
  
  d_micro <- baseline_micro(d_bl)
  baseline_micro_check(d_bl_out, d_micro)
  
  list(
    d_bl = d_bl_out,
    d_micro = d_micro
  )
}

# Causative organisms are held wide in the baseline extract, with up to three
# organisms per participant that each have the same set of fields. This turns 
# them into one row per participant and organism.
baseline_micro <- function(d_bl){
  
  cols <- grep("^BAS_Microbio_Org0[1-3]", names(d_bl), value = TRUE)
  d_m <- d_bl[, c("StudyPtID", cols), with = FALSE]
  d_m[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  
  # one measure group per field, each holding the columns for organisms 1 to 3
  d_org <- melt(
    d_m, id.vars = "StudyPtID",
    measure.vars = patterns(
      organism      = "^BAS_Microbio_Org0[1-3]OrganismName$",
      specify       = "^BAS_Microbio_Org0[1-3]OrganismsSpecify$",
      specify_other = "^BAS_Microbio_Org0[1-3]OrganismsSpecifyOther$",
      pre_syn       = "^BAS_Microbio_Org0[1-3]Detected_PreOpSynovial$",
      intra_syn     = "^BAS_Microbio_Org0[1-3]Detected_IntraOpSynovial$",
      intra_tis     = "^BAS_Microbio_Org0[1-3]Detected_IntraOpTissue$",
      blood         = "^BAS_Microbio_Org0[1-3]Detected_BloodCultures$"
    ),
    variable.name = "slot"
  )
  
  # an empty organism name means that the slot was not used
  d_org[, organism := clean_text(organism, tokens_to_na = na_tokens)]
  d_org <- d_org[!is.na(organism)]
  
  d_org[, `:=`(
    specify       = clean_text(specify, tokens_to_na = na_tokens),
    specify_other = clean_text(specify_other, tokens_to_na = na_tokens)
  )]
  # where the organism was detected, as TRUE/FALSE
  detected <- c("pre_syn", "intra_syn", "intra_tis", "blood")
  d_org[, (detected) := lapply(.SD, yn), .SDcols = detected]
  
  d_org
}

# The identified flag, the amount and the organisms listed should agree. Report
# any that do not.
baseline_micro_check <- function(d_bl, d_micro){
  
  d_chk <- merge(
    d_bl[, .(StudyPtID, organism_identified, org_amount)],
    d_micro[, .(n_org = uniqueN(organism)), by = StudyPtID],
    by = "StudyPtID", all.x = TRUE
  )
  d_chk[is.na(n_org), n_org := 0L]
  
  n_yes_none   <- d_chk[organism_identified %in% "Yes" & n_org == 0, .N]
  n_org_notyes <- d_chk[!(organism_identified %in% "Yes") & n_org > 0, .N]
  n_amt_diff   <- d_chk[
    organism_identified %in% "Yes" & n_org > 0 & !is.na(org_amount) & org_amount != n_org, .N
  ]
  if(n_yes_none + n_org_notyes + n_amt_diff > 0){
    message(
      "Baseline: microbiology: identified = Yes with no organism recorded: ", n_yes_none,
      "; organism recorded without identified = Yes: ", n_org_notyes,
      "; amount differs from the number of organisms recorded: ", n_amt_diff
    )
  }
}

# Protocol deviations.
# One row per recorded deviation; a participant can have more than one.
# - Test sites and participants that are not in the randomised set (id_rand,
#   from the screening step) are removed and listed in d_excl.
# - Dates are parsed and the Yes/No fields are recoded to logical.
# - Free text details/action taken, the user audit fields and the names of the
#   people completing the form are not carried through to the derived data.
# Deviations are recorded against the participant and the form has no domain
# field, so there is no domain level attribution here.
deviations <- function(
    d_pd, id_pop,
    test_codes = c("DS9")
){
  
  n_raw <- nrow(d_pd)
  message("PD: raw data has ", n_raw, " rows.")
  
  # the id and the form identifier must be usable
  d_pd[, StudyPtID := clean_text(StudyPtID)]
  stopifnot(!anyNA(d_pd$StudyPtID))
  stopifnot(!anyNA(d_pd$PD_ID), !anyDuplicated(d_pd$PD_ID))
  
  d_pd[, id_prefix := sub("-.*$", "", StudyPtID)]
  
  # removals: test sites, then anyone that is not a randomised participant
  d_pd[, reason_excl := fcase(
    id_prefix %in% test_codes,   "test site",
    !(StudyPtID %in% id_pop),    "not in reporting population",
    default = NA_character_
  )]
  d_excl <- d_pd[!is.na(reason_excl), .(StudyPtID, PD_ID, reason_excl)]
  d_pd <- d_pd[is.na(reason_excl)]
  d_pd[, reason_excl := NULL]
  
  message(
    "PD: removed ", sum(d_excl$reason_excl == "test site"), " test site rows and ",
    sum(d_excl$reason_excl == "not in reporting population"), 
    " rows for pts not in the reporting population; ", 
    nrow(d_pd), " rows remain (", uniqueN(d_pd$StudyPtID), " pts)"
  )
  
  d_pd <- d_pd[, .(
    StudyPtID,
    id_prefix,
    PD_ID,
    protocol_version = clean_text(EL_ProtocolVersion),
    complete         = yn(PD_Complete),
    type             = clean_text(PD_Deviation_Type),
    type_specify     = clean_text(PD_Deviation_TypeSpecify),
    dt_deviation     = parse_dmy_mon(PD_Deviation_Date),
    dt_completed     = parse_dmy_mon(PD_CompletedBy_Date),
    data_integrity   = yn(PD_Outcome_DataIntegrity),
    pt_safety        = yn(PD_Outcome_ParticipantSafety),
    serious_breach   = yn(PD_Outcome_SeriousBreach),
    tmt_agree        = yn(PD_Outcome_TrialMgmtTeamAgree)
  )]
  setorder(d_pd, dt_deviation, PD_ID)
  
  # report on anything unexpected rather than silently dropping it
  if(anyNA(d_pd$type)){
    message("PD: ", sum(is.na(d_pd$type)), " rows with no deviation type")
  }
  if(anyNA(d_pd$dt_deviation)){
    message("PD: ", sum(is.na(d_pd$dt_deviation)), " rows with no valid deviation date")
  }
  n_incomplete <- d_pd[complete %in% FALSE, .N]
  if(n_incomplete){
    message("PD: ", n_incomplete, " rows with the form not complete")
  }
  
  d_pd_smry <- d_pd[, .N, keyby = type]
  for(j in seq_len(nrow(d_pd_smry))){
    message("PD: ", d_pd_smry$type[j], ": ", d_pd_smry$N[j])
  }
  message("\n")
  
  list(
    d_pd   = d_pd,
    d_excl = d_excl
  )
}


main <- function(){
  
  # Load raw data. The raw data is only used in this function but l_raw is 
  # saved to the extract anyway.
  l_raw <- read_extract(l_cfg)
  nms_data <- names(l_raw)
  message("Extracted datasets for: ")
  for(i in seq_len(length(nms_data))){ message("  ", nms_data[i]) }
  
  
  
  # always copy from here otherwise you will change the raw data due
  # to pass by ref
  
  # screening (platform eligibility): who was randomised
  l_dat <- screening(
    d_scr  = copy(l_raw$screening), 
    d_rm  = copy(l_raw$removals), 
    d_wd = copy(l_raw$wd)
    )
  
  # participant list (contains trt allocation arms). l_pt$d_pt is the 
  # reporting population that the steps below are restricted to.
  l_pt <- participants(
    d_pt_list = copy(l_raw$pt_list),
    d_rand = copy(l_dat$d_rand),
    dt_extract = l_cfg$version$dt_extract
    )
  # these are who we report on....
  id_pop <- l_pt$d_pt$StudyPtID
  
  # domains - allocation by domain gives treatment status and revealed
  dm_names <- names(l_cfg$dm_defn)
  l_dm <- lapply(dm_names, function(dm){
    l_dm_def <- l_cfg$dm_defn[[dm]]
    d_dm <- copy(l_raw[[l_dm_def$ref]])
    domain(
      d_dm, 
      l_dm_def,  
      d_rand =  copy(l_dat$d_rand), 
      d_pt_list  = copy(l_pt$d_pt_list)) 
  })
  names(l_dm) <- dm_names
  
  # the domains and trt that each pt in the reporting set are assigned to
  l_pt$d_pt_dm <- domain_rand(l_dm, id_pop)
  
  # the domains and arms that each pt was allocated to at platform
  # randomisation, and whether the allocation has been revealed
  l_pt$d_pt_alloc <- domain_alloc(l_pt$d_pt, l_pt$d_pt_dm, l_cfg$dm_defn)
  
  # primary outcome (day 365 +/- window). Measured on the participant, i.e.
  # at the platform level. To see it at the domain level do a join with 
  # l_pt$d_pt_dm
  l_po <- d365(
    d_d365 = copy(l_raw$d365),
    d_pt = l_pt$d_pt,
    l_d365_cfg = l_cfg$d365
  )
  
  # baseline description for the reporting population. Also platform level, 
  # join l_pt$d_pt_dm to see it by domain and arm.
  l_bl <- baseline(
    d_bl = copy(l_raw$baseline), 
    id_pop = id_pop, 
    l_bl_cfg = l_cfg$bl
  )
  
  # protocol deviations
  l_pd <- deviations(
    d_pd = copy(l_raw$prot_dev),
    id_pop = id_pop,
    test_codes = l_cfg$test_codes
  )
  
  # The derived data. Each element is documented where it is created.
  #   l_cfg - the configuration, including the baseline data dictionary
  #   l_dat - screening and consort counts
  #   l_pt  - d_pt_list, d_pt (reporting population), d_pt_dm (allocation
  #           revealed, by domain), d_pt_alloc (allocated, by domain)
  #   l_dm  - one table per domain (eligibility outcome, arm)
  #   l_bl  - d_bl (baseline, one row per participant), d_micro (organisms)
  #   l_po  - d_po (primary outcome, one row per participant)
  #   l_pd  - d_pd (protocol deviations), d_excl (rows removed)
  #   l_raw - source data
  l_out <- list(
    l_cfg = l_cfg,
    l_dat = l_dat,
    l_pt = l_pt,
    l_dm = l_dm,
    l_bl = l_bl,
    l_po = l_po,
    l_pd = l_pd,
    l_raw = l_raw
  )
  
  dir_out <- here::here("data/derived", l_cfg$version$extract_dttm)
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  f_out <- paste0("dat-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".qs2")
  
  qs2::qs_save(
    l_out,
    file = here::here(dir_out, f_out)
  )
  
  
}


# set DATA_SOURCE_ONLY=true to source the functions (e.g. in a report) 
# without doing any processing.
if(!interactive() && Sys.getenv("DATA_SOURCE_ONLY") != "true"){
  main()
}


