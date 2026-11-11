

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

# Data dictionaries: one row per variable that is reported, in the order it is
# reported. A dictionary drives the cleaning of the variables (clean_vars()) and
# the layout of the table of them in the report, so a variable is added in one
# place. There is a dictionary for the baseline data and one for day 100.
#   name   - variable name in the derived data
#   src    - column in the raw extract
#   label  - plain text label used in the report
#   type   - one of
#              num  - a number
#              cat  - one of `levels`. If `levels` is NULL they are the values 
#                     that are found in the data, most common first
#              bin  - Yes/No. A checkbox (Checked/Unchecked) is read as Yes/No
#              days - a date, as the days from platform randomisation
#              text - free text, which is not summarised: only whether there is any
#   levels - the allowed values of a cat variable; anything else is set to NA
#   group  - optional heading the variable is reported under
#   when   - optional name of a bin variable. The variable is only reported for
#            the participants that have Yes for it.
dict_var <- function(
    name, src, label, type, levels = NULL, group = NA_character_, 
    when = NA_character_
){
  data.table(
    name = name, src = src, label = label, type = type, 
    levels = list(levels), group = group, when = when
  )
}

l_cfg$bl <- list(
  vars = rbind(
    dict_var("age", "BAS_Dem_Age", "Age, years", "num"),
    dict_var("sex", "BAS_Dem_Sex", "Sex", "cat", c("Female", "Male")),
    dict_var("joint", "BAS_DIP_IJSite", "Index joint", "cat", c("Hip", "Knee")),
    dict_var("side", "BAS_DIP_IJSide", "Index joint side", "cat", c("Left", "Right")),
    dict_var("prosthesis_type", "BAS_DIP_ProthesisType", "Prosthesis type", "cat", 
           c("Primary", "Revision")),
    
    dict_var("organism_identified", "BAS_Microbio_CausativeOrganismIdentified",
             "Causative organism identified", "bin", group = "Microbiology"),
    
    dict_var("cmb_diabetes", "BAS_Comorb_DiabetesMellitus", 
           "Diabetes mellitus", "bin", group = "Comorbidities"),
    dict_var("cmb_renal", "BAS_Comorb_ChronicRenalDisease",
           "Chronic renal disease", "bin", group = "Comorbidities"),
    dict_var("cmb_esrf", "BAS_Comorb_EndStageRenalFailure",
           "End stage renal failure", "bin", group = "Comorbidities"),
    dict_var("cmb_malignancy", "BAS_Comorb_MalignancyPast12M",
           "Malignancy in past 12 months", "bin", group = "Comorbidities"),
    dict_var("cmb_cirrhosis", "BAS_Comorb_Cirrhosis",
           "Cirrhosis", "bin", group = "Comorbidities"),
    dict_var("cmb_ra", "BAS_Comorb_RheumatoidArthritis",
           "Rheumatoid arthritis", "bin", group = "Comorbidities"),
    dict_var("cmb_steroid", "BAS_Comorb_CorticosteroidPast6M",
           "Systemic corticosteroid use in past 6 months", "bin", group = "Comorbidities"),
    dict_var("cmb_immunosupp", "BAS_Comorb_IatrogenicImmunoSupp",
           "Iatrogenic immunosuppression", "bin", group = "Comorbidities"),
    dict_var("cmb_cardiac", "BAS_Comorb_Cardiac",
           "Congestive cardiac failure", "bin", group = "Comorbidities"),
    dict_var("cmb_obesity", "BAS_Comorb_Obesity",
           "Obesity (BMI >= 30)", "bin", group = "Comorbidities"),
    
    dict_var("asa", "BAS_CQI_ASAClassification", 
             "ASA classification", "cat",
             c("ASA I", "ASA II", "ASA III", "ASA IV", "ASA V", "ASA VI", "Not documented")),
    
    dict_var("bld_crp", "BAS_Bloods_CRP_mgL", 
           "C-reactive protein, mg/L", "num", group = "Blood tests"),
    dict_var("bld_creatinine", "BAS_Bloods_Antibio_SerumCreatinine_umolL",
           "Serum creatinine, umol/L", "num", group = "Blood tests"),
    dict_var("bld_alt", "BAS_Bloods_Antibio_ALT_Entered", 
           "ALT", "num", group = "Blood tests"),
    dict_var("bld_ast", "BAS_Bloods_Antibio_AST_Entered", 
           "AST", "num", group = "Blood tests"),
    dict_var("bld_bilirubin", "BAS_Bloods_Antibio_Bilirubin_umolL",
           "Bilirubin, umol/L", "num", group = "Blood tests"),
    
    dict_var("ohs", "BAS_OHS_Score", 
           "Oxford Hip Score (0-48)", "num", group = "Patient reported outcomes"),
    dict_var("oks", "BAS_OKS_Score",
           "Oxford Knee Score (0-48)", "num", group = "Patient reported outcomes"),
    dict_var("eq5d_score", "BAS_EQ5D5L_Score",
           "EQ-5D-5L score", "num", group = "Patient reported outcomes")
  )
)

# Day 100 form: changes to antibiotics and suspected adverse reactions. The
# variables that are reported are in the data dictionary. 
l_cfg$d100 <- list(
  day = 100,
  vars = rbind(
    dict_var("ab_change", "D100_AR_ABChangeOrCessation", 
             "Antibiotic change or cessation", "bin"),
    dict_var("cdiff", "D100_AR_CDifficileDiarrhoea", 
             "Clostridioides difficile diarrhoea", "bin"),
    
    # only for those that had a change or cessation of antibiotics
    dict_var("ab_change_days", "D100_AR_ABChangeDate", 
             "Days from randomisation to change", "days",
             group = "Participants with an antibiotic change or cessation", 
             when = "ab_change"),
    dict_var("ab_first", "D100_AR_Antibiotic_1stChange", 
             "First antibiotic changed", "cat",
             group = "Participants with an antibiotic change or cessation", 
             when = "ab_change"),
    dict_var("ab_first_other", "D100_AR_Antibiotic_1stChangeSpecify", 
             "Other antibiotic specified", "text",
             group = "Participants with an antibiotic change or cessation", 
             when = "ab_change"),
    
    dict_var("ar_nausea", "D100_AR_SuspAR_NauseaVomiting", 
             "Nausea or vomiting", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_diarrhoea", "D100_AR_SuspAR_Diarrhoea", 
             "Diarrhoea", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_hepato", "D100_AR_SuspAR_Hepatotoxicity", 
             "Hepatotoxicity", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_rash", "D100_AR_SuspAR_RashOrItch", 
             "Rash or itch", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_neuro", "D100_AR_SuspAR_Neurological", 
             "Neurological", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_musculo", "D100_AR_SuspAR_Musculoskeletal", 
             "Musculoskeletal", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_haem", "D100_AR_SuspAR_Hematological", 
             "Haematological", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_renal", "D100_AR_SuspAR_RenalToxicity", 
             "Renal toxicity", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_hypersens", "D100_AR_SuspAR_Hypersensitivity", 
             "Hypersensitivity", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_other", "D100_AR_SuspAR_Other", 
             "Other", "bin", group = "Suspected adverse reactions"),
    dict_var("ar_other_text", "D100_AR_SuspAR_OtherSpecify", 
             "Other reaction described", "text", group = "Suspected adverse reactions")
  )
)

# Primary outcome: treatment success at day 365 after platform randomisation.
# The outcome can be assessed within `window` days either side of `day`, so the 
# window closes on day 395. An outcome that has not been recorded is missing 
# once the window has closed and is not yet due until then. Treatment success is a composite of
# the components on the day 365 form, defined here once and used to derive the
# outcome in d365() and to report each component:
#   name   - variable name in the derived data (l_po$d_po)
#   src    - column in the raw day 365 data
#   label  - used in the report
#   levels - the allowed responses
#   good   - the response that counts towards treatment success
#   bad    - the response that is a failure of treatment
#   event  - what the failure response means, for the report
# Treatment success is all of the components having the good response and
# failure is any of them having the bad response.
d365_comp <- function(name, src, label, levels, good, bad, event){
  data.table(
    name = name, src = src, label = label, levels = list(levels),
    good = good, bad = bad, event = event
  )
}

l_cfg$d365 <- list(
  day = 365, 
  window = 30,
  # What a component with no response is called: missing once the window has 
  # closed and not yet due until then.
  no_response = c(
    missing = "Not recorded", 
    not_due = "Not yet due"
  ),
  components = rbind(
    d365_comp(
      "vital_status", "D365_VitalStatus", "Vital status",
      c("Alive", "Dead", "Unknown"), good = "Alive", bad = "Dead", 
      event = "dead"
    ),
    d365_comp(
      "infc_clin", "D365_VitalStatus_IJInfection_ClinicalEvidence", 
      "Clinical evidence of infection",
      c("No", "Yes", "Unknown"), good = "No", bad = "Yes",
      event = "clinical evidence of infection"
    ),
    d365_comp(
      "infc_micro", "D365_VitalStatus_IJInfection_MicrobioEvidence", 
      "Microbiological evidence of infection",
      c("No", "Yes", "Unknown"), good = "No", bad = "Yes",
      event = "microbiological evidence of infection"
    ),
    d365_comp(
      "prosth_in_place", "D365_VitalStatus_ProsthesisInPlace", "Prosthesis in place",
      c("Yes", "No", "Unknown"), good = "Yes", bad = "No",
      event = "prosthesis not in place"
    )
  )
)


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

# A checkbox is exported as Checked/Unchecked. Read these as Yes/No.
as_yes_no <- function(x){
  x <- clean_text(x, tokens_to_na = na_tokens)
  x_lc <- str_to_lower(x)
  x[x_lc %in% "checked"]   <- "Yes"
  x[x_lc %in% "unchecked"] <- "No"
  x
}

# The values found in x, most common first
observed_levels <- function(x){
  x <- clean_text(x, tokens_to_na = na_tokens)
  n <- table(x)
  names(n)[order(-as.integer(n), names(n))]
}

# Clean the variables in a data dictionary (see dict_var()). Returns the
# participant id and the cleaned variables, in the order of d_raw. A variable of
# type days needs the date of platform randomisation (dt_rand) in d_raw.
clean_vars <- function(d_raw, vars){
  
  d_out <- data.table(StudyPtID = d_raw$StudyPtID)
  
  for(i in seq_len(nrow(vars))){
    v <- vars[i]
    x <- d_raw[[v$src]]
    
    value <- switch(
      v$type,
      num  = to_num(x, v$name),
      cat  = to_factor(
        x, if(is.null(v$levels[[1]])) observed_levels(x) else v$levels[[1]], v$name
      ),
      bin  = to_factor(as_yes_no(x), c("No", "Yes"), v$name),
      days = as.integer(parse_dmy_mon(x) - d_raw$dt_rand),
      text = clean_text(x, tokens_to_na = na_tokens),
      stop("Clean: unknown variable type ", v$type)
    )
    set(d_out, j = v$name, value = value)
  }
  d_out
}

# imports all csv data from source dir
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
    
  }
  
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

# Clean up the screening (platform eligibility) data.
# The intended structure of the screening steps is not entirely clear.
# Threfore, I have largely followed what seems to be the intent in the consort 
# generation script did from JM. THe following needs to be thoroughly checked.
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
  
  
  # standardise screening outcomes per line 188 in JM code
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
    "Vector of rand pts after removals have been processed",
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
  d_pt_list[, (cols) := lapply(.SD, function(z){ clean_text(z) }), .SDcols = cols]
  
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
  
  # Randomised participants that are in the participant list. Currently some 
  # randomised participants are missing from the list, per message above. 
  # All of the processing that follows (baseline, primary outcome, deviations, 
  # domains) is restricted to these participants so every table in the report 
  # has the same denominator.
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
  #  d_pt_list: participant list with test sites removed
  #  d_pt:      reporting population with site, randomisation date and days
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
  message("Domain: ", l_dm_def$label, " has ",   last(v_n), " rows in the raw data.")
  
  cols <- c(
    "StudyPtID",  
    "EL_Outcome"
  )
  d_dm[,(cols) := lapply(.SD, function(z){clean_text(z)}), .SDcols = cols]
  # NA will be assumed to be pending
  message("Domain: assuming NA in EL_outcome implies pending state")
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
  
  
  # are there any pts in the domain data that do not appear in the 
  # rand data. why this would be permitted by the database is unknown as it
  # would seem to violate a logical relational constraint.
  n_excl <- d_dm[!(StudyPtID %in% unique(d_rand$StudyPtID)), .N]
  if(n_excl){
    message("Domain: exclude ", n_excl, " pts that did not appear in rand data")
    d_dm <- d_dm[StudyPtID %in% unique(d_rand$StudyPtID)]
  }
  
  
  # are there any pts that have multiple values for their outcome listed?
  d_dm_tmp <- unique(d_dm[, .(StudyPtID, EL_Outcome)])
  n_mult_outcome <- d_dm_tmp[, .N, keyby = StudyPtID][N>1, .N]
  if(n_mult_outcome){
    message("Domain: ", n_mult_outcome, " pts have multiple outcome values")
  }
  
  
  # for duration domains only!!
  # if the outcome is set to eliminated, it means that the pt was
  # assigned to the other revision type and so they should not be counted as 
  # being part of this (duration) domain
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
  # all possible treatment arms for this domain:
  trt_lu <- unlist(l_dm_def$trt_map)
  d_dm[, trt := unname(trt_lu[match(d_dm[[l_dm_def$trt_col]], names(trt_lu))])]
  
  n_mis_alloc <- d_dm[EL_Outcome == "Randomised" & is.na(trt), .N]
  if(n_mis_alloc){
    message("Domain: ", n_mis_alloc, " randomised but missing allocation")
  }
  
  # If the dm data shows a pt recorded as randomised but without a trt allocated
  # then something is wrong.
  n_mis_out <- d_dm[!(EL_Outcome %in% "Randomised") & !is.na(trt), .N]
  if(n_mis_out){
    message("Domain: ", n_mis_out, " outcome not rand but has allocation")
    message("Domain: * Setting allocation to NA")
    d_dm[!(EL_Outcome %in% "Randomised") & !is.na(trt), trt := NA_character_]
  }
  
  n_dm <- d_dm[, .N]
  n_dm_rand <- d_dm[EL_Outcome %in% "Randomised", .N]
  # all those from pt list with non NA value for domain trt allocation.
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
  
  # build up domain/pt/allocation based on the platform data
  d_alloc <- rbindlist(lapply(l_dm_defn, function(l_def){
    trt_lu <- unlist(l_def$trt_map)
    # form a data set of study pt and their original treatment allocation 
    # code
    d <- data.table(
      dm = l_def$dm,
      StudyPtID = d_pt$StudyPtID,
      code = clean_text(d_pt[[l_def$trt_col]])
    )[!is.na(code)]
    # add in the actual treatment arm based on our lookup table
    d[, arm := unname(trt_lu[match(code, names(trt_lu))])]
    
    if(anyNA(d$arm)){
      message(
        "Domain: ", l_def$label, ": ", sum(is.na(d$arm)), 
        " allocation codes not in the arm mapping"
      )
    }
    d[, code := NULL]
  }))
  
  # set revealed to false for all pts
  d_alloc[, revealed := FALSE]
  # for those that appear in the randomised revealed set in the domain data
  # update revealed status to true.
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

# Primary outcome is treatment success at day 365 after platform randomisation.
# One row per participant in d_pt, which holds the population that we report on.
#
#   success - all of the components have the good response: alive, no clinical
#             or microbiological evidence of infection of the index joint and 
#             the prosthesis is in place
#   failure - any of the components has the bad response: dead, evidence of
#             infection, or the prosthesis is not in place
#
# The components are defined in l_cfg$d365$components. 
#
# We never assume success. Every component needs to have the good response.
# So, a participant with a missing or unknown component and no failure is not a 
# success. A failure on any one component is a failure of the composite whatever
# the other components are. For example, a participant that has died is a failure
# and their other components are not applicable. Success and failure indicators 
# can both be FALSE when the outcome is not known. It is the status that then 
# separates, by the days since platform randomisation:
#
#   Outcome missing - the window has closed (after day 365 + window) and the 
#                     outcome is not known
#   Not yet due     - the window has not closed (up to and including day 
#                     365 + window) and the outcome is not known
#
# Returns
#   d_po:      one row per participant with the component responses (as 
#              factors), success, failure and status. Three more fields:
#                complete_case  - TRUE if the outcome is known, which is a 
#                                 success or a failure. These are the 
#                                 participants in a complete case analysis. A
#                                 failure is included whatever the other 
#                                 components are.
#                comps_complete - TRUE if every component has the good or the 
#                                 bad response (none is missing or unknown)
#                n_comp_missing - the number of components that are missing 
#                                 or unknown, for multiple imputation of the 
#                                 components
#              The outcome is missing, and to be imputed, if status is 
#              Outcome missing. For participants that 
#              died, the date of death and the days from platform randomisation.
#   d_po_comp: the components in long format, one row per participant and 
#              component. The response is the response on the day 365 form or,
#              if there is none, Not recorded (the window has closed) or Not 
#              yet due (see l_d365_cfg$no_response).
d365 <- function(
    d_d365, d_pt, l_d365_cfg
){
  
  # all the detail that we need about the the comparisons
  comps <- l_d365_cfg$components
  win_close <- l_d365_cfg$day + l_d365_cfg$window
  no_resp   <- l_d365_cfg$no_response
  
  # critical fields for primary outcome
  # StudyPtID
  # D365_Complete
  # D365_VitalStatus
  # D365_VitalStatus_IJInfection_ClinicalEvidence
  # D365_VitalStatus_IJInfection_MicrobioEvidence
  # D365_VitalStatus_ProsthesisInPlace
  
  d_o <- d_d365[, .(
    StudyPtID   = clean_text(StudyPtID),
    dt_death    = parse_dmy_mon(D365_VitalStatus_DOD),
    # only record dod unknown if we record that there has been a check done
    dod_unknown = str_to_lower(clean_text(D365_VitalStatus_DOD_Unknown)) %in% "checked"
  )]
  # responses, as factors with the allowed levels
  for(i in seq_len(nrow(comps))){
    v <- comps[i]
    
    # bind in the status of each component of the composite
    # set in data.table where i = NULL results in all rows. 
    # value is the col we are adding
    set(d_o, i = NULL, j = v$name, value = to_factor(
      str_to_title(clean_text(d_d365[[v$src]])), v$levels[[1]], v$name
    ))
  }
  d_o <- d_o[!is.na(StudyPtID)]
  stopifnot(!anyDuplicated(d_o$StudyPtID))
  
  # are there any with outcome that are not in ther reporting set?
  # YES, there is!!
  # so again, we seem to have some data wanting to creep through even thought 
  # it is not in the pt list, i.e the data on which we report
  n_out <- d_o[!(StudyPtID %in% d_pt$StudyPtID), .N]
  message("D365: ", nrow(d_o), " rows, ", n_out, " not in the reporting population")
  
  d_po <- merge(
    d_pt[, .(StudyPtID, dt_rand, days_rand)], 
    d_o[StudyPtID %in% d_pt$StudyPtID], 
    by = "StudyPtID", all.x = TRUE
  )
  
  # the composite: all good for success, any bad for failure
  # build a matrix with a col for each component and the value for each cell to 
  # true it is in the good state for the component being considered
  is_good <- vapply(seq_len(nrow(comps)), function(i){
    d_po[[comps$name[i]]] %in% comps$good[i]
  }, logical(nrow(d_po)))
  # as abouve but for the bad state
  is_bad <- vapply(seq_len(nrow(comps)), function(i){
    d_po[[comps$name[i]]] %in% comps$bad[i]
  }, logical(nrow(d_po)))
  # success only if all are good
  d_po[, success := rowSums(is_good) == nrow(comps)]
  # failure if any bad
  d_po[, failure := rowSums(is_bad) > 0]
  
  # the outcome is known if it is a success or a failure. This is the complete
  # case set of pts. 
  # A success reqs all components to be presetn, but a failure is included 
  # irrespective of whether the other components are presetn or not.
  d_po[, complete_case := success | failure]
  
  # a component with neither the good nor the bad response is missing or unknown
  # how many of the components were missing by pt?
  d_po[, n_comp_missing := nrow(comps) - rowSums(is_good | is_bad)]
  d_po[, comps_complete := n_comp_missing == 0]
  
  # success is never assumed: it needs every component to have the good response
  stopifnot(!any(d_po$success & !d_po$comps_complete))
  
  d_po[, status := fcase(
    success,                "Success",
    failure,                "Failure",
    days_rand > win_close,  "Outcome missing",
    default =               "Not yet due"
  )]
  d_po[, status := factor(
    status, levels = c("Success", "Failure", "Outcome missing", "Not yet due")
  )]
  
  d_po_smry <- d_po[, .N, keyby = status]
  for(j in seq_len(nrow(d_po_smry))){
    message("D365: ", d_po_smry$status[j], ": ", d_po_smry$N[j])
  }
  # a failure can have components that are missing; an outcome that is missing
  # always does (and none of its components is a failure)
  message(
    "D365: ", d_po[status == "Failure" & !comps_complete, .N], " of the ", 
    d_po[status == "Failure", .N], " failures have a component that is missing ", 
    "or unknown, ", d_po[status == "Failure" & !comps_complete & vital_status %in% "Dead", .N],
    " of whom have died"
  )
  
  # deaths: days from platform randomisation to the date of death
  d_po[, days_to_death := as.integer(dt_death - dt_rand)]
  d_po[, dt_rand := NULL]
  n_dead <- d_po[vital_status %in% "Dead", .N]
  message(
    "D365: ", n_dead, " deaths, ", d_po[vital_status %in% "Dead" & !is.na(days_to_death), .N],
    " with a date of death"
  )
  # why can we get deaths happening outside what is logically possible>?>??
  n_odd <- d_po[!is.na(days_to_death) & (days_to_death < 0 | days_to_death > days_rand), .N]
  if(n_odd){
    message("D365: ", n_odd, " dates of death before randomisation or after the extract")
  }
  message("\n")
  
  # components in long format. A component with no response is Not recorded 
  # once the window has closed and Not yet due before that.
  d_po_comp <- rbindlist(lapply(seq_len(nrow(comps)), function(i){
    v <- comps[i]
    resp <- d_po[[v$name]]
    data.table(
      StudyPtID = d_po$StudyPtID,
      component = v$name,
      response  = factor(
        fcase(
          !is.na(resp),                as.character(resp),
          d_po$days_rand > win_close,  no_resp[["missing"]],
          default =                    no_resp[["not_due"]]
        ),
        levels = c(v$levels[[1]], unname(no_resp))
      )
    )
  }))
  d_po_comp[, component := factor(component, levels = comps$name)]
  
  list(
    d_po = d_po,
    d_po_comp = d_po_comp
  )
}

# Day 100 form giving changes to antibiotics and suspected adverse reactions.
# One row per participant in d_pt (reporting population). A participant
# without a day 100 form has has_d100 = FALSE and the values NA, so the 
# summaries should be of those with a form. The variables are cleaned as set 
# out in the data dictionary l_cfg$d100$vars. The date of the change in 
# antibiotics is the days from platform randomisation. 
d100 <- function(
    d_d100, d_pt, l_d100_cfg
){
  
  d_d100[, StudyPtID := clean_text(StudyPtID)]
  d_d100 <- d_d100[!is.na(StudyPtID)]
  stopifnot(!anyDuplicated(d_d100$StudyPtID))
  
  n_out <- d_d100[!(StudyPtID %in% d_pt$StudyPtID), .N]
  message("D100: ", nrow(d_d100), " rows, ", n_out, " not in the reporting population")
  
  # the forms for the reporting population, with the date of randomisation
  d_form <- merge(
    d_pt[, .(StudyPtID, dt_rand)], 
    d_d100[StudyPtID %in% d_pt$StudyPtID], 
    by = "StudyPtID"
  )
  
  d_ar <- clean_vars(d_form, l_d100_cfg$vars)
  d_ar[, `:=`(has_d100 = TRUE, complete = yn(d_form$D100_Complete))]
  
  # report anything that looks wrong rather than silently using it
  n_early <- d_ar[!is.na(ab_change_days) & ab_change_days < 0, .N]
  if(n_early){
    message("D100: ", n_early, " antibiotic changes dated before platform randomisation")
  }
  n_no_change <- d_ar[
    !(ab_change %in% "Yes") & (!is.na(ab_change_days) | !is.na(ab_first)), .N
  ]
  if(n_no_change){
    message(
      "D100: ", n_no_change, " pts with a date or antibiotic of a change but ",
      "antibiotic change or cessation is not Yes"
    )
  }
  
  # everyone in the reporting population, with has_d100 = FALSE if there is no form
  d_ar <- merge(d_pt[, .(StudyPtID, days_rand)], d_ar, by = "StudyPtID", all.x = TRUE)
  d_ar[is.na(has_d100), has_d100 := FALSE]
  
  message(
    "D100: ", d_ar[has_d100 == TRUE, .N], " of ", nrow(d_ar), " pts have a day 100 form, ", 
    d_ar[complete %in% TRUE, .N], " complete"
  )
  message("\n")
  
  list(d_ar = d_ar)
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
  
  # wrapper around clean funcs
  d_clean <- clean_vars(d_bl, vars)
  d_clean[, `:=`(
    has_bl      = TRUE,
    bl_complete = yn(d_bl$BAS_Complete),
    org_amount  = to_num(d_bl$BAS_Microbio_CausativeOrganismAmount, "organism amount")
  )]
  
  # keep everyone in the population, with has_bl = FALSE if there is no record
  d_bl_out <- merge(
    data.table(StudyPtID = id_pop), d_clean, by = "StudyPtID", all.x = TRUE
  )
  d_bl_out[is.na(has_bl), has_bl := FALSE]
  
  
  # micro data done separately 
  d_micro <- baseline_micro(d_bl)
  baseline_micro_check(d_bl_out, d_micro)
  
  list(
    d_bl = d_bl_out,
    d_micro = d_micro
  )
}

# Causative organisms are held wide in the baseline extract. In the dummy data
# there are up to three organisms per participant that each have the same set 
# of fields but unclear whether three is the upper limit. 
# Here we assume an upper limit of 9 and convert to one row per participant 
# and organism.
baseline_micro <- function(
    d_bl
    ){
  
  cols <- grep("^BAS_Microbio_Org0.", names(d_bl), value = TRUE)
  
  n_org <- length(unique(substr(cols, 14, 18)))
  stopifnot(n_org < 10)
  message("BLMicro: pt data have up to ", n_org, " organisms identified")
  
  d_m <- d_bl[, c("StudyPtID", cols), with = FALSE]
  d_m[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  
  # one measure group per field, each holding the columns for organisms 1 to 3
  d_org <- melt(
    d_m, id.vars = "StudyPtID",
    measure.vars = patterns(
      organism      = "^BAS_Microbio_Org0[1-9]OrganismName$",
      specify       = "^BAS_Microbio_Org0[1-9]OrganismsSpecify$",
      specify_other = "^BAS_Microbio_Org0[1-9]OrganismsSpecifyOther$",
      pre_syn       = "^BAS_Microbio_Org0[1-9]Detected_PreOpSynovial$",
      intra_syn     = "^BAS_Microbio_Org0[1-9]Detected_IntraOpSynovial$",
      intra_tis     = "^BAS_Microbio_Org0[1-9]Detected_IntraOpTissue$",
      blood         = "^BAS_Microbio_Org0[1-9]Detected_BloodCultures$"
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
  # where the organism was detected (options are PreOpSynovial, IntraOpSynovial,
  # IntraOpTissue or BloodCultures) set to as TRUE/FALSE
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
# - Test sites and participants that are not in randomised set (id_rand,
#   from the screening step) are removed and listed in d_excl.
# - Dates are parsed and the Yes/No fields are recoded to logical.
# - Free text details/action taken, the user audit fields and the names of 
#   people completing form are not carried through to the derived data.
# Deviations are recorded against the participant and form has no domain
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
  
  # site
  d_pd[, id_prefix := sub("-.*$", "", StudyPtID)]
  
  # removals: test sites, then anyone that is not a randomised participant
  # no EL_SiteCode in PD data
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
      d_pt_list  = copy(l_pt$d_pt_list)
      ) 
  })
  names(l_dm) <- dm_names
  
  # just extract the set of randomised domain pts
  l_pt$d_pt_dm <- domain_rand(l_dm, id_pop)
  
  # the domains and arms that each pt was allocated to at platform
  # randomisation, and whether the allocation has been revealed
  l_pt$d_pt_alloc <- domain_alloc(l_pt$d_pt, l_pt$d_pt_dm, l_cfg$dm_defn)
  
  # primary outcome (day 365 +/- window). Measured on the participant, i.e.
  # at the platform level. To see it at the domain level do a join with 
  # l_pt$d_pt_dm
  l_po <- d365(
    d_d365 = copy(l_raw$d365),
    d_pt = copy(l_pt$d_pt),
    l_d365_cfg = l_cfg$d365
  )
  
  # day 100 form: antibiotic changes and suspected adverse reactions. Platform
  # level, join l_pt$d_pt_dm to see it by domain and arm.
  l_d100 <- d100(
    d_d100 = copy(l_raw$d100),
    d_pt = l_pt$d_pt,
    l_d100_cfg = l_cfg$d100
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
  #   l_po  - d_po (primary outcome, one row per participant), d_po_comp 
  #           (its components, one row per participant and component)
  #   l_d100 - d_ar (day 100 form: antibiotic changes and suspected adverse
  #           reactions, one row per participant)
  #   l_pd  - d_pd (protocol deviations), d_excl (rows removed)
  #   l_raw - source data
  l_out <- list(
    l_cfg = l_cfg,
    l_dat = l_dat,
    l_pt = l_pt,
    l_dm = l_dm,
    l_bl = l_bl,
    l_po = l_po,
    l_d100 = l_d100,
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


