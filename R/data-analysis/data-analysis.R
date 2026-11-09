

library(data.table)
# regex/string cleaning only - data.table handles the tables
library(stringr)   
# library(grid)
# library(ragg)
library(here)
library(lubridate)

source(here::here("R/data-analysis", "utils.R"))


# Config ------


l_cfg <- list()

l_cfg$version <- list(
  report_date = Sys.Date(),
  extract_dttm = "202609021608"
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

l_cfg$bl <- list(

  nms_bl = data.table(
    col_from = c(
      "BAS_Dem_DOB",
      "BAS_Dem_Age",
      "BAS_Dem_Sex",
      "BAS_DIP_IJSide",       
      "BAS_DIP_IJSite",       
      "BAS_DIP_ProthesisType",
      "BAS_Comorb_DiabetesMellitus",     
      "BAS_Comorb_ChronicRenalDisease",  
      "BAS_Comorb_EndStageRenalFailure", 
      "BAS_Comorb_MalignancyPast12M",    
      "BAS_Comorb_Cirrhosis",            
      "BAS_Comorb_RheumatoidArthritis",  
      "BAS_Comorb_CorticosteroidPast6M", 
      "BAS_Comorb_IatrogenicImmunoSupp", 
      "BAS_Comorb_Cardiac",              
      "BAS_Comorb_Obesity",
      "BAS_Bloods_CRP_mgL",                      
      "BAS_Bloods_Antibio_SerumCreatinine_umolL",
      "BAS_Bloods_Antibio_ALT_Entered",          
      "BAS_Bloods_Antibio_AST_Entered",          
      "BAS_Bloods_Antibio_Bilirubin_umolL",  
      "BAS_CQI_ASAClassification",
      "BAS_OKS_PainDescription",  
      "BAS_OKS_WashingDrying",    
      "BAS_OKS_Vehicle",          
      "BAS_OKS_WalkTime",         
      "BAS_OKS_Chair",            
      "BAS_OKS_Limp",             
      "BAS_OKS_Kneel",            
      "BAS_OKS_PainNight",        
      "BAS_OKS_Work",             
      "BAS_OKS_GiveWay",          
      "BAS_OKS_Shopping",         
      "BAS_OKS_Stairs",           
      "BAS_OKS_CompletedBy",      
      "BAS_OKS_NotCompleteReason",
      "BAS_OKS_Score",   
      "BAS_EQ5D5L_Mobility",          
      "BAS_EQ5D5L_PersonalCare",      
      "BAS_EQ5D5L_UsualActivities",   
      "BAS_EQ5D5L_PainDiscomfort",    
      "BAS_EQ5D5L_AnxietyDepression", 
      "BAS_EQ5D5L_HealthToday",       
      "BAS_EQ5D5L_CompletedBy",       
      "BAS_EQ5D5L_NotCompletedReason",
      "BAS_EQ5D5L_Score"             
    )
  ),
  
  nms_bl_mic = data.table(
    col = c(          
      "BAS_Microbio_CausativeOrganismIdentified"  ,
      "BAS_Microbio_CausativeOrganismAmount"  ,    
      "BAS_Microbio_Org01OrganismName"   ,         
      "BAS_Microbio_Org01OrganismsSpecify" ,       
      "BAS_Microbio_Org01OrganismsSpecifyOther" ,  
      "BAS_Microbio_Org01Detected_PreOpSynovial", 
      "BAS_Microbio_Org01Detected_IntraOpSynovial",
      "BAS_Microbio_Org01Detected_IntraOpTissue"  ,
      "BAS_Microbio_Org01Detected_BloodCultures" , 
      "BAS_Microbio_Org02OrganismName"       ,     
      "BAS_Microbio_Org02OrganismsSpecify"   ,     
      "BAS_Microbio_Org02OrganismsSpecifyOther" ,  
      "BAS_Microbio_Org02Detected_PreOpSynovial"  ,
      "BAS_Microbio_Org02Detected_IntraOpSynovial",
      "BAS_Microbio_Org02Detected_IntraOpTissue" , 
      "BAS_Microbio_Org02Detected_BloodCultures",  
      "BAS_Microbio_Org03OrganismName"         ,   
      "BAS_Microbio_Org03OrganismsSpecify" ,       
      "BAS_Microbio_Org03OrganismsSpecifyOther"   ,
      "BAS_Microbio_Org03Detected_PreOpSynovial" , 
      "BAS_Microbio_Org03Detected_IntraOpSynovial",
      "BAS_Microbio_Org03Detected_IntraOpTissue"  ,
      "BAS_Microbio_Org03Detected_BloodCultures"  
    )
  )
  
)

l_cfg$bl$nms_bl[, col_to := copy(col_from)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_Dem_", "", col_to)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_DIP_", "", col_to)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_Comorb", "cmb", col_to)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_Bloods", "bld", col_to)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_CQI_", "", col_to)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_OKS", "oks", col_to)]
l_cfg$bl$nms_bl[, col_to := sub("BAS_EQ5D5L", "qol", col_to)]


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


# process removals data

# each removal file has three cols which relate to
# screening rows, 
# platform participants and 
# registry participants 
# c("eligibility-code", "platform-code", "registry-code")
# respectively which allow for selectively removing units from each dataset.
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
  d_wd[, reason_grp := fcase(
    str_starts(reason, "enrolled in error"), "invalid_enrolment",
    str_starts(reason, "invalid consent"), "invalid_enrolment",
    str_starts(reason, "consent withdrawn"), "consent_withdrawal",
    str_starts(reason, "appeared eligible"), "found_ineligible_later",
    default = "other"
  )]
  
  
  if(any(d_wd[, .N, keyby = StudyPtID][[2]] > 1)){
    n_dup <- d_wd[, .N, keyby = StudyPtID][N > 1, .N]
    message("WD: contains ", n_dup, " duplicate StudyPtIDs")
  }
  
  message("WD: Taking first record for each StudyPtID and dropping any StudyPtID set to NA")
  d_wd <- d_wd[!is.na(StudyPtID), .SD[1], keyby = StudyPtID]
  message("WD: data now has ", nrow(d_wd), " rows.")
  
  list(
    d_wd = d_wd
  )
}

# clean up the screening (platform eligibility) data
screening <- function(
    d_scr, d_rm, d_wd,
    test_codes = c("DS9")
                      ){
  l_dat <- list()
  
  # in contrast to the JM consort script, first I identify the removal records
  # and withdrawals before processing the screen (plt_elig) data
  
  # removals (step) - just picks up the eligibility, platform and registry codes
  # for removal (if there are any)
  l_dat[["rm"]] <- proc_removals(d_rm)
  
  # withdrawals - identifies those that are withdrawn
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
  # completed outcome but no physical-presence value.
  stopifnot(!any(is.na(d_scr$presence) & !is.na(d_scr$outcome)))
  
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
  
  
  # now check the withdrawals for the existence of invalid ids
  ids_invalid_from_wd <- l_dat[["wd"]]$d_wd[
    reason_grp == "invalid_enrolment" & wd_all == "yes", StudyPtID]
  id_invalid_enrl <- union(ids_invalid_from_wd, l_dat[["rm"]]$plt_code)
  
  if(length(id_invalid_enrl)){
    n_found <- nrow(d_scr[StudyPtID %in% id_invalid_enrl, .SD[1], keyby = StudyPtID])
    message("Screening: invalid enrolments ", length(id_invalid_enrl), 
            " found ", n_found, " reclass outcome as Not eligible")
    d_scr[StudyPtID %in% id_invalid_enrl, outcome := "Not eligible" ]
    
    l_dat$d_rand <- l_dat$d_rand[]
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
  l_dat[["d_rand"]] <- d_scr[
    !is.na(StudyPtID) &
      !(EligibilityCode %in% l_dat[["rm"]]$elig_code) &
      !(StudyPtID %in% ids_invalid_from_wd), 
    .SD[1], keyby = StudyPtID]
  
  stopifnot(nrow(l_dat[["d_rand"]]) == sum(d_scr$outcome == "Randomised", na.rm = TRUE))
  
  
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
  
  l_dat$d_screening <- copy(d_scr)
  
  l_dat
}


participants <- function(
    d_pt_list, d_rand,
    test_codes = c("DS9")
    ){
  
  
  v_n <- c(nrow(d_pt_list))
  message("Pts: original has ",   last(v_n), " rows.")
  
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
    message("Pts: ", length(missing_from_list), " pts from screening not in pt list")
  }
  
  # return processed set
  list(
    d_pt_list = d_pt_list
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
  
  
  # multiple outcomes per pt
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
    by = "StudyPtID", all.x = T)
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
  message("Domain: ", n_dm, " remain in domain ", n_dm_rand, " rand")
  
  d_dm_smry <- d_dm[, .N, keyby = .(EL_Outcome, trt)]
  for(j in 1:nrow(d_dm_smry)){
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


# day 365 by domain
d365 <- function(
    d_d365, d_dm
    ){
  
  
  # critical fields for primary outcome?
  # StudyPtID
  # D365_Complete
  # D365_VitalStatus
  # D365_VitalStatus_IJInfection_ClinicalEvidence
  # D365_VitalStatus_IJInfection_MicrobioEvidence
  # D365_VitalStatus_ProsthesisInPlace
  
  setnames(d_d365, "D365_VitalStatus", "alive")
  setnames(d_d365, "D365_VitalStatus_IJInfection_ClinicalEvidence", "infc_clin")
  setnames(d_d365, "D365_VitalStatus_IJInfection_MicrobioEvidence", "infc_micro")
  setnames(d_d365, "D365_VitalStatus_ProsthesisInPlace", "prosth_in_place")
  
  cols <- c(
    "StudyPtID",
    "D365_Complete",  
    "alive",  
    "infc_clin",  
    "infc_micro",  
    "prosth_in_place"
  )
  d_d365[, (cols) := lapply(.SD, function(z){ 
    clean_text(z) 
  }), .SDcols = cols]
  
  
  
  d_dm <- base::merge(
    d_dm, 
    d_d365[, cols, with = F], by = "StudyPtID", all.x = T )
  
  d_dm[, trt_success := (str_to_lower(alive) %in% "alive") & 
         (str_to_lower(infc_clin) %in% "no") &
         (str_to_lower(infc_micro) %in% "no") &
         (str_to_lower(prosth_in_place) %in% "yes")]
  
  d_dm[, c("StudyPtID", "EL_Outcome", cols, "trt_success"), with = F]
}

# picks out domain level data, which could be aggregated over all domains 
# and then set to unique but also probably need all platform participants as
# well 
baseline <- function(
    d_bl, d_dm, l_dm_def
    ){
  
  
  # age
  # sex
  # infection side (left/right)
  # infection joint (hip/knee)
  # prosthesis type
  
  # comorb:
  # Diabetes mellitus 
  # Chronic renal disease (eGFR<50 but not on dialysis) 
  # End stage renal failure (on haemodialysis or CAPD) 
  # Any malignancy past 12 months (excludes skin SCC/BCC) 
  # Cirrhosis
  # Rheumatoid Arthritis 
  # Systemic corticosteroid use past 6 months 
  # Iatrogenic immunosuppression 
  # Congestive cardiac failure 
  # Obesity (BMI>=30) 
  
  # blood test result
  # C-reactive protein (mg/L) 
  # ALT day 1 (+/- 2 days) 
  # AST day 1 (+/- 2 days) 
  # Bilirubin day 1 (+/- 2 days) 
  
  # asa class
  
  # oxford hip scores
  
  # eq5d5l 
  
  setnames(d_bl, l_cfg$bl$nms_bl$col_from, l_cfg$bl$nms_bl$col_to)
  
  
  # microbiology (list)
  nms_micro <- names(d_bl)
  nms_micro <- nms_micro[nms_micro %like% "BAS_Microbio"]
  d_micro <- d_bl[, c("StudyPtID", nms_micro), with = F]
  d_micro[, BAS_Microbio_CausativeOrganismAmount := 
            as.character(BAS_Microbio_CausativeOrganismAmount)]
  cols <- names(d_micro)
  d_micro[, (cols) := lapply(.SD, function(z){ 
    as.character(z) 
  }), .SDcols = cols]
  d_micro_l <- data.table::melt(
    d_micro, id.vars = c(
      "StudyPtID", 
      "BAS_Microbio_CausativeOrganismIdentified", 
      "BAS_Microbio_CausativeOrganismAmount" ))
  d_micro_l[, variable := sub("BAS_Microbio_", "", variable)]
  d_micro_l[, id_mic := substr(variable, 1, 5)]
  d_micro_l[, variable := gsub("Org0[1-9]", "", variable)]
  d_micro_l[, id_mic := as.numeric(substr(id_mic, 4, 5))]
  d_micro_l[, variable := sub("Detected_", "", variable)]
  
  setnames(d_micro_l, "BAS_Microbio_CausativeOrganismIdentified", "identified")
  setnames(d_micro_l, "BAS_Microbio_CausativeOrganismAmount", "quantity")
 
  
  # now just pick up only those relevant to the domain. 
  # will need a separate function or revised functionality to pick out all 
  # platform baseline data
  d_micro_l <- d_micro_l[StudyPtID %in% d_dm$StudyPtID]
  
  # Subset to pts relevant to domain
  # only take a subset of fields, i.e. those relevant to reporting
  d_bl = d_bl[, c("StudyPtID", "BAS_ID", "BAS_Complete", l_cfg$bl$nms_bl$col_to), with = F]
  dm_id <- paste0(l_dm_def$dm, "_ID")
  d_bl <- base::merge(
    d_dm[, c(dm_id, "EL_LocationCode", "StudyPtID", "EL_Outcome"), with = F], 
    d_bl, 
    by = "StudyPtID", all.x = T )
  
  list(
    d_bl = d_bl,
    d_micro = d_micro_l
  )
}


main <- function(){
  
  # Load raw data (utils.R)
  l_raw <- read_extract(l_cfg)
  names(l_raw)
  
  
  # always copy from here otherwise you will change the raw data due
  # to pass by ref
  l_dat <- screening(
    d_scr  = copy(l_raw$screening), 
    d_rm  = copy(l_raw$removals), 
    d_wd = copy(l_raw$wd)
    )
  # names(l_dat)
  # nrow(l_dat$d_rand)
  

  # participant list (contains trt allocation arms)
  l_pt <- participants(
    d_pt_list = copy(l_raw$pt_list),
    d_rand = copy(l_dat$d_rand)
    )
  
  
  # domains - allocation by domain gives treatment status and revealed
  d_rand <- copy(l_dat$d_rand)
  d_pt_list <- copy(l_pt$d_pt_list)
  dm_names <- names(l_cfg$dm_defn)
  l_dm <- lapply(dm_names, function(dm){
    l_dm_def <- l_cfg$dm_defn[[dm]]
    d_dm <- copy(l_raw[[l_dm_def$ref]])
    domain(d_dm, l_dm_def,  d_rand, d_pt_list) 
  })
  names(l_dm) <- dm_names
  
  
  # day 365 +/- 30 primary outcome trt_succss
  # want this by domain, but we can bind the list and then take distinct to
  # get an overall summary of primary outcome 
  # names(l_raw[["d365"]])
  l_dm_365 <- lapply(dm_names, function(dm){
    l_dm_def <- l_cfg$dm_defn[[dm]]
    d_dm <- copy(l_raw[[l_dm_def$ref]])
    d365(d_d365 = copy(l_raw[["d365"]]),  d_dm)
  })
  names(l_dm_365) <- dm_names
  
  
  # baseline description - again, want this by domain but can 
  # aggregate then distinct for overall summary 
  # probably need to do an analogous version for the platform entrants
  # names(l_raw);#  View(l_raw[["baseline"]])
  # names(l_raw[["baseline"]])
  d_bl <- copy(l_raw[["baseline"]])
  
  l_bl <- lapply(dm_names, function(dm){
    l_dm_def <- l_cfg$dm_defn[[dm]]
    d_dm <- copy(l_raw[[l_dm_def$ref]])
    baseline(copy(l_raw[["baseline"]]),  d_dm, l_dm_def)
  })
  names(l_bl) <- dm_names
  
  l_out <- list(
    l_raw = l_raw,
    l_dat = l_dat,
    l_pt = l_pt,
    l_dm = l_dm,
    l_bl = l_bl,
    l_cfg = l_cfg
  )
  
  dir_out <- here::here("data/derived", l_cfg$version$extract_dttm)
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  f_out <- paste0("dat-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".qs2")
  
  qs2::qs_save(
    l_out,
    file = here::here(dir_out, f_out)
  )
  
  
}


# set SIM05_SOURCE_ONLY=true to source the functions (e.g. in a report) 
# without kicking off a run
if(!interactive()){
  main()
}




