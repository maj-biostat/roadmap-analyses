


# EligibilityCode is the row key. Test-site rows are removed first. Blank
# and "Randomised Pending" outcomes are both treated as pending.
proc_screening <- function(d_screening, cfg){
  
  n_rows_read <- nrow(d_screening)
  
  test_site_rows <- is_test_site_row(
    d_screening$StudyPtID, d_screening$EL_SiteCode, cfg$data$test_site_codes)
  d_screening <- d_screening[!test_site_rows]
  
  message("Eligibility rows read: ", n_rows_read, "; test-site rows removed: ",
          sum(test_site_rows), "; rows kept: ", nrow(d_screening))
  
  d_screening[, EligibilityCode := clean_code(EligibilityCode)]
  d_screening[, CON_DomainSD := clean_code(CON_DomainSD)]
  d_screening[, CON_DomainAC := clean_code(CON_DomainAC)]
  d_screening[, CON_DomainDA_DB := clean_code(CON_DomainDA_DB)]
  d_screening[, CON_DomainAC := clean_code(CON_DomainAC)]
  
  if (base::anyNA(d_screening$EligibilityCode)) {
    problem <- paste0("The eligibility export has ", sum(is.na(screening$EligibilityCode)),
                      " row(s) with a NA EligibilityCode.")
    message("Warning: ", problem, " Those rows are being dropped from screening dataset.")
    d_screening <- d_screening[!is.na(EligibilityCode)]
  }
  
  if (base::anyDuplicated(d_screening$EligibilityCode) > 0) {
    stop("The eligibility export has ", sum(duplicated(d_screening$EligibilityCode)), " duplicates")
  }
  
  # unclear why the separate field but retain functionality from JM
  d_screening[, `:=`(
    outcome_key  = str_to_lower(clean_code(EL_Outcome)),
    presence_key = str_to_lower(clean_code(EL_Incl_PhysicallyPresent))
  )]
  
  recognised_outcome <- c("randomised", "randomised pending", "registry",
                          "not eligible", "excluded", "not included")
  bad_outcome <- !(is.na(d_screening$outcome_key) | d_screening$outcome_key %in% recognised_outcome)
  if (any(bad_outcome)) {
    stop(sum(bad_outcome), " row(s) have an unexpected EL_Outcome value. ", call. = FALSE)
  }
  
  d_screening[, `:=`(
    outcome = fcase(
      outcome_key == "randomised", "Randomised",
      outcome_key == "registry", "Registry",
      outcome_key %in% c("not eligible", "excluded", "not included"), "Not eligible",
      # blank and "Randomised Pending"
      default = NA_character_        
    ),
    presence = fcase(
      is.na(presence_key), NA_character_,
      presence_key == "yes", "Yes",
      presence_key == "no", "No",
      default = "<unrecognised>"
    )
  )]
  d_screening[, c("outcome_key", "presence_key") := NULL]
  
  d_screening[]
}



