


proc_removals <- function(d_removals, cfg){
  
  rm_cols <- cfg$data$d_file_desc[name == "removals", cols[[1]]]
  d_removals[, (rm_cols) := lapply(.SD, clean_removal_code), .SDcols = rm_cols]
  
  # Looks for rows where all fields are simultaneously set to NA. 
  # Uncertain why this would arise but retain the functionality from JM
  rows_all_blank <- rowSums(!is.na(d_removals[, .SD, .SDcols = rm_cols])) == 0
  if (any(rows_all_blank)) {
    stop("The removal list has ", sum(rows_all_blank),
         " row(s) with all three codes blank.", call. = FALSE)
  }
  # It looks like for a given row, some cells may be set to NA while others
  # have a non-null value. Here we check for duplicates and hard stop if found
  for (colm in rm_cols) {
    # clean out NAs 
    codes <- drop_na_values(d_removals[[colm]])
    if (anyDuplicated(codes) > 0) {
      stop("The removal list column ", colm, " has duplicate codes.", call. = FALSE)
    }
  }
  
  # build rm list for future use
  rm_list <- list(
    eligibility_codes = unique(drop_na_values(d_removals[["eligibility-code"]])),
    platform_ids = unique(drop_na_values(d_removals[["platform-code"]])),
    registry_ids = unique(drop_na_values(d_removals[["registry-code"]]))
  )
  message("Removal list: ", 
          length(rm_list$eligibility_codes), " eligibility code(s), ", 
          length(rm_list$platform_ids), " platform code(s), ", 
          length(rm_list$registry_ids), " registry code(s)")
  
  rm_list
}


# For now, only a listed EligibilityCode removes a screening row. 
# Physical presence equal to "No" remains in the denominator unless that 
# row is listed in the rm_list
apply_screen_removals <- function(d_screening, rm_list) {
  
  n_screened_input <- nrow(d_screening)
  listed_for_removal <- d_screening$EligibilityCode %in% rm_list$eligibility_codes
  
  # Carry removed randomised IDs into the domain exclusions.
  is_removed_randomised <- listed_for_removal &
    !is.na(d_screening$outcome) & d_screening$outcome == "Randomised"
  removed_randomised_ids <- clean_code(d_screening$StudyPtID[is_removed_randomised])
  if (anyNA(removed_randomised_ids)) {
    stop("A removed d_screening row with outcome Randomised has no StudyPtID. ", 
         call. = FALSE)
  }
  if (anyDuplicated(removed_randomised_ids) > 0) {
    stop("Two removed Randomised d_screening rows share one StudyPtID. ", call. = FALSE)
  }
  
  # total listed for removal
  n_removed_listed <- sum(listed_for_removal)
  
  # pt was not present and listed for removal
  n_removed_not_present <- sum(listed_for_removal & d_screening$presence == "No", na.rm = TRUE)
  
  # pt wasn't present for elig assess but not listed for removal
  n_kept_not_present <- sum(!listed_for_removal & d_screening$presence == "No", na.rm = TRUE)
  
  # todo - need to check
  # drop all listed for removal.. - need to check...
  d_screening <- d_screening[!listed_for_removal]
  
  if (any(d_screening$presence == "<unrecognised>", na.rm = TRUE)) {
    stop("EL_Incl_PhysicallyPresent has a value other than Yes, No or blank. ", call. = FALSE)
  }
  if (any(is.na(d_screening$presence) & !is.na(d_screening$outcome))) {
    stop("A row has a completed outcome but no physical-presence value. ", call. = FALSE)
  }
  
  message("screening removals: input = ", n_screened_input)
  message("screening removals: number removed by eligibility code = ", n_removed_listed,
          " (of which phys presence No = ", n_removed_not_present, ")")
  message("screening removals: number kept = ", nrow(d_screening), 
          " (unlisted physical presence No kept = ", n_kept_not_present, ")")

  
  list(
    d_screening = d_screening[],
    removed_randomised_ids = removed_randomised_ids,
    n_screened_input = n_screened_input,
    n_removed_listed = n_removed_listed
  )
}






proc_withdrawals <- function(d_withdrawal, cfg){
  
  d_file_desc <- cfg$data$d_file_desc
  
  f_label <- d_file_desc[name == "withdraw", prefix]
  
  # again, unclear why a separate fields are created but retaining for now
  d_withdrawal[, `:=`(
    id         = clean_code(StudyPtID),
    total_key  = str_to_lower(clean_code(WD_Total_WithdrawAll)),
    reason_key = str_to_lower(clean_code(WD_Reason))
  )]
  # mj - semantics of this are unclear - suggest more meaningful var names
  # %in% (not ==) so that a blank total field gives FALSE, not NA
  d_withdrawal[, is_total := total_key %in% "yes"]
  # matched on the start of the dropdown text (the export carries the
  # option text without its bracketed explanation)
  d_withdrawal[, reason_class := fcase(
    str_starts(reason_key, "enrolled in error"), "invalid_enrolment",
    str_starts(reason_key, "invalid consent"),   "invalid_enrolment",
    str_starts(reason_key, "consent withdrawn"), "consent_withdrawal",
    str_starts(reason_key, "appeared eligible"), "found_ineligible_later",
    default = "other"
  )]
  
  d_withdrawal <- d_withdrawal[!is.na(id)]
  # one row per participant: the first wins
  d_withdrawal <- unique(d_withdrawal, by = "id")   
  
  n_blank_total <- sum(is.na(d_withdrawal$total_key))
  if (n_blank_total > 0) {
    message("Warning: ", n_blank_total,
            " withdrawal row(s) have a blank WD_Total_WithdrawAll; treated as not total.")
  }
  
  n_unmapped <- sum(d_withdrawal$is_total & d_withdrawal$reason_class == "other")
  if (n_unmapped > 0) {
    message("Warning: ", n_unmapped,
            " total withdrawal(s) have a reason that is not classified; not removed.")
  }
  
  invalid_enrolment_ids <- d_withdrawal$id[
    d_withdrawal$is_total & d_withdrawal$reason_class == "invalid_enrolment"]
  n_consent_total <- sum(
    d_withdrawal$is_total & d_withdrawal$reason_class == "consent_withdrawal")
  n_later_total   <- sum(
    d_withdrawal$is_total & d_withdrawal$reason_class == "found_ineligible_later")
  
  message("Withdrawals: total =  invalid_enrolment + consent_withdrawal + found_ineligible_later")
  message("Withdrawals: identified = ", sum(d_withdrawal$is_total), " from file ", f_label )
  message("Withdrawals: invalid enrol or consent = ", length(invalid_enrolment_ids))
  message("Withdrawals: consent withdrawn = ", n_consent_total)
  message("Withdrawals: found ineligible after enrolment = ", n_later_total)
  

  withdrawals_list <- list(
    d_withdrawal = d_withdrawal,
    
    n_blank_total = n_blank_total,
    n_unmapped = n_unmapped,
    
    invalid_enrolment_ids = invalid_enrolment_ids,
    n_invalid_e_or_c = length(invalid_enrolment_ids),
    n_consent_total = n_consent_total,
    n_later_total = n_later_total
    
  )
}


# Total withdrawals for enrolment in error or invalid consent, plus listed
# platform IDs, move from Randomised to Not eligible.  Rows remain in Screened. 
# Consent withdrawals and later-discovered ineligibility remain
# Randomised (protocol sections 8.10 and 9.3).
reclassify_invalid_enrolments <- function(d_screening, w_list, rm_list) {

  # 
  invalid_enrolment_ids <- union(w_list$invalid_enrolment_ids, rm_list$platform_ids)
  
  is_invalid_enrolment <- !is.na(d_screening$outcome) & d_screening$outcome == "Randomised" &
    clean_code(d_screening$StudyPtID) %in% invalid_enrolment_ids
  
  if (any(is_invalid_enrolment)) {
    d_screening[is_invalid_enrolment, outcome := "Not eligible"]
    message("Reclassified ", sum(is_invalid_enrolment),
            " invalid enrolment(s): Randomised -> Not eligible")
  }
  
  list(d_screening = d_screening[], invalid_enrolment_ids = invalid_enrolment_ids)
}



reclassify_registry_removals <- function(d_screening, rm_list) {
  
  if (length(rm_list$registry_ids) == 0) {
    return(d_screening[])
  }
  
  # are there any registry pts listed in the removals?
  registry_listed <- clean_code(d_screening$RegistryPtID) %in% rm_list$registry_ids
  
  is_registry_removal <- registry_listed &
    !is.na(d_screening$outcome) & d_screening$outcome == "Registry"
  
  if (any(registry_listed & !is_registry_removal)) {
    message("Warning: ", sum(registry_listed & !is_registry_removal),
            " registry code(s) on the removal list do not have a current Registry outcome; ",
            "not reclassified.")
  }
  if (any(is_registry_removal)) {
    d_screening[is_registry_removal, outcome := "Not eligible"]
    message("Reclassified ", sum(is_registry_removal),
            " registry removal(s): Registry -> Not eligible")
  }
  d_screening[]
}