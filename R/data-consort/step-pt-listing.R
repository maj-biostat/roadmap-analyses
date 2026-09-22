# Step 9: read and check the participant list -----------------------------

proc_pt_listing <- function(d_pt_listing, platform_ids, cfg) {
  
  
  # the list has no site column, so the ID prefix alone decides the test site
  list_test_rows <- is_test_site_row(d_pt_listing$StudyPtID, NA_character_, cfg$data$test_site_codes)
  d_pt_listing <- d_pt_listing[!list_test_rows]
  message("Participant list rows: ", nrow(d_pt_listing),
          " (test-site rows removed: ", sum(list_test_rows), ")")
  
  # One row per participant. A duplicate row would count that participant's
  # arm twice, so a duplicate stops the script (always)
  n_duplicate_rows <- sum(duplicated(d_pt_listing$StudyPtID) & !is.na(d_pt_listing$StudyPtID))
  if (n_duplicate_rows > 0) {
    problem <- paste0("The participant list has ", n_duplicate_rows,
                      " duplicate StudyPtID row(s).")
    stop(problem, call. = FALSE)
  }
  
  list_ids <- str_to_upper(clean_code(d_pt_listing$StudyPtID))
  missing_from_list <- setdiff(str_to_upper(clean_code(platform_ids)), list_ids)
  if (length(missing_from_list) > 0) {
    problem <- paste0(length(missing_from_list), " randomised participant(s) from the ",
                      "eligibility export are not in the participant list. Check that ",
                      "both exports are from the same data cut.")
    
    # todo - update to make this always fail.
    # if (strict) stop(problem, " No diagram was produced.", call. = FALSE)
    
    message("Warning: ", problem, " Their arm and Day are unknown.")
    message("Warning:  mjtodo - update to always fail on missing IDs....")
  }
  
  d_pt_listing[]
}



