


# Randomised in a domain export means the allocation has been revealed for
# that domain. Domain populations overlap (a participant can be in more than
# one domain) and are not added together.
count_domain <- function(
    d_screening,
    # data for this domain from the extract
    d_domain, 
    dom_spec, 
    # probably shouldn't include dependencies on this but will do as first pass.
    consort,
    cfg  
    ) {
  
  
  result <- list(
    revealed_ids = character(0), 
    arms = character(0),
    n_missing_arm = 0L, 
    n_not_included = 0L, 
    n_excluded = 0L,
    n_pending = 0L, 
    n_declined = 0L, 
    n_not_sought = 0L, 
    n_eliminated = 0L
  )

  # Domain exports have no site column, so use the participant ID prefix.
  domain_test_rows <- is_test_site_row(d_domain$StudyPtID, NA_character_, cfg$data$test_site_codes)
  if (any(domain_test_rows)) {
    message(dom_spec$label, ": ", sum(domain_test_rows), " test-site row(s) removed.")
    d_domain <- d_domain[!domain_test_rows]
  }
  d_domain[, outcome := clean_code(EL_Outcome)]  
  
  
  unexpected <- setdiff(unique(drop_na_values(d_domain$outcome)),
                        c("Randomised", "Not Included", "Excluded", "Eliminated"))
  if (length(unexpected) > 0) {
    message("Warning: ", dom_spec$label, " has unrecognised EL_Outcome value(s): ",
            paste(unexpected, collapse = ", "))
  }
  
  # Separate valid, removed and unmatched revealed IDs.
  revealed_all <- unique(d_domain[outcome == "Randomised" & !is.na(StudyPtID), StudyPtID])
  revealed_removed <- intersect(revealed_all, consort$removed_ids)
  revealed_unmatched <- setdiff(revealed_all, union(consort$platform_ids, consort$removed_ids))
  
  if (length(revealed_removed) > 0) {
    message(dom_spec$label, ": ", length(revealed_removed),
            " revealed participant(s) are on the removed set and are left out.")
  }
  if (length(revealed_unmatched) > 0) {
    problem <- paste0(dom_spec$label, ": ", length(revealed_unmatched),
                      " revealed StudyPtID(s) are not randomised participants. ",
                      "Check that the domain and eligibility exports are from the same data cut.")
    stop(problem, call. = FALSE)
  }
  
  d_domain <- d_domain[StudyPtID %in% consort$platform_ids & !is.na(StudyPtID)]
  
  # Check for participants with more than one outcome in the same domain.
  conflicting <- unique(d_domain[, .(StudyPtID, outcome_or_pending = fcoalesce(outcome, "<pending>"))])
  conflicting <- conflicting[, .N, by = StudyPtID][N > 1]
  if (nrow(conflicting) > 0) {
    problem <- paste0(dom_spec$label, ": ", nrow(conflicting),
                      " StudyPtID(s) have conflicting EL_Outcome values.")
    stop(problem, call. = FALSE)
  }
  
  # Eliminated means the other duration revision applies, so this revision
  # was not assessed.
  eliminated <- d_domain[outcome == "Eliminated"]
  result$n_eliminated <- uniqueN(eliminated$StudyPtID)
  d_domain <- d_domain[is.na(outcome) | outcome != "Eliminated"]
  if (result$n_eliminated > 0) {
    message(dom_spec$label, ": ", result$n_eliminated,
            " participant(s) marked Eliminated (the other duration revision applies); ",
            "counted as never assessed for this domain.")
  }
  
  revealed <- d_domain[outcome == "Randomised"]
  not_included <- d_domain[outcome == "Not Included"]
  excluded <- d_domain[outcome == "Excluded"]
  export_pending <- d_domain[is.na(outcome)]
  result$revealed_ids <- unique(revealed$StudyPtID)
  result$n_not_included <- uniqueN(not_included$StudyPtID)
  result$n_excluded <- uniqueN(excluded$StudyPtID)
  n_export_pending <- uniqueN(export_pending$StudyPtID)
  
  # Classify randomised participants with no domain row by consent status.
  # IDs not present in this domain
  absent_ids <- setdiff(consort$platform_ids, unique(d_domain$StudyPtID))
  n_consented_absent <- 0L
  n_na_consent <- 0L
  if (length(absent_ids) > 0 && dom_spec$consent_column %in% names(d_screening)) {
    absent_rows <- unique(d_screening[StudyPtID %in% absent_ids], by = "StudyPtID")
    absent_consent <- as.character(absent_rows[[dom_spec$consent_column]])
    result$n_declined   <- sum(absent_consent == "No", na.rm = TRUE)
    result$n_not_sought <- sum(str_starts(absent_consent, "Not sought"), na.rm = TRUE)
    n_na_consent <- sum(is.na(absent_consent))
    if (dom_spec$consent_pending) {
      n_consented_absent <- sum(absent_consent == "Yes", na.rm = TRUE)
    }
  } else if (length(absent_ids) > 0) {
    n_na_consent <- length(absent_ids)
  }
  result$n_pending <- n_export_pending + n_consented_absent
  
  message("  rows for randomised participants: ", uniqueN(d_domain$StudyPtID),
          "; revealed = ", length(result$revealed_ids),
          ", not included = ", result$n_not_included,
          ", excluded = ", result$n_excluded,
          ", pending in export = ", n_export_pending)
  message("  no row in export: ", length(absent_ids),
          " (declined = ", result$n_declined,
          ", not sought = ", result$n_not_sought,
          ", consented without a row = ", n_consented_absent,
          ", no consent value (NA) = ", n_na_consent, ")")
  
  if (length(result$revealed_ids) == 0) {
    return(result)
  }
  if (is.null(consort$pt_listing)) {
    message("Warning: ", dom_spec$label, ": no participant list, so no arm split.")
    result$arms <- rep(NA_character_, length(result$revealed_ids))
    return(result)
  }
  
  check_columns(dom_spec$file, dom_spec$arm_col, names(consort$pt_listing))

  list_rows <- match(result$revealed_ids, consort$pt_listing$StudyPtID)
  codes <- clean_code(consort$pt_listing[[dom_spec$arm_col]])[list_rows]
  unknown_codes <- setdiff(unique(drop_na_values(codes)), names(cfg$data$arm_labels))
  if (length(unknown_codes) > 0) {
    stop(dom_spec$label, ": unknown arm code(s) ", paste(unknown_codes, collapse = ", "),
         ". Add them to arm_labels in config.R.", call. = FALSE)
  }
  # NA when blank
  result$arms <- unname(cfg$data$arm_labels[match(codes, names(cfg$data$arm_labels))])   
  result$n_missing_arm <- sum(is.na(result$arms))
  if (result$n_missing_arm > 0) {
    message("Warning: ", dom_spec$label, ": ", result$n_missing_arm,
            " revealed participant(s) have no arm code.")
  }
  result
}

# Run count_domain() for every row of domain_spec (in config), returning a named list
# of results keyed by domain code (SD, AC, DA, DB).
count_all_domains <- function(consort, l_extract, cfg) {
  
  domain_results <- list()
  domain_spec <- copy(cfg$data$l_domain_spec)
  spec_names <- names(domain_spec)
  d_file_desc <- copy(cfg$data$d_file_desc)
  d_screening = copy(consort$screening)
  
  i = 1
  for (i in seq_along(domain_spec)) {
    
    dom_spec <- domain_spec[[i]]
    d_domain = copy(l_extract[[dom_spec$file]])
    
    domain_results[[ spec_names[i] ]] <- count_domain(
      d_screening,
      # data for this domain from the extract
      d_domain, 
      dom_spec, 
      consort,
      cfg
    )
  }
  
  domain_results
}




# PT_DomsPendingNow gives the exact pending domains and avoids assigning the
# shared duration consent field to Part A or Part B.

pending_domains_from_list <- function(consort) {
  
  domain_results = copy(consort$domain_results)
  
  if (is.null(consort$pt_listing) || !"PT_DomsPendingNow" %in% names(consort$pt_listing)) {
    message("PT_DomsPendingNow not present; the duration pending box will say unavailable.")
    return(list(domain_results = domain_results, n_duration_pending = NA_integer_))
  }
  
  pending_long <- consort$pt_listing[, .(
    StudyPtID   = clean_code(StudyPtID),
    domain_code = str_to_upper(clean_code(PT_DomsPendingNow))
  )]
  # splits up the domain codes downwards
  pending_long <- pending_long[
    , .(domain_code = unlist(strsplit(domain_code, "\\|"))), by = StudyPtID]
  
  pending_long[, domain_code := clean_code(domain_code)]
  pending_long <- pending_long[
    !is.na(StudyPtID) & !is.na(domain_code) &
      StudyPtID %in% consort$platform_ids & 
      !StudyPtID %in% consort$removed_ids]
  pending_long <- unique(pending_long, by = c("StudyPtID", "domain_code"))
  
  # Do not count an allocation as both revealed and pending in the same domain.
  pending_ids <- list()
  
  for (code in names(domain_results)) {
    
    ids <- pending_long$StudyPtID[pending_long$domain_code == code]
    overlap <- intersect(ids, domain_results[[code]]$revealed_ids)
    if (length(overlap) > 0) {
      problem <- paste0(code, ": ", length(overlap),
                        " StudyPtID(s) are both revealed and pending in PT_DomsPendingNow.")
      stop(problem, call. = FALSE)
    }
    pending_ids[[code]] <- setdiff(ids, domain_results[[code]]$revealed_ids)
    domain_results[[code]]$n_pending <- length(pending_ids[[code]])
  }
  n_duration_pending <- length(union(pending_ids$DA, pending_ids$DB))
  
  message("Pending from PT_DomsPendingNow: SD = ", domain_results$SD$n_pending,
          ", AC = ", domain_results$AC$n_pending,
          ", DA = ", domain_results$DA$n_pending,
          ", DB = ", domain_results$DB$n_pending,
          "; duration A and/or B = ", n_duration_pending)
  
  list(domain_results = domain_results, n_duration_pending = n_duration_pending)
}
