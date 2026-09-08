


summarise_domain_reveals <- function(
    domain_results, n_randomised
    ) {
  
  n_revealed_sd <- length(domain_results$SD$revealed_ids)
  n_revealed_ac <- length(domain_results$AC$revealed_ids)
  n_revealed_da <- length(domain_results$DA$revealed_ids)
  n_revealed_db <- length(domain_results$DB$revealed_ids)
  
  revealed_any_ids <- unique(c(
    domain_results$SD$revealed_ids, domain_results$AC$revealed_ids,
    domain_results$DA$revealed_ids, domain_results$DB$revealed_ids
  ))
  n_revealed_any <- length(revealed_any_ids)
  if (n_revealed_any > n_randomised) {
    stop("More participants are revealed in a domain than are randomised. ",
         "Check that all exports are from the same data cut. No diagram was produced.",
         call. = FALSE)
  }
  n_not_revealed <- n_randomised - n_revealed_any
  message("Domain counts: SD = ", n_revealed_sd, ", AC = ", n_revealed_ac,
          ", DA = ", n_revealed_da, ", DB = ", n_revealed_db)
  message("Revealed in at least one domain: ", n_revealed_any, " of ", n_randomised,
          " randomised; not yet revealed: ", n_not_revealed)
  
  list(
    n_revealed_sd = n_revealed_sd, n_revealed_ac = n_revealed_ac,
    n_revealed_da = n_revealed_da, n_revealed_db = n_revealed_db,
    n_revealed_any = n_revealed_any, n_not_revealed = n_not_revealed
  )
}

# Day 365 includes randomised participants with Day >= 365 in the
# participant list.
day365_count <- function(
    d_pt_listing,
    platform_ids,
    n_randomised
    ) {
  
  if (is.null(d_pt_listing) || !"Day" %in% names(d_pt_listing)) {
    message("Warning: no participant list or no Day column; Day 365 count unavailable.")
    return(NA_integer_)
  }
  randomised_rows <- d_pt_listing[StudyPtID %in% platform_ids]
  randomised_rows[, day_number := suppressWarnings(as.numeric(Day))]
  n_day_not_numeric <- sum(is.na(randomised_rows$day_number) & !is.na(randomised_rows$Day))
  if (n_day_not_numeric > 0) {
    message("Warning: ", n_day_not_numeric, " Day value(s) are not numbers; treated as missing.")
  }
  if (nrow(randomised_rows) == 0) {
    problem <- "No participant-list StudyPtID matches a randomised participant."
    stop(problem, call. = FALSE)
  }
  n_day365 <- uniqueN(randomised_rows[day_number >= 365, StudyPtID])
  message("Reached Day 365: ", n_day365, " of ", n_randomised)
  n_day365
}
