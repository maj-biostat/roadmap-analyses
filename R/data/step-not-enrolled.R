count_not_enrolled_reasons <- function(
    d_screening, n_registry, n_not_eligible, cfg) {
  
  not_enrolled <- d_screening[outcome %in% c("Registry", "Not eligible")]
  n_not_enrolled <- nrow(not_enrolled)
  
  # Allow the Yes/No, logical and 1/0 forms seen in exports.
  count_flag <- function(column) {
    if (!column %in% names(not_enrolled)) {
      message("Warning - column ", column, " not found returning zero.")
      return(0L)
    }
    values <- as.character(not_enrolled[[column]])
    sum(values %in% c("Yes", "TRUE", "1"))
  }
  exclusion_flags <- copy(cfg$data$d_excl_flags_spec)
  # vapply is jhust the same as sapply but with a pre-spec rtn value
  exclusion_flags[, n := vapply(column, count_flag, integer(1))]
  
  n_not_present <- sum(not_enrolled$presence == "No", na.rm = TRUE)
  if (n_not_present > 0) {
    message("Not-enrolled rows with physical presence No (kept as screen failures): ",
            n_not_present)
  }
  
  n_consent_declined <- 0L
  n_not_approached   <- 0L
  if ("EL_Con_Platform_Obtained" %in% names(not_enrolled)) {
    consent_value <- as.character(not_enrolled$EL_Con_Platform_Obtained)
    n_consent_declined <- sum(consent_value == "No - patient refused consent", na.rm = TRUE)
    n_not_approached   <- sum(consent_value == "No - not approached for consent", na.rm = TRUE)
  }
  
  exclusion_reasons <- rbindlist(list(
    exclusion_flags[, .(reason, n)],
    data.table(reason = "Core inclusion criterion not met: not physically present", n = n_not_present),
    data.table(reason = "Eligible, consent declined", n = n_consent_declined),
    data.table(reason = "Eligible, not approached", n = n_not_approached)
  ))
  
  # Format each reason against the full not-enrolled population.
  if (n_not_enrolled > 0) {
    reason_lines <- paste0(exclusion_reasons$reason, " (", exclusion_reasons$n, ", ",
                           sprintf("%.1f", 100 * exclusion_reasons$n / n_not_enrolled), "%)")
  } else {
    reason_lines <- paste0(exclusion_reasons$reason, " (", exclusion_reasons$n, ")")
  }
  reason_lines <- append(reason_lines, "", after = nrow(exclusion_flags))
  
  # just for figure (I think...)
  not_enrolled_text <- paste(
    c(paste0("Not Enrolled in Platform (n = ", n_registry + n_not_eligible, ")"),
      "",
      "Exclusion criteria:",
      reason_lines),
    collapse = "\n"
  )
  
  list(exclusion_reasons = exclusion_reasons, not_enrolled_text = not_enrolled_text)
}

