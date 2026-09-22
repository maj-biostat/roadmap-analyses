

reconcile_top_level_counts <- function(d_screening) {
  
  n_screened     <- nrow(d_screening)
  n_randomised   <- sum(d_screening$outcome == "Randomised", na.rm = TRUE)
  n_registry     <- sum(d_screening$outcome == "Registry", na.rm = TRUE)
  n_not_eligible <- sum(d_screening$outcome == "Not eligible", na.rm = TRUE)
  n_pending      <- sum(is.na(d_screening$outcome))
  
  message("Reconciliation: Randomised ", n_randomised)
  message("Reconciliation: Registry ", n_registry)
  message("Reconciliation: Not elig ", n_not_eligible)
  message("Reconciliation: Pending ", n_pending)
  message("Reconciliation: Screened ", n_screened)
  
  if (n_randomised + n_registry + n_not_eligible + n_pending != n_screened) {
    stop("Reconciliation failed: ", n_randomised, " + ", n_registry, " + ", n_not_eligible,
         " + ", n_pending, " is not ", n_screened, call. = FALSE)
  }
  
  message("Reconciliation: (OK) ", n_randomised, " + ", n_registry, " + ", n_not_eligible,
          " + ", n_pending, " = ", n_screened)
  message("Registry total (randomised + registry): ", n_randomised + n_registry)
  
  list(
    n_screened     = n_screened,
    n_randomised   = n_randomised,
    n_registry     = n_registry,
    n_not_eligible = n_not_eligible,
    n_pending      = n_pending
  )
}
