

source(here::here("R/data", "config.R"))
source(here::here("R/data", "utils-io.R"))
source(here::here("R/data", "utils-clean.R"))
source(here::here("R/data", "step-screening.R"))
source(here::here("R/data", "step-removals.R"))
source(here::here("R/data", "step-reconcile.R"))
source(here::here("R/data", "step-not-enrolled.R"))
source(here::here("R/data", "step-pt-listing.R"))
source(here::here("R/data", "step-domains.R"))
source(here::here("R/data", "step-summary.R"))

# `consort` is the single object that accumulates every result.
consort <- list()



# Approximately equiv to Steps 1-3 - this locate exports, load and standardise screening data

# Raw data, simply load and validate presence of some fields (utils-io.R)
l_extract <- read_extract(cfg)

# Preliminary processing of the raw data (step-screening.R)
# remove test rows, clean field text, check na, duplicates, recode cols
consort$screening <- proc_screening(l_extract$elig_plt, cfg)
# Keep the original participant IDs and randomised total for later checks.
consort$all_study_ids <- unique(drop_na_values(clean_code(consort$screening$StudyPtID)))
consort$n_randomised_raw <- sum(consort$screening$outcome == "Randomised", na.rm = TRUE)

# Approximately equiv to Steps 4-6: removal list, screening removals, reclassifications (step-removals.R)
# Process the removals list
rm_list <- proc_removals(l_extract$removals, cfg)
# Apply the removals to the screening data (i.e. drop rows)
removed <- apply_screen_removals(consort$screening, rm_list)
# Reassign screening after removals
# Note to self - alt is to retain each screening data set in the progression
consort$screening <- removed$d_screening

# Withdrawals after platform entry
w_list <- proc_withdrawals(l_extract$withdraw, cfg)
# Based on the invalid enrolments subset identified above and the 
# platform ids listed in the removal list, subset the screening data
# Redesignates outcome value to "Not eligible".
invalid <- reclassify_invalid_enrolments(consort$screening, w_list, rm_list)
# reassign screened data
consort$screening <- invalid$d_screening
# reassign screened data
consort$screening <- reclassify_registry_removals(consort$screening, rm_list)



# Step 7: reconcile top-level counts (D = Y + N + A + P)
consort$counts <- reconcile_top_level_counts(consort$screening)

# StudyPtID links randomised participants to the list and domain exports.
consort$platform_ids <- consort$screening[outcome == "Randomised" & !is.na(StudyPtID), unique(StudyPtID)]



# Step 8: count reasons not enrolled
consort$not_enrolled <- count_not_enrolled_reasons(
  consort$screening, consort$counts$n_registry, consort$counts$n_not_eligible, cfg
)



# Step 9: check the participant listings
consort$pt_listing <- proc_pt_listing(l_extract$pt_listing, consort$platform_ids, cfg)

# Removed screening rows and invalid enrolments are also excluded from domains.
consort$removed_ids <- union(
  removed$removed_randomised_ids,
  intersect(invalid$invalid_enrolment_ids, consort$all_study_ids)
)
if (length(intersect(clean_code(consort$platform_ids), consort$removed_ids)) > 0) {
  stop("A participant is both randomised and on the removed set. ",
       "No diagram was produced.", call. = FALSE)
}
message("Removed participants for the domain counts: ", length(consort$removed_ids))



# Step 10: domain allocation reveals
consort$domain_results <- count_all_domains(consort, l_extract, cfg)
pending <- pending_domains_from_list(consort)

consort$domain_results <- pending$domain_results
consort$n_duration_pending <- pending$n_duration_pending



# Step 11: domain and Day 365 summaries
consort$domain_summary <- summarise_domain_reveals(
  consort$domain_results, consort$counts$n_randomised)

consort$n_day365 <- day365_count(consort)


