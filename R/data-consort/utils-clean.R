require(stringr)

#' Trim/squish whitespace and turn "" into NA. The baseline cleaner every
#' code/ID column is run through before comparison.
clean_code <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

#' clean_code(), plus treats common typed-in missing values as blank.
#' Only used for the removal list, where codes are hand-maintained.
clean_removal_code <- function(x) {
  x <- clean_code(x)
  x[!is.na(x) & str_to_lower(x) %in% c("na", "n/a", "null")] <- NA_character_
  x
}

drop_na_values <- function(x){
  x[!is.na(x)]
} 

#' Participant ID prefix (before the first "-"), used to identify test-site
#' records when there is no site-code column to check.
id_prefix <- function(participant_id) {
  prefix <- stringr::str_to_upper(clean_code(participant_id))
  base::sub("-.*$", "", prefix)
}

is_test_site_row <- function(participant_id, site_code, test_codes) {
  test_codes <- stringr::str_to_upper(clean_code(test_codes))
  site   <- stringr::str_to_upper(clean_code(site_code))
  prefix <- id_prefix(participant_id)
  (!is.na(site) & site %in% test_codes) | (!is.na(prefix) & prefix %in% test_codes)
}

format_n_pct <- function(n, denominator) {
  if (is.na(denominator) || denominator <= 0) {
    return(paste0("n = ", n))
  }
  paste0("n = ", n, " (", round(100 * n / denominator, 1), "%)")
}

#' Lighten a colour by blending it toward white (used by the diagram code).
# mix_with_white <- function(colour, amount) {
#   channels <- col2rgb(colour)
#   mixed <- round(channels + (255 - channels) * amount)
#   rgb(mixed[1, ], mixed[2, ], mixed[3, ], maxColorValue = 255)
# }
