

source(here::here("R/data", "config.R"))
source(here::here("R/data", "utils-io.R"))
# source(here::here("R/data", "utils-clean.R"))
# source(here::here("R/data", "step-screening.R"))
# source(here::here("R/data", "step-removals.R"))
# source(here::here("R/data", "step-reconcile.R"))
# source(here::here("R/data", "step-not-enrolled.R"))
# source(here::here("R/data", "step-participant-list.R"))
# source(here::here("R/data", "step-domains.R"))
# source(here::here("R/data", "step-summary.R"))

# `consort` is the single object that accumulates every result.
consort <- list()

# Steps 1-3 - this locate exports, load and standardise screening data
l_extract <- read_extract(cfg)


# Raw data imported from spiral export
l_raw <- read_raw_data()
kableExtra::kbl(
  l_raw$d_f_map, format = "simple", 
  caption = paste0("Spiral data, extract date ", l_raw$extract_date))


consort$screening <- load_screening(files$eligibility_file, cfg$test_site_codes, cfg$strict_id_checks)






