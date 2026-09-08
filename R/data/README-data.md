# Data Extract

Entry point is `data-main.R`

Purpose is to extract source data from spiral extract to produce structures for consort dig and setup data for summaries.

## Config `config.R`

Assumes extracts contained in folder of form: `data/xxxxx/YYYYMMDDHHMM` where xxxxx could be dummy data, raw extract etc.

Currently expects files
+ Participant Eligibility export.csv
+ Participant list export.csv
+ Domain eligibility surgical export.csv
+ Domain eligibility antibiotic choice export.csv
+ Domain eligibility antibiotic duration Rev 1 export.csv
+ Domain eligibility antibiotic duration Rev 2 export.csv
+ Withdrawal - Trial export.csv
+ eligibility-codes-for-removal.csv

and will hard fail if not found or if any duplicates present.

Main counts:
  Screened (D)      eligibility rows after test-site and listed removals
  Randomised (Y)    Randomised outcome after invalid enrolments are reclassified
  Registry only (N) Registry outcome after registry removals
  Not eligible (A)  Not Eligible, Excluded or Not Included, plus reclassifications
  Pending (P)       blank outcome or Randomised Pending

D = Y + N + A + P. Domain allocations are counted separately because a
participant can be in more than one domain. Day 365 is based on Day >= 365
in the participant list.

## Removals  - TBC

Functionality identifies units that should not contribute to the platform population.

Provide example contents of the `eligibility-codes-for-removal.csv` file.
Is this file provided by spiral or is it produced by research team based on protocol rules?

MJ interpretation (might be wrong) of fields:

`eligibility_codes` - map to eligibility assessment level (i.e. each unique assessment) keys to be removed?


## Withdrawals  - TBC

Functionality identifies units that should not contribute to the platform population.

MJ interpretation (might be wrong) of fields:

`WD_Total_WithdrawAll` withdrawn from all domains?


