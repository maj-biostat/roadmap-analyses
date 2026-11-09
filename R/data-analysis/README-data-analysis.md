# Data - Analysis

Data processing for analysis.

The file extract produces a set of files per the following table, which are then renamed so that they can be easily referenced within the data processing scripts.
Each of the datasets are stored within a list indexed by their expected filename (minus the .csv).

The files are stored in a folder under the data directory which is named according to the extract date, e.g. `202609021608`

| Original file | Expected name | Description |
|:-----------------------------------|:-----------------------------------|:-----------------------------------|
| Baseline export_2026-09-02_1607.csv | baseline.csv |    |
| D100 export_2026-09-02_1607.csv | d100.csv |    |
| D365 export_2026-09-02_1607.csv | d365.csv |    |
| Domain eligibility antibiotic choice export_2026-09-02_1607.csv | AC-elig.csv |   |
| Domain eligibility antibiotic duration Rev 1 export_2026-09-02_1607.csv | DA-elig.csv |   |
| Domain eligibility antibiotic duration Rev 2 export_2026-09-02_1607.csv | DB-elig.csv |   |
| Domain eligibility surgical export_2026-09-02_1607.csv | SD-elig.csv |   |
| Participant Eligibility export_2026-09-02_1607.csv | screening.csv | Screening data? |
| Participant list export_2026-09-02_1607.csv | pt-list.csv |   |
| Protocol Deviation export_2026-09-02_1607.csv | prot-dev.csv |   |
| Registry Baseline export_2026-09-02_1607.csv | reg-baseline.csv |   |
| Registry D365 export_2026-09-02_1607.csv | reg-d365.csv |   |
| Registry list export_2026-09-02_1607.csv | reg-pt-list.csv |   |
| Withdrawal - Registry export_2026-09-02_1607.csv | reg-wd.csv |   |
| Withdrawal - Trial export_2026-09-02_1607.csv | wd.csv |   |
| - | removals.csv | Used for overriding data in extract 


