# Data - Analysis

The analysis reports are based on pre-processing extracts from spiral which dump data in a usable form into the derived directory. 

The analysis file (`data-analysis.R`) should be configured based on the current extract and `main` invoked to create the necessary data sets.

For convenience, the extract files are renamed per the following table. 
These are then stored within the `l_raw` list object under the new filenames.

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

For now, the processing is fairly rudimentary.

