# Data - Analysis

The interim analysis reports pre-process data extracts from spiral to put it in a more usable format and then store it in the derived directory.

The analysis file (`data-analysis.R`) should be configured based on the current extract and `main` invoked to create the necessary data sets.

One way to run the pre-processing (after pointing the script at the correct extract), is invoke `just data` from command line to run or interactively via invoking `main` function after sourcing script.

Note that for convenience, the extract files are renamed per the following table and the script is tied to this naming scheme.
The raw extract is stored within the `l_raw` list object under the new filenames and then used throughout to produce the data objects that are ultimately referenced in the reports.

The files are stored in a folder under the data directory which is named according to the extract date, e.g. `202609021608`

| Original file | Expected name | Description |
|:-------------|:----|:------------------|
| Baseline export_2026-09-02_1607.csv | baseline.csv |    |
| D100 export_2026-09-02_1607.csv | d100.csv |    |
| D365 export_2026-09-02_1607.csv | d365.csv |    |
| Domain eligibility antibiotic choice export_2026-09-02_1607.csv | elig-ac.csv |   |
| Domain eligibility antibiotic duration Rev 1 export_2026-09-02_1607.csv | elig-da.csv |   |
| Domain eligibility antibiotic duration Rev 2 export_2026-09-02_1607.csv | elig-db.csv |   |
| Domain eligibility surgical export_2026-09-02_1607.csv | elig-sd.csv |   |
| Participant Eligibility export_2026-09-02_1607.csv | screening.csv | Screening data? |
| Participant list export_2026-09-02_1607.csv | pt-list.csv |   |
| Protocol Deviation export_2026-09-02_1607.csv | prot-dev.csv |   |
| Registry Baseline export_2026-09-02_1607.csv | reg-baseline.csv |   |
| Registry D365 export_2026-09-02_1607.csv | reg-d365.csv |   |
| Registry list export_2026-09-02_1607.csv | reg-pt-list.csv |   |
| Withdrawal - Registry export_2026-09-02_1607.csv | reg-wd.csv |   |
| Withdrawal - Trial export_2026-09-02_1607.csv | wd.csv |   |
| - | removals.csv | Used for overriding data in extract 



