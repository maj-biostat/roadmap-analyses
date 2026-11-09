# Reporting

The analysis report is generated based on the outputs from the data-analysis.R script which uses a specified extract to create a set of derived outputs that can be easily summarised in the report.

The header of the report file contains the following fragment:

```
params:
  # extract folder under data/derived/ (and data/dummy/)
  extract_dttm: "202609021608"
  # derived file name; leave empty to use the most recent dat-*.qs2 in the folder
  derived_file: ""
```

and you have to update the `extract_dttm` field in order to pick out the applicable derived dataset.



## Protocol deviations

There was no extract associated with protocol deviations.

## Safety 

Per the protocol:

*Patients eligible for the ROADMAP trial are at significant risk of morbidity or mortality from their PJI and its complications, regardless of their participation in the trial. Many such events would meet the conventional definitions of a serious adverse event (SAE).*

*Only registered drugs and surgical therapies with established safety profiles will be used in ROADMAP at trial launch.*

*Selected adverse events will be recorded as part of core primary, core secondary, or domain-specific secondary outcomes. These include but are not limited to all-cause mortality, acute kidney injury, acute liver injury, Clostridioides difficile-associated diarrhoea, cessation of an antibiotic due to adverse effects, unplanned reoperation, and fracture or dislocation.*

The following are yet to be summarised.

+ all-cause mortality, 
+ acute kidney injury, 
+ acute liver injury, 
+ Clostridioides difficile-associated diarrhoea, 
+ cessation of an antibiotic due to adverse effects, 
+ unplanned reoperation, 
+ fracture or dislocation

