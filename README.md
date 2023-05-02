# Chronic pain & group psychotherapy

Doctoral thesis by Julia Sonnleitner
Code by Hannah Metzler (metzler@csh.ac.at, www.hannahmetzler.eu)

All R scripts and data are online in a private github repository. If you want to continue analysing these data, I can discuss your project with Julia Sonnleitner, and then possibly give you access to that repository. Please contact me at metzler@csh.ac.at. 

## Data sets: 

* data_pain_t0t2t3.csv: both treatment groups at T0, T2, T3
* data_pain_t0t1.csv: treatment group only at T0 and T1
- icd10.csv: detailed diagnosis for each axis
- icd10_largecategories.csv: patients per intermediate category of disorder
- therapy_goals_and_evaluation.csv: How patients perceived therapy helped them achieve different goals, and how they evaluated the group therapy. Data only from Treatment group at T3.

Values: 
* Missing values are coded as NA
* In long data formats (where T0, T2, T3 are in the same column with repeated rows per subject), values that were not measured in later waves are coded as 999 when they occur the second or third time (e.g. demographics). This allows excluding them when describing the sample, without neglecting actually missing values of participants who dropped out. 
* For more details on the variables and data, check the script analysis_runner.Rmd (open in R, or in a text editor)


## Folder structure

Before running the R scripts, create the following folders in your folder for this project, and put scripts and data in the appropriate folder. Figures and output is where figures and tables etc will be saved. 

* data
* figures
* output
* scripts

## Code: 
* Das Skript "analysis_runner.Rmd" führt durch die Analysen, ruft ein Skript nach dem anderen auf (von 1 bis 7), beschreibt alle Variablen, und erstellt einen PDF oder Word report (wenn es in R verwendet wird (mittels knit button)). 
* Skript 01 formatiert die Daten (data_pain_t0t2t3.csv) und berechnet Differenzen zwischen Zeitpunkten. Alle folgenden Skripten zu den 3 Zeitpunkten verwenden die formatierten Daten aus Skript 1. 
* Skript 02 berechnet Descriptive statistics pro Zeitpunkt mit den Daten aus Skript 01
* Skript 03 erstellt alle figures zu den Daten aus Skript 01
* Skript 4 analysiert die Daten von T1, der nur bei der Treatment Gruppe erhoben wurde. 
* Skript 5 analysiert die Daten in beiden Gruppen bei T0, T2, T3. Korrelationen, t-tests, ANOVAs und ihre Voraussetzungen. Datenset aus Skript 01. 
* Skript 6 macht wilcoxon tests zu den 3 Zeitpunkten, zu ordinalen Variablen, weil bei ihnen keine ANOVA möglich ist.
* Skript 7 analysiert den ICD 10. Datensets dafür: 
  - icd10.csv: detailed diagnosis for each axis
  - icd10_largecategories.csv (patients per intermediate category of disorder)
* therapy_goals_evaluation.Rmd creates a second report that describes the Therapy goals and evaluation data in the treatment group (dataset: therapy_goals_and_evaluation.csv)
