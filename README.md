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

The script "analysis_runner.Rmd" conducts the analyses, calls one script after another (from 1 to 7), describes all variables, and creates a PDF or Word report (if used in R (using the knit button)).

* Script 01 formats the data (data_pain_t0t2t3.csv) and calculates differences between time points. All following scripts for the 3 time points use the formatted data from Script 01.
* Script 02 calculates descriptive statistics per time point using the data from Script 01.
* Script 03 creates all figures using the data from Script 01.
* Script 04 analyzes the data from T1, which was only collected in the treatment group.
* Script 05 analyzes the data in both groups at T0, T2, T3. Correlations, t-tests, ANOVAs, and their assumptions. Dataset from Script 01.
* Script 06 performs Wilcoxon tests on the 3 time points for ordinal variables, as ANOVA is not possible for them.
* Script 07 analyzes the ICD 10. Datasets for this:
  - icd10.csv: detailed diagnosis for each axis
  - icd10_largecategories.csv (patients per intermediate category of disorder)
* therapy_goals_evaluation.Rmd creates a second report that describes the Therapy goals and evaluation data in the treatment group (dataset: therapy_goals_and_evaluation.csv)
