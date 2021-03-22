# automation_portfolio
This is a porforlio of my code from 3 projects.

## Automation of SAS to FST format
+ Translates our crash data into .fst file by running a cmd script.
+ This script runs both a SAS script and an R script with opening any IDE (just click the button!)

## Analysis of OWI ratios
This project involved the compilation of 5 data sets to calculate OWI ratios broken down by age group, race and sex. In order to combine these datasets, I had to rename columns and recode variables so datasets can be joined. I wrote functions that summarize each of the datasets and combines them into a table with the parameter allowing for any combo of age, sex, or race.
+ Datasets include: NHTS, Census, arrest data, DMV driver, and persons in a crash

## Import JSON from an API
We wanted to find the average sentence length of 3+ OWI offenders. I used our court case data to compile this data into one dataframe for analysis. This script is the first part of my analysis that grabs, compiles and flattens JSON into a single dataframe given a data range and court code.