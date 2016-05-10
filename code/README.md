# Code Readme

This is the README for my STAT 530 final project code at UIUC.

### data_steps.R

Reads in the GSE file and creates training and test files for the original analysis. Drops 3 observations for NA values and quite a few predictors not present in all samples. Then we normalize the data *together*, not according to the paper. We did it according to the paper initially to terrible results.

### EDA.R

Gets general information about the data from the original analysis, mostly to ensure our data looks the same as that used in the paper.

### reproduce.R

Attempted to reproduce the paper results and found a problem with the data such that we can only reproduce the results on the test set.

### init_analyses.R

Tried to implement Naive Bayes, LASSO, and Random Forest using the authors' train/test split and normalization. Terrible results, scrapped their data processing procedure and proceeded with `my_analyses.R`

### my_analyses.R

Reads in the raw data and performs our own train/test splitting and no normalization. Then implements LASSO, Random Forest, and GBM. Also produces some plots and citations.

### other_data.R [Currently not Implemented]

Plans to read in and perform predictions on the other datasets mentioned in the paper.
