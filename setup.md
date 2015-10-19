1. Install the lastest version of R from https://www.r-project.org/
2. Install RStudio Version 0.99.486 or higher from https://www.rstudio.com/ (supports autocomplete).
3. Open RStudio and install the libraries listed below using install.packages():

* ggplot2: Great for visualizations.
* dplyr: Perform operations on dataframes with clean and easily readable code.
* glmnet: A library for regularized linear regression (prevents excessive overfitting).
* randomForest: A basic machine learning algorithm that produces a decent baseline predictive model. See Edwin Chen's answer to https://www.quora.com/Random-Forests/How-do-random-forests-work-in-laymans-terms for more information
* missForest: Fills missing values in a dataset using randomForest.
* softImpute: Performs dimensionality reduction even when there are missing values.

