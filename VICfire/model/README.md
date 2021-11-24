# Model

We tried different modelling techniques such as a simple linear regression, glm, lasso regression and random forest. The code can be found in the `modelling.rmd` file. This file contains the features selection as well as the parameter tuning for the random forest model. The visual representation of the observed vs predicted values can also be found in this file.

Our final model is a random forest model fitted on all of the data (2016-2021). The code for the final model can be found in the `final_model.rmd` and the model object is named `rfmodel_final.rds`.