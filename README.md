# Overview

This package is built around the function logreg() which allows you to perform binomial logistic regression on a dataset with a single command. The command and its arguments are:

```{r}
logreg(
  data,
  targets,
  response,
  binomial_target,
  binomial_alternate = NULL,
  mode = "binomial",
  splits,
  partition = NULL,
  restrict_NA = FALSE,
  endpoint = NULL
)
```

## Variable explanation:

data:
The dataset which stores the explanatory and response variables.

targets:
A vector of each explanatory variable. Must be in quotes.

response:
The response variable. Must be in quotes.

binomial_target:
The target value of the response variable for binomial regression. Must be in quotes.

binomial_alternate:
(optional) The secondary value of the response variable for binomial regression. Must be in quotes. (if unused, the default is "other")

mode:
(optional) What type of regression you want to do. Must be in quotes. (the only currently supported type is "binomial)

splits:
The number of categories you want your explanatory variable split up into (higher values are more accurate but may not work, I recommend 10)

partition:
(optional) Set to TRUE to cut your dataset down to a managable size for modelling, set to FALSE to bypass this. If you bypass this, it may crash R.

restrict_NA:
Disable the automatic removal of incredibly low correlation variables. Set to true if you keep getting errors that predictive_model_A doesn't exist.

endpoint:
What you want as the output of this command. Options: "check" (check data compatability), format (format response variable), split (split explanatory variables), numbers (view the regression model as text), or model (create a regression model and ggplot2 visualization)


# Installation

Installation is fast and easy! Just run the following lines in your rstudio install:

```{r}
install.packages("devtools")
library(devtools)
devtools::install_github("xyrilria/logreg")
library(logreg)
```
