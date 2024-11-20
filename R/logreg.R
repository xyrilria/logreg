#' Format data and perform binomial logistic regression in a single command
#'
#'
#' @param data The dataset which stores the explanatory and response variables.
#' @param targets A vector of each explanatory variable. Must be in quotes.
#' @param response The response variable. Must be in quotes.
#' @param binomial_target The target value of the response variable for binomial regression
#' @param binomial_alternate (optional) The secondary value of the response variable for binomial regression (if unused, the default is "other")
#' @param mode (optional) What type of regression you want to do (the only currently supported type is "binomial")
#' @param splits The number of categories you want your explanatory variable split up into (higher values are more accurate but may not work, I recommend 10)
#' @param partition (optional) Set to TRUE to cut your dataset down to a managable size for modelling, set to FALSE to bypass this. If you bypass this, it may crash R.
#' @param endpoint What you want as the output of this command. Options: "check" (check data compatability), format (format response variable), split (split explanatory variables), numbers (view the regression model as text), or model (create a regression model and ggplot2 visualization)
#' @export
logreg <- function(data, targets, response, binomial_target, binomial_alternate = NULL, mode = "binomial", splits, partition = NULL, endpoint = NULL){

  #check if endpoint is at a valid level
  if(endpoint != "check" & endpoint != "format" & endpoint != "split" & endpoint != "model" & endpoint != "numbers"){
    stop("endpoint must be: check, format, split, numbers, or model")
  }

  #check if dependencies are loaded
  if(isNamespaceLoaded(c("tidyverse","dplyr","tidymodels", "caret")) != TRUE){
    stop("This command relies on dependencies stored in `tidyverse`, `dplyr`, `caret`, and `tidymodels`. Please install and library those packages and try again.")
  }

  #first: format training data if needed
  #check if needed:
  if(nrow(data) >= 1000 & is.null(partition)){
    stop("Dataset too large. Run with argument `partition = TRUE` to create training data or `partition = FALSE` to override this error, which may crash R")
  } else if(nrow(data) >= 1000 & partition == TRUE){
    #create partitioned dataset (training data)
    seed <- .Random.seed
    indicies <- createDataPartition(
      y = data[[response]],
      times = 1,
      p = (1000 / nrow(data)),
      list = FALSE
    )
    oldData <- data
    data <- data[indicies , ]
    data_test <- data[-indicies ,]
    print(paste("Training data created, used", nrow(data), "rows out of", nrow(oldData)))
    print("note: this command is currently unable to analyze training data against a testing data model. if this is an issue, manually partition ")
  }
  if(endpoint == "check"){
    return(data)
  }

  #format `response` for binomial regression compatability
  #check if a valid mode is called
  if(mode != "binomial"){
    stop('mode must be "binomial". multinomial regression will be supported soon. use binomial_target and binomial_alternate to define target terms')
  }

  #adjust dataset for binomial
  if(mode == "binomial"){

    if(is.null(binomial_alternate)){
      data <- data %>%
        mutate(
          !!sym(response) := factor(
            if_else(
              !!sym(response) == binomial_target, 0, 1
            ),
            levels = c(0, 1),
            labels = c(binomial_target, "other")
          )
        )
    } else if(!is.null(binomial_alternate)){
      data <- data %>%
        filter(
          !!sym(response) == binomial_target | !!sym(response) == binomial_alternate
        ) %>%
        mutate(
          !!sym(response) := factor(
            if_else(
              !!sym(response) == binomial_target, 0, 1
            ),
            levels = c(0, 1),
            labels = c(binomial_target, binomial_alternate)
          )
        )
    }
  }

  if(endpoint == "format"){
    return(data)
  }

  #1. split variables
  for (target in targets) {

    if(is.factor(data[[target]]) | is.character(data[[target]])){

      #create regression results
      model <- logistic_reg() %>%
        set_engine("glm") %>%
        fit(as.formula(paste(response, "~", target)), data = data, family = "binomial")

      #create dataframe
      df_model <- as.data.frame(tidy(model))

      #splitting
      quantiles <- df_model %>%
        pull(estimate) %>%
        quantile(na.rm = TRUE, probs = seq(0, 1, length.out = splits), names = FALSE)

      seq_range <- seq(min(df_model$estimate, na.rm = TRUE), max(df_model$estimate, na.rm = TRUE), length.out = splits)

      #mutate reg_res, higher L means better predictor
      df_testA <- df_model %>%
        reframe(
          term = term,
          prob_cat = cut(
            if_else(
              abs(estimate) >= 0.1 & abs(estimate) >= std.error,
              estimate,
              NA_real_
            ),
            breaks = quantiles,
            labels = paste0("L", 1:(length(quantiles) - 1)),
            include.lowest = TRUE,
            right = TRUE
          )
        )

      #second grouping mechanism, compare the 2 later
      df_testB <- df_model %>%
        reframe(
          term = term,
          prob_cat = cut(
            if_else(
              abs(estimate) >= 0.1 & abs(estimate) >= std.error,
              estimate,
              NA_real_
            ),
            breaks = seq_range,
            labels = paste0("L", 1:(length(quantiles) - 1)),
            include.lowest = TRUE,
            right = TRUE
          )
        )

      #note: later, set up an actual setting to determine which things get cut out. maybe named threshold?


      df_modelA <- data %>%
        left_join(
          df_testA %>%
            mutate(term = gsub(paste0("^", target), "", df_testA$term)),
          by = setNames("term", as.character(ensym(target)))
        ) %>%
        mutate(
          {{target}} := as.factor(prob_cat)
        ) %>%
        select(-prob_cat)

      df_modelB <- data %>%
        left_join(
          df_testB %>%
            mutate(term = gsub(paste0("^", target), "", df_testA$term)),
          by = setNames("term", as.character(ensym(target)))
        ) %>%
        mutate(
          {{target}} := as.factor(prob_cat)
        ) %>%
        select(-prob_cat)

      #accuracy of model A
      response_levels_A <- length(unique(df_modelA[[response]]))
      if (response_levels_A == 2) {
        #binomial
        predictive_model_A <- logistic_reg() %>%
          set_engine("glm") %>%
          fit(as.formula(paste(response, "~", target)), data = df_modelA, family = "binomial")

      } else if (response_levels_A > 2) {
        stop("no multinomial")
        #multinomial
        predictive_model_A <- multinom_reg() %>%
          set_engine("nnet") %>%
          set_mode("classification") %>%
          fit(as.formula(paste(response, "~", target)), data = df_modelA)
      }

      #predict
      predicted_A <- predict(predictive_model_A, df_modelA, type = "class")

      #combine
      test_A <- cbind(df_modelA, predicted_A)

      #correct
      correct_preds_A <- test_A %>%
        filter(.pred_class == test_A[[response]]) %>%
        count() %>%
        pull(n)

      #total
      total_preds_A <- nrow(test_A)

      #% correct (as decimal)
      accuracy_A <- (correct_preds_A / total_preds_A)


      #accuracy of model B
      response_levels_B <- length(unique(df_modelB[[response]]))
      if (response_levels_B == 2) {
        #binomial
        predictive_model_B <- logistic_reg() %>%
          set_engine("glm") %>%
          fit(as.formula(paste(response, "~", target)), data = df_modelB, family = "binomial")

      } else if (response_levels_B > 2) {
        stop("no multinomial")
        #multinomial
        predictive_model_B <- multinom_reg() %>%
          set_engine("nnet") %>%
          set_mode("classification") %>%
          fit(as.formula(paste(response, "~", target)), data = df_modelB)
      }

      #predict
      predicted_B <- predict(predictive_model_B, df_modelB, type = "class")

      #combine
      test_B <- cbind(df_modelB, predicted_B)

      #correct
      correct_preds_B <- test_B %>%
        filter(.pred_class == test_B[[response]]) %>%
        count() %>%
        pull(n)

      #total
      total_preds_B <- nrow(test_B)

      #% correct (as decimal)
      accuracy_B <- (correct_preds_B / total_preds_B)



      if(accuracy_A > accuracy_B){
        df_target <- df_testA
        print(paste("quantiles for", target,  "created a better predictor"))
      } else if(accuracy_A == accuracy_B){
        print(paste("the models for", target, "create equally accurate predictions. Model A (quantiles) will be used."))
        df_target <- df_testA
      } else{
        df_target <- df_testB
        print(paste("quantiles for", target,  "created a better predictor"))
      }

      data <- data %>%
        left_join(
          df_target %>%
            mutate(term = gsub(paste0("^", target), "", df_target$term)),
          by = setNames("term", as.character(ensym(target)))
        ) %>%
        mutate(
          {{target}} := as.factor(prob_cat)
        ) %>%
        select(-prob_cat)


    } else if(is.numeric(data[[target]])){

      data <- data %>%
        mutate(
          reference_number = row_number()
        )

      #create quantiles for categories
      quantiles <- quantile(data[[target]], na.rm = TRUE, probs = seq(0, 1, length.out = splits), names = FALSE)

      #create categories for the range one thingy
      seq_range <- seq(min(data[[target]], na.rm = TRUE), max(data[[target]], na.rm = TRUE), length.out = splits)

      #categories
      df_testA <- data %>%
        mutate(
          prob_cat = cut(
            !!sym(target),
            breaks = quantiles,
            labels = paste0("L", 1:(length(quantiles) - 1)),
            include.lowest = TRUE,
            right = TRUE
          )
        )

      df_testB <- data %>%
        mutate(
          prob_cat = cut(
            !!sym(target),
            breaks = seq_range,
            labels = paste0("L", 1:(length(quantiles) - 1)),
            include.lowest = TRUE,
            right = TRUE
          )
        )

      df_modelA <- df_testA %>%
        mutate(
          !!sym(target) := prob_cat
        ) %>%
        select(-c(prob_cat))

      df_modelB <- df_testB %>%
        mutate(
          !!sym(target) := prob_cat
        ) %>%
        select(-c(prob_cat))

      #accuracy of model A
      response_levels_A <- length(unique(df_modelA[[response]]))
      formula_str <- paste0("`", response, "` ~ `", target, "`")
      if (response_levels_A == 2) {
        #binomial
        predictive_model_A <- logistic_reg() %>%
          set_engine("glm") %>%
          fit(as.formula(formula_str), data = df_modelA, family = "binomial")

      } else if (response_levels_A > 2) {
        stop("no multinomial")
        #multinomial
        predictive_model_A <- multinom_reg() %>%
          set_engine("nnet") %>%
          set_mode("classification") %>%
          fit(as.formula(formula_str), data = df_modelA, family = "binomial")
      }

      #predict
      predicted_A <- predict(predictive_model_A, df_modelA, type = "class")

      #combine
      test_A <- cbind(df_modelA, predicted_A)

      #correct
      correct_preds_A <- test_A %>%
        filter(.pred_class == !!sym(response)) %>%
        count() %>%
        pull(n)

      #total
      total_preds_A <- nrow(test_A)

      #% correct (as decimal)
      accuracy_A <- (correct_preds_A / total_preds_A)


      #accuracy of model B
      response_levels_B <- length(unique(df_modelB[[response]]))
      if (response_levels_B == 2) {
        #binomial
        predictive_model_B <- logistic_reg() %>%
          set_engine("glm") %>%
          fit(as.formula(formula_str), data = df_modelB, family = "binomial")

      } else if (response_levels_B > 2) {
        stop("no multinomial")
        #multinomial
        predictive_model_B <- multinom_reg() %>%
          set_engine("nnet") %>%
          set_mode("classification") %>%
          fit(as.formula(formula_str), data = df_modelB, family = "binomial")
      }

      #predict
      predicted_B <- predict(predictive_model_B, df_modelB, type = "class")

      #combine
      test_B <- cbind(df_modelB, predicted_B)

      #correct
      correct_preds_B <- test_B %>%
        filter(.pred_class == !!sym(response)) %>%
        count() %>%
        pull(n)

      #total
      total_preds_B <- nrow(test_B)

      #% correct (as decimal)
      accuracy_B <- (correct_preds_B / total_preds_B)

      #check which model is better
      if(accuracy_A > accuracy_B){
        df_target <- df_modelA
        print(paste("quantiles for", target,  "created a better predictor at p =", accuracy_A))
      } else if(accuracy_A == accuracy_B){
        print(paste("the models for", target, "create equally accurate predictions. Model A (quantiles) will be used."))
        df_target <- df_modelA
      } else{
        df_target <- df_modelB
        print(paste("quantiles for", target,  "created a better predictor at p =", accuracy_B))
      }

      data <- data %>%
        mutate(
          !!sym(target) := as.factor(df_target[[target]])
        )

    }
  }
  if(endpoint == "split"){
    return(data)
  }

  #2. model
  #create a prediction for the quality of the model:
  response_levels <- length(unique(data[[response]]))
  formula_str <- paste0("`", response, "` ~ `", paste(targets, collapse = "` + `"), "`")
  if (response_levels == 2) {
    #binomial
    predictive_model <- logistic_reg() %>%
      set_engine("glm") %>%
      fit(as.formula(formula_str), data = data, family = "binomial")

  } else if (response_levels > 2) {
    #multinomial
    stop("multinomial not supported yet")
    predictive_model <- multinom_reg() %>%
      set_engine("nnet") %>%
      set_mode("classification") %>%
      fit(as.formula(formula_str), data = data)
  }

  #predict
  predicted <- predict(predictive_model, data, type = "class")
  #combine
  test <- cbind(data, predicted)

  #cut out N/A data so the model is more descriptive
  test <- test %>%
    filter(
      !is.na(.pred_class)
    )

  #correct
  correct_preds <- test %>%
    filter(.pred_class == test[[response]]) %>%
    count() %>%
    pull(n)

  #total
  total_preds <- nrow(test)

  #% correct (return as decimal)
  model_quality <- (correct_preds / total_preds)


  #create ggplot model
  response_levels <- length(unique(data[[response]]))
  formula_str <- paste0("`", response, "` ~ `", paste(targets, collapse = "` + `"), "`")
  if (response_levels == 2) {
    #binomial
    predictive_model <- logistic_reg() %>%
      set_engine("glm") %>%
      fit(as.formula(formula_str), data = data, family = "binomial")

  } else if (response_levels > 2) {
    stop("multinomial not supported yet")
    #multinomial
    predictive_model <- multinom_reg() %>%
      set_engine("nnet") %>%
      set_mode("classification") %>%
      fit(as.formula(formula_str), data = data)
  }

  if(endpoint == "numbers"){
    return(tidy(predictive_model))
  }

  #predictions/make full dataset
  predictions <- predict(predictive_model, data, type = "prob")
  data <- cbind(data, predictions) %>%
    mutate(
      index = row_number()
    ) %>%
    filter(
      !is.na(!!sym(paste0(".pred_", binomial_target)))
    )

  #graphing
  model <- ggplot(data, aes(x = index, y = !!sym(paste0(".pred_", binomial_target)), color = !!sym(response))) +
    geom_point() +
    ylim(0, 1) +
    theme(
      axis.text.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    labs(
      title = paste(
        'Predicted probability of "',
        if_else(is.null(binomial_alternate),
                paste(
                  as.character(binomial_target), '" (1) vs "other" (0)', sep = ""
                ),
                paste(
                  as.character(binomial_target), '" (1) vs "', as.character(binomial_alternate), '" (0)', sep = ""
                )
        ), sep = ""
      ),
      x = paste("note: overall accuracy =", model_quality)
    )


  print(model_quality)
  return(model)
}
