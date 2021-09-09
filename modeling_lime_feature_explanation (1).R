# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>%
    #step_num2factor(JobLevel, StockOptionLevel) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

automl_leader <- h2o.loadModel(path = "04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20210801_144306")

automl_leader

# 3. LIME ----

predictions_tbl <- automl_leader %>%
    h2o.predict(newdata = as.h2o(test_tbl)) %>%
    as_tibble() %>%
    bind_cols(
        test_tbl %>%
            select(Attrition, EmployeeNumber)
    )

predictions_tbl

# 3.2 Single Explanation ----

explainer_obj <- train_tbl %>%
    select(-Attrition) %>%
    lime(model          = automl_leader,
         bin_continuous = TRUE,
         nbins          = 4,
         quantile_bins  = TRUE)

explainer_obj

explanation <- test_tbl %>%
    slice(5) %>%
    select(-Attrition) %>%
    lime::explain(explainer       = explainer_obj,
                  n_labels        = 1,
                  n_features      = 8,
                  n_permutations  = 5000,
                  kernel_width    = 1.5
    )

explanation %>%
    as_tibble() %>%
    select(feature:prediction)

plot_features(explanation = explanation, ncol = 1)

# 3.3 Multiple Explanations ----

explanation_multiple <- test_tbl %>%
    slice(1:20) %>%
    select(-Attrition) %>%
    lime::explain(explainer       = explainer_obj,
                  n_labels        = 1,
                  n_features      = 8,
                  n_permutations  = 5000,
                  kernel_width    = 1.5
    )

explanation_multiple %>%
    as_tibble()

# plot_features(explanation_multiple, ncol = 4)

plot_explanations(explanation_multiple)

# Challenge, Part 1: plot_features_tq ----

library(glue)

plot_features(explanation = explanation, ncol = 1)

case_1 <- explanation %>%
    filter(case == 1)

data_transformed <- case_1 %>%
    as_tibble %>%
    mutate(
        feature_desc   = as_factor(feature_desc) %>%
            fct_reorder(abs(feature_weight), .desc = FALSE),
        key            = ifelse(feature_weight > 0, "Supports", "Contradicts") %>%
                                    fct_relevel("Supports"),
        case_text      = glue("Case: {case}"),
        label_text     = glue("Label: {label}"),
        prob_text      = glue("Probability: {round(label_prob, 2)}"),
        fit_text       = glue("Explanation Fit: {model_r2 %>% round(2)}")
    ) %>%
    select(feature_desc, feature_weight, key, case_text:fit_text)


data_transformed %>%
    ggplot(aes(x = feature_desc, y = feature_weight, fill = key)) +
    geom_col() +
    coord_flip() +
    theme(legend.position = "bottom") +
    theme_tq() +
    scale_fill_tq() +
    labs(x = "Weight",
         y = "Feature") +
    theme(title = element_text(size = 9)) +
    facet_wrap(~ case_text + label_text + prob_text + fit_text,
               ncol = 1, scales = "free")

plot_features_tq <- function(explanation, ncol) {

    data_transformed_tbl <- explanation %>%
        as_tibble() %>%
        mutate(
            feature_desc   = as_factor(feature_desc) %>%
                fct_reorder(abs(feature_weight), .desc = FALSE),
            key            = ifelse(feature_weight > 0, "Supports", "Contradicts") %>%
                fct_relevel("Supports"),
            case_text      = glue("Case: {case}"),
            label_text     = glue("Label: {label}"),
            prob_text      = glue("Probability: {round(label_prob, 2)}"),
            fit_text       = glue("Explanation Fit: {model_r2 %>% round(2)}")
        ) %>%
        select(feature_desc, feature_weight, key, case_text:fit_text)

    data_transformed_tbl %>%
        ggplot(aes(x = feature_desc, y = feature_weight, fill = key)) +
        geom_col() +
        coord_flip() +
        theme(legend.position = "bottom") +
        theme_tq() +
        scale_fill_tq() +
        labs(x = "Weight",
             y = "Feature") +
        theme(title = element_text(size = 9)) +
        facet_wrap(~ case_text + label_text + prob_text + fit_text,
                   ncol = ncol, scales = "free")

}

explanation_multiple %>%
    filter(case %in% 1:6) %>%
    plot_features_tq(ncol = 3)

# Challenge, Part 2: plot_explanations_tq ----

plot_explanations(explanation_multiple)

data_transformed_multiple <- explanation_multiple %>%
    as_tibble() %>%

    mutate(
        case    = as_factor(case),
        order_1 = rank(feature)
    ) %>%

    group_by(feature) %>%
    mutate(order_2 = rank(feature_value)) %>%
    ungroup() %>%

    mutate(order = order_1 * 1000 + order_2) %>%
    mutate(
        feature_desc = as_factor(feature_desc) %>%
            fct_reorder(order, .desc = T)
    ) %>%
    select(case, feature_desc, feature_weight, label)

data_transformed_multiple %>%
    ggplot(aes(x = case, y = feature_desc)) +
    geom_tile(aes(fill = feature_weight)) +
    facet_wrap(~ label) +
    theme_tq() +
    scale_fill_gradient2(low  = palette_light()[[2]],
                         high = palette_light()[[1]]) +
    theme(panel.grid = element_blank(),
          legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
          ) +
    labs(y = "Feature", x = "Case",
         fill = glue("Feature Weight"))

plot_explanations_tq <- function(explanation) {

    data_transformed_multiple <- explanation %>%
        as_tibble() %>%

        mutate(
            case    = as_factor(case),
            order_1 = rank(feature)
        ) %>%

        group_by(feature) %>%
        mutate(order_2 = rank(feature_value)) %>%
        ungroup() %>%

        mutate(order = order_1 * 1000 + order_2) %>%
        mutate(
            feature_desc = as_factor(feature_desc) %>%
                fct_reorder(order, .desc = T)
        ) %>%
        select(case, feature_desc, feature_weight, label)

    data_transformed_multiple %>%
        ggplot(aes(x = case, y = feature_desc)) +
        geom_tile(aes(fill = feature_weight)) +
        facet_wrap(~ label) +
        theme_tq() +
        scale_fill_gradient2(low  = palette_light()[[2]],
                             mid  = "white",
                             high = palette_light()[[1]]) +
        theme(panel.grid = element_blank(),
              legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
        ) +
        labs(y = "Feature", x = "Case",
             fill = glue("Feature Weight"))
}

explanation_multiple %>% plot_explanations()
explanation_multiple %>% plot_explanations_tq()