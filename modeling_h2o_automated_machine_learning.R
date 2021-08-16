# H2O MODELING ----

# 1. Setup ----

# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#     if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
#
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(forcats)
library(cowplot)
library(fs)
library(glue)

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
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing

recipe_obj <- recipe(Attrition ~ ., train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_num2factor(JobLevel, levels = c('1', '2', '3', '4', '5')) %>%
    step_num2factor(StockOptionLevel, levels = c('0', '1', '2', '3'), transform = function(x) {x + 1}) %>%
    prep()


train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Modeling ----

h2o.init()


split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
    x = x,
    y = y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    max_runtime_secs = 30,
    nfolds = 5 # 10 might be more accurate but will take longer
)

typeof(automl_models_h2o)

slotNames(automl_models_h2o)

automl_models_h2o@leader

h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20210801_142327")
h2o.getModel("DeepLearning_grid__1_AutoML_20210801_142327_model_3")

automl_models_h2o@leaderboard %>%
    as_tibble() %>%
    slice(1) %>%
    pull(model_id) %>%
    h2o.getModel()

extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE) {

    model_name <- h2o_leaderboard %>%
        as_tibble() %>%
        slice(n) %>%
        pull(model_id)

    if(verbose) message(model_name)

    return(model_name)

}

automl_models_h2o@leaderboard %>%
    extract_h2o_model_name_by_position(2) %>%
    h2o.getModel()

# Already saved, won't overwrite, throws error
# h2o.getModel("GLM_1_AutoML_20210801_144306") %>%
#     h2o.saveModel(path = "04_Modeling/h2o_models/")
#
# h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20210801_144306") %>%
#     h2o.saveModel(path = "04_Modeling/h2o_models/")
#
# h2o.getModel("DeepLearning_grid__2_AutoML_20210801_144306_model_1") %>%
#     h2o.saveModel(path = "04_Modeling/h2o_models/")
#
# h2o.getModel("GBM_grid__1_AutoML_20210801_144306_model_12") %>%
#     h2o.saveModel(path = "04_Modeling/h2o_models/")

deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_grid__2_AutoML_20210801_144306_model_1")

stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_AutoML_20210801_144306")

stacked_ensemble_h2o

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions)

predictions_tbl <- predictions %>% as_tibble()
predictions_tbl

deep_learning_h2o@allparameters

# Visualizing the Leaderboard ----

data_transformed <- automl_models_h2o@leaderboard %>%
    as_tibble() %>%
    select(model_id,auc,logloss) %>%
    mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
    slice(1:10) %>%
    rownames_to_column() %>%
    mutate(
        model_id = as.factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
    ) %>%
    gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>%
    mutate(model_id = paste0(rowname, ". ", model_id) %>% as.factor() %>% fct_rev())

data_transformed %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = 3) +
    geom_label(aes(label = round(value, 2), hjust = "inward")) +
    facet_wrap(~ key, scales = "free_x") + # data in long format for facets to work
    theme_tq() +
    scale_color_tq() +
    labs(title = "H2O Leaderboard Metrics",
         subtitle = paste0("Ordered by: auc"),
         y = "Model Position, Model ID", x = "")

plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"),
                                 n_max = 20, size = 4, include_lbl = TRUE) {

    # Setup inputs
    order_by <- tolower(order_by[[1]])

    leaderboard_tbl <- h2o_leaderboard %>%
        as_tibble() %>%
        select(model_id,auc,logloss) %>%
        mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
        rownames_to_column(var = "rowname") %>%
        mutate(model_id = paste0(rowname, ". ", as.character(model_id) %>% as.factor()))

    # Tranformation

    if (order_by == "auc") {

        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(model_id = as_factor(model_id) %>% reorder(auc),
                   model_type = as.factor(model_type)
                   ) %>%
            gather(key = key, value = value,
                   -c(model_id, model_type, rowname), factor_key = T)

    } else if (order_by == "logloss") {

        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(model_id = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
                   model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value,
                   -c(model_id, model_type, rowname), factor_key = T)

    } else {

        stop(paste0("order_by = '", order_by, "' is not a permitted option."))
    }

    # Visualization
    g <- data_transformed_tbl %>%
        ggplot(aes(value, model_id, color = model_type)) +
        geom_point(size = size) +
        facet_wrap(~ key, scales = "free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Leaderboard Metrics",
             subtitle = paste0("Ordered by: ", toupper(order_by)),
             y = "Model Position, Model ID", x = "")

    if (include_lbl) g <- g + geom_label(aes(label = round (value, 2), hjust = "inward"))

    return(g)

}

automl_models_h2o@leaderboard %>%
    plot_h2o_leaderboard(order_by = "logloss")

# BONUS: GRID SEARCH & CV ----

deeplearning_h2o

h2o.performance(deeplearning_h2o, as.h2o(test_tbl))

deeplearning_grid_01 <- h2o.grid(
    algorithm = "deeplearning",
    grid_id = "deeplearning_grid_01",

    # h2o.deeplearning()
    x = x,
    y = y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    nfolds = 5,

    hyper_params = list(
        hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
        epochs = c(10, 50, 100)
    )
)

deeplearning_grid_01

h2o.getGrid(grid_id = "deeplearning_grid_01",
            sort_by = "auc",
            decreasing = TRUE)

deeplearning_grid_01_model_7 <- h2o.getModel("deeplearning_grid_01_model_7")

deeplearning_grid_01_model_7 %>% h2o.auc(train = T, valid = T, xval = T)

deeplearning_grid_01_model_7 %>%
    h2o.performance(newdata = as.h2o(test_tbl))