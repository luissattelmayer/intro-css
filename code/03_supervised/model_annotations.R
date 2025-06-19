
needs(tidyverse, tidymodels, textrecipes, vip)

df <- read_csv("data/annotations_june2025.csv") |> 
    mutate(label = as.factor(label), 
           label = fct_relevel(label, "1", "0")
           ) |> 
    drop_na(label)

df |> 
    count(label)

df |> 
    count(label) |> 
    mutate(prop = n / sum(n)*100) 

set.seed(1598)

split <- initial_split(df, prop = 0.8, strata = label)

df_train <- training(split)
df_test <- testing(split)

economy_recipe <- recipe(label ~ text, data = df_train) |>
    step_tokenize(text) |>
    step_stopwords(text) |>
    step_tfidf(text)  

rf_spec <- rand_forest(trees = 1000) |> 
    set_engine("ranger", importance = "impurity") |> 
    set_mode("classification")

rf_spec
# Create a workflow with the recipe and model for random forest
economy_workflow_rf <- workflow() |>
    add_recipe(economy_recipe) |>
    add_model(rf_spec)

economy_model_rf <- fit(economy_workflow_rf, data = df_train)

economy_model_rf

economy_model_rf |> 
    extract_fit_parsnip() |>
    vip(num_features = 20)

preds_economy_rf <- augment(economy_model_rf, df_test)

preds_economy_rf |> view()

conf_mat <- conf_mat(preds_economy_rf, truth = label, estimate = .pred_class)

conf_mat

compute_metrics <- metric_set(accuracy, recall, precision, f_meas)

compute_metrics(preds_economy_rf, truth = label, estimate = .pred_class)

preds_economy_rf |> 
    filter(label == 1, .pred_class == 0) |> 
    pull(text)

# Look at false positives

preds_economy_rf |> 
    filter(label == 0, .pred_class == 1) |> 
    pull(text)

# Alternative on SVM

svm_spec <- svm_linear() |>
    set_mode("classification") |> 
    set_engine("LiblineaR")

economy_workflow_svm <- workflow() %>%
    add_recipe(economy_recipe) %>%
    add_model(svm_spec)

economy_model_svm <- fit(economy_workflow_svm, data = df_train)

preds_economy_svm <- augment(economy_model_svm, df_test)

conf_mat_svm <- conf_mat(preds_economy_svm, truth = label, estimate = .pred_class)

conf_mat_svm

compute_metrics(preds_economy_svm, truth = label, estimate = .pred_class)



# Prediction on full corpus

full_corpus <- read_csv("https://www.dropbox.com/s/dpu5m3xqz4u4nv7/tweets_house_rep_party.csv?dl=1")

preds_full_corpus <- augment(economy_model_rf, full_corpus)

preds_full_corpus |> 
    count(party)

preds_full_corpus |> 
    group_by(twitter_handle) |> 
    count(.pred_class) |> 
    mutate(prop = n/sum(n)*100) |>
    filter(.pred_class == 1) |> 
    ggplot(aes(prop)) +
    geom_histogram(bins = 30) 


preds_full_corpus |>
    group_by(party) |>
    count(.pred_class)  |>
    mutate(prop = n / sum(n) * 100) |>
    drop_na() |>
    filter(.pred_class == "1")  |>
    ggplot(aes(x = party, y = prop)) +
    geom_col() +
    theme_light()

preds_full_corpus |>
    mutate(month = floor_date(date, "month")) |> 
    group_by(month, party) |>
    count(.pred_class)  |>
    mutate(prop = n / sum(n) * 100) |>
    drop_na() |>
    filter(.pred_class == "1")  |>
    ggplot(aes(x = month, y = prop, color = party)) +
    geom_line() +
    theme_light() +
    # Add republican and democrat colors
    scale_color_manual(values = c("blue", "red"))


# Hyperparameter tuning

econ_folds <- vfold_cv(df_train)


econ_model_rf_tune <- rand_forest(trees = tune()) |> 
    set_mode("classification") |> 
    set_engine("ranger")

econ_recipe_rf_tune <- recipe(label ~ text, data = df_train) |>
    step_tokenize(text) |>
    step_tokenfilter(text) |>
    step_tfidf(text)

lambda_grid <- grid_regular(
    trees(range = c(100, 1000)),
    levels = 10
)

econ_wf_rf_tune <- workflow() |> 
    add_recipe(econ_recipe_rf_tune) |> 
    add_model(econ_model_rf_tune)

set.seed(123)

tune_rf_rs <- tune_grid(
    econ_wf_rf_tune,
    econ_folds,
    grid = lambda_grid,
    metrics = metric_set(f_meas, accuracy, recall, precision)
)
tune_rf_rs


collect_metrics(tune_rf_rs) |> 
    ggplot(aes(x = trees, y = mean)) +
    geom_line() +
    theme_light() +
    facet_wrap(~.metric, scales = "free_y") +
    scale_x_continuous(breaks = seq(100, 1000, by = 100))

final_rf_wf <- finalize_workflow(econ_wf_rf_tune, select_best(tune_rf_rs, metric = "f_meas"))

final_rf_wf

final_fitted <- last_fit(final_rf_wf, split)

predictions_last_fit <- collect_predictions(final_fitted)
predictions_last_fit

compute_metrics(predictions_last_fit, truth = label, estimate = .pred_class)
