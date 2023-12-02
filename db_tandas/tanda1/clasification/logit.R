
source("00_packages.R")

train <- import("db_tandas/tanda1/train1.rds")
test <- import("db_tandas/tanda1/test1.rds")

train <- train |> mutate( depto = as.factor(depto),
                          clase = as.factor (clase),
                          educ_jefe = as.factor(educ_jefe),
                          p5090 = as.factor(p5090),
                          pobre = factor(pobre, levels=c(0,1),labels=c("pobre","no pobre")))

test <- test |> mutate( depto = as.factor(depto),
                          clase = as.factor (clase),
                          educ_jefe = as.factor(educ_jefe),
                          p5090 = as.factor(p5090)
                          )

logistic_spec <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")


recipe <- recipe(pobre ~ clase + p5000 + p5010 + p5090 + nper + depto + mean_edad + 
                   jefe_mujer + educ_jefe + num_minors + num_ocup, data = train) |>
  step_novel(all_nominal_predictors()) |> step_dummy(all_nominal_predictors()) |>
  step_mutate(p5010 = nper/p5010, num_minors=num_minors/nper, num_ocup = num_ocup/nper) |>
  step_center(all_predictors()) |> step_scale(all_predictors()) |>
  step_zv() |> step_rm(nper)


summary(recipe)

workflow_logit <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(logistic_spec)

cv_folds <- vfold_cv(train, v = 5)

modelo_logit <- workflow_logit %>%
  fit_resamples(resamples = cv_folds,
                metrics = metric_set(accuracy, roc_auc, f_meas))

final_fit <- fit(workflow_logit,data = train )

tidy(final_fit)

test <- test %>%
  mutate(predicciones_logit = predict(final_fit, test)$.pred_class) %>% 
  mutate(pobre = ifelse(as.character(predicciones_logit)=="pobre",1,0))
  
submission_logit1 <- test |>
  select(id, pobre)

rio::export(submission_logit1, "results/classification_logit1.csv")
