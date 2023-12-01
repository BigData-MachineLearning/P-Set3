
source("00_packages.R")

#===========================#
##### === 0.Import  === #####
#===========================#

rm(list = ls()) #Limpiar el entorno.
# Importo los datos:

train <- import("db_tandas/tanda1/train_f.rds")


test <- import("db_tandas/tanda1/test_f.rds")

# volver variables missing 0 

train["work_size"][is.na(train["work_size"])] <- 0
train["p5100"][is.na(train["p5100"])] <- 0

test["work_size"][is.na(test["work_size"])] <- 0
test["p5100"][is.na(test["p5100"])] <- 0

train <- train |> 
  mutate( pobre = factor(as.character(pobre)))

tree_spec <- decision_tree(tree_depth = tune(),
                           min_n = tune(),
                           cost_complexity = tune()) |>
  set_engine("rpart") |>
  set_mode("classification")

recipe <- recipe(pobre ~., data = train) |>
  step_nzv() |>
  step_novel(all_nominal_predictors()) |> 
  step_rm(p5130, p5140, p5010, id,npersug, ingtotug,
          ingtotugarr, ingpcug, li,  lp,  indigente ,
          npobres ,nindigentes ,fex_c   , fex_dpto ) |>
  step_dummy(all_nominal_predictors()) 


tree_wf <- workflow() |>
  add_model(tree_spec) |>
  add_recipe(recipe)

tree_folds <- vfold_cv(train, v = 10)

grid_tree <- grid_regular(tree_depth(range = c(4,20)),
                          min_n(range = c(10,30)),
                          cost_complexity(range = c(0.1,1)),
                          levels = c(tree_depth =5,
                                     min_n =5,
                                     cost_complexity = 10))

tuned_tree <- tune_grid(tree_wf,
                        resamples =tree_folds,
                        grid = grid_tree,
                        metrics = metric_set(f_meas))

best_parms_tree <- select_best(tuned_tree, metric = "f_meas")
best_parms_tree

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
tree_final <- finalize_workflow(tree_wf, best_parms_tree)

tree_final_fit <- fit(tree_final, data = train)

test$pred1 <- predict(tree_final_fit, test)[[1]]
