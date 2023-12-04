
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

train <- train %>% select(p5000,p5010,p5090,p5100,p5100, nper, npersug,li,lp,mean_edad,jefe_mujer,educ_jefe,
                          tasa_afil,reg_salud,num_adult,num_minors,num_ocup,cost_arriendo,cuartos_per,
                          access_finan,bonificaciones,subs_alimeto,pay_alimento,pay_vivienda,pay_otros,primas,
                          mean_hrs_work,work_size,tasa_cotizantes,sub_empleo,disp_trabajar,dinero_trabajo,
                          dinero_arriendo, dinero_externo, dinero_remesas, ayuda_gob, pobre)

train <- train |> 
  mutate( pobre = factor(pobre))

names(train)
forest_spec <- rand_forest(min_n = tune(),
                           trees = tune(),
                           mtry  = tune()) |>
  set_engine("ranger") |>
  set_mode("classification")

recipe <- recipe(pobre ~., data = train) |>
  step_nzv() |>
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) 




forest_wf <- workflow() |>
  add_model(forest_spec) |>
  add_recipe(recipe)

forest_folds <- vfold_cv(train, v = 10)

rf_grid <- grid_regular(
  min_n(range = c(1, 10)),
  trees(range = c(200, 400)),
  mtry(range = c(4,10)),
  levels =5)

tuned_forest <- tune_grid(forest_wf,
                          resamples =forest_folds,
                          grid = rf_grid,
                          metrics = metric_set(f_meas))

best_parms_forest <- select_best(tuned_forest, metric = "f_meas")
best_parms_forest

# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
forest_final <- finalize_workflow(forest_wf, best_parms_forest)

forest_final_fit <- fit(forest_final, data = train)



test$pred1 <- predict(forest_final_fit, test)[[1]]

test$pred1
# Guardar datos

submission_forest <- test |> select(id, pred1)


rio::export(submission_tree, "results/tanda1_forest.csv")

