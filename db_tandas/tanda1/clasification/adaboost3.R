


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



# smote






# Adaboost

ada_spec <- boost_tree(
  trees = tune(),
  min_n = tune()
) |>
  set_mode("classification") %>% 
  set_engine("C5.0")


ada_grid <- grid_random(
  min_n(range = c(1, 10)),
  trees(range = c(1, 100)), size = 10)

recipe <- recipe(pobre ~ ., data = train) |>
  step_center(all_predictors()) |> 
  step_scale(all_predictors()) |>
  step_nzv(all_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_smote(pobre)

ada_wf <- workflow() |>
  add_model(ada_spec) |>
  add_recipe(recipe)


ada_folds <- vfold_cv(train, v = 5)

tuned_ada <- tune_grid(ada_wf,
                       resamples = ada_folds,
                       grid = ada_grid,
                       metrics = metric_set(f_meas)
)

best_params_ada <- tuned_ada %>% select_best(metric = "f_meas")


ada_final <- finalize_workflow(ada_wf, best_params_ada)

ada_final_fit <- fit(ada_final, data = train)


test$pred1 <- predict(ada_final_fit, test)[[1]]

test$pred1

# Guardar datos

submission_ada <- test |> select(id, pred1) |>
  rename(pobre= pred1)


rio::export(submission_ada, "results/clasification_adaboost3.csv")


