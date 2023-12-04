


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
train["oficio"][is.na(train["oficio"])] <- -99



test["work_size"][is.na(test["work_size"])] <- 0
test["p5100"][is.na(test["p5100"])] <- 0
test["oficio"][is.na(test["oficio"])] <- -99

train <- train %>% select(p5000,p5010,p5090,p5100,p5100, nper, npersug,li,lp,mean_edad,jefe_mujer,educ_jefe,
                          tasa_afil,reg_salud,num_adult,num_minors,num_ocup,cost_arriendo,cuartos_per,
                          access_finan,bonificaciones,subs_alimeto,pay_alimento,pay_vivienda,pay_otros,primas,
                          mean_hrs_work,work_size,tasa_cotizantes,sub_empleo,disp_trabajar,dinero_trabajo,
                          dinero_arriendo, dinero_externo, dinero_remesas, ayuda_gob, pobre,oficio) |>
  mutate(mean_edad_2 = mean_edad^2) |>mutate(oficio = factor(oficio))

train <- train |> 
  mutate( pobre = factor(pobre))

test <- test |>
  mutate(mean_edad_2 = mean_edad^2,
         oficio = factor(oficio))

# Adaboost

ada_spec <- boost_tree(
  trees = 93,
  min_n = 8 
  
) |>
  set_mode("classification") %>% 
  set_engine("C5.0")


recipe <- recipe(pobre ~ ., data = train) |>
  step_center(all_numeric_predictors()) |> 
  step_scale(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

ada_wf <- workflow() |>
  add_model(ada_spec) |>
  add_recipe(recipe)


ada_final_fit <- fit(ada_wf, data = train)


test$pred1 <- predict(ada_final_fit, test)[[1]]

test$pred1
# Guardar datos

submission_ada <- test |> select(id, pred1)


rio::export(submission_ada, "results/clasification_adaboost5.csv")


