


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
                          dinero_arriendo, dinero_externo, dinero_remesas, ayuda_gob, ingtotugarr,lp)

train <- train |>
  mutate( ingtotugarr = as.numeric(ingtotugarr))


boost_spec <- boost_tree(
  trees = tune(), # Número de árboles a ajustar
  min_n = tune(), # Número mínimo de observaciones en cada nodo terminal
  learn_rate = tune() # Tasa de aprendizaje
) %>%
  set_mode("regression") # Establecer el modo de regresión


# Especificar el modelo boost_tree

# Crear una receta para preprocesar los datos
recipe <- recipe(ingtotugarr ~ ., data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 



boost_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(boost_spec)



# Especificar el modelo boost_tree

# Crear una receta para preprocesar los datos
recipe <- recipe(ingtotugarr ~ ., data = train) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) 



boost_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(boost_spec)




# Crear una cuadrícula de parámetros para explorar
grid <- grid_regular(
  trees(range = c(300, 400)),
  min_n(range = c(1, 3)),
  learn_rate(range = c(0.01, 0.1)), 
  levels = c(trees = 2, min_n = 2, learn_rate = 10)  # Número de combinaciones de parámetros a generar
)

# Crear un esquema de validación cruzada
folds <- vfold_cv(train, v = 5) # 5 pliegues



# Ajustar el modelo con la receta, la cuadrícula y los pliegues
boost_res <- tune_grid(
  boost_workflow,
  grid = grid,
  resamples = folds,
  metrics = metric_set(rmse, rsq) # Métricas a evaluar
)



best_model <- select_best(boost_res, metric = "rmse")



boost_final <- finalize_workflow(boost_workflow, best_model)

boost_final_fit <- fit(boost_final, data = train)


test<- augment(boost_final_fit, new_data = test)


submission<- test |> select(id,lp,.pred) |>
  rename(ingtotugarr=.pred) |>
  mutate(pobre = ifelse(ingtotugarr<lp,1,0)) |>
  select(id,pobre)
submission




