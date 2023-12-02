source("00_packages.R")
install.packages("sf")
install.packages("glmnet")


#Bases de datos:
rm(list = ls())
library(sf)
library(glmnet)
train <- import("db_tandas/tanda1/train_f.rds")

test <- import("db_tandas/tanda1/test_f.rds")

train["work_size"][is.na(train["work_size"])] <- 0
train["p5100"][is.na(train["p5100"])] <- 0

test["work_size"][is.na(test["work_size"])] <- 0
test["p5100"][is.na(test["p5100"])] <- 0

train <- train %>% select(p5090,p5100, nper,jefe_mujer,educ_jefe,tasa_afil
                          ,reg_salud,num_adult,num_minors,num_ocup,cost_arriendo,cuartos_per,
                          access_finan,bonificaciones,subs_alimeto,primas,
                          mean_hrs_work,work_size,tasa_cotizantes,sub_empleo,dinero_trabajo,
                          dinero_arriendo, dinero_externo, dinero_remesas, ayuda_gob, ingtotugarr, lp)

train<- train %>% mutate(ingtotugarr=as.numeric(ingtotugarr))

# =============================================================================#
############################ === E-NET 1 === ##################################
# =============================================================================#

# In Excel: NO

# Subitted (02/12/2023)
# Nico

# grid para cuadrar el lambda


penalty_grid <- grid_regular(penalty(range = c(0, 10)), mixture(), levels = c(penalty = 50, mixture = 50))


# Especifico el modelo
enet_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")


# Declaro mi receta

enet_recipe <- 
  recipe(formula = ingtotugarr ~ p5090+p5100+ nper+jefe_mujer+educ_jefe+tasa_afil
         +reg_salud+num_adult+num_minors+num_ocup+cost_arriendo+cuartos_per+
         access_finan+bonificaciones+subs_alimeto+primas+
         mean_hrs_work+work_size+tasa_cotizantes+sub_empleo+dinero_trabajo+
         dinero_arriendo+ dinero_externo+ dinero_remesas+ ayuda_gob 
         , data = train)  %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


# Workflow

enet_workflow <- workflow() %>% 
  add_recipe(enet_recipe) %>% 
  add_model(enet_spec)


# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros

enet_final_fit <- fit(enet_workflow, data = train)

test$pred1 <- predict(enet_final_fit, test)[[1]]

# Guardar datos

submission_enet_1 <- test |> select(id, pred1) |>
  rename(ingreso = pred1)


rio::export(submission_enet_1, "results/tanda1_enet1.csv")