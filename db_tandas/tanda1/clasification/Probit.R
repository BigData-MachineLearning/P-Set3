
# Cargar paquetes
source("00_packages.R")

# Limpiar el entorno

rm(list = ls()) #Limpiar el entorno.

train <- import("db_tandas/tanda1/train_f.rds")


test <- import("db_tandas/tanda1/test_f.rds")

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


probit_spec <- logistic_reg() %>% 
  set_engine("glm", family = stats::binomial(link = "probit")) %>%
  set_mode("classification") %>% 
  translate()

recipe <- recipe( pobre ~ . , data = train) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) 

workflow_probit <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(probit_spec)

cv_folds <- vfold_cv(train, v = 5)

modelo_probit <- workflow_probit %>%
  fit_resamples(resamples = cv_folds, metrics = metric_set(accuracy, roc_auc, f_meas))


final_fit <- fit(workflow_probit,data = train )

tidy(final_fit)

test <- test %>%
  mutate(predicciones_probit = predict(final_fit, test)$.pred_class) %>% 
  mutate(pobre = ifelse(as.character(predicciones_probit)=="pobre",1,0))

submission_probit <- test |>
  select(id, pobre)

rio::export(submission_probit, "results/classification_probit.csv")


