
source("00_packages.R")

#===========================#
##### === 0.Import  === #####
#===========================#

rm(list = ls()) #Limpiar el entorno.

# Importo los datos:

p_load(GGally,  #extensiones de ggplot2
       C50) # Graficas

train <- import("db_tandas/tanda1/train_f.rds")

summary(train)

train <- train %>%
  select(p5000, p5010, p5090, p5100, nper, npersug, li, lp, mean_edad, jefe_mujer, educ_jefe,
         tasa_afil, reg_salud, num_adult, num_minors, num_ocup, cost_arriendo, cuartos_per,
         access_finan, bonificaciones, subs_alimeto, pay_alimento, pay_vivienda, pay_otros, primas,
         mean_hrs_work, work_size, tasa_cotizantes, sub_empleo, disp_trabajar, dinero_trabajo,
         dinero_arriendo, dinero_externo, dinero_remesas, ayuda_gob, pobre)%>% 
  filter(cost_arriendo < 600000000)

train$pobre <- as.factor(train$pobre)

#1. caracteristicas del hogar: 

Caract_hogar <- train %>%
  select(p5000, p5010, p5090, nper, num_adult, num_minors, cuartos_per, mean_edad, jefe_mujer, educ_jefe)

stargazer(as.data.frame(Caract_hogar),
          type = "text",title = "Estadisticas Descriptivas caracteristicas del hogar",
          digits = 1, out = "table.txt",
          covariate.labels = c("Total cuartos", "habitaciones", "Tipo de ocupacion", "Numero de personas",
                               "Numero de adultos","Numero de menores de edad","Cuartos por persona", "Edad promedio del hogar",
                               "Cabeza de hogar mujer", "Educacion cabeza de hogar"))

## Caracteristicas laborales
Caract_laborales <- train %>%
  select(num_ocup, mean_hrs_work, work_size, sub_empleo, disp_trabajar, tasa_afil, reg_salud)

stargazer(as.data.frame(Caract_laborales),
          type = "text",title = "Estadisticas Descriptivas Variables Laborales",
          digits = 1, out = "table.txt",
          covariate.labels = c("Ocupados", "Promedio de horas trabajadas", "Tamaño de la empresa",
                               "Otro trabajo","Disponible para trabajar","Tasa de afiliacion",
                               "regimen de salud"))

##Ingresos y gatsos

Caract_ingresos_gastos <- train %>%
  select(dinero_trabajo, dinero_arriendo, dinero_externo, dinero_remesas, ayuda_gob, access_finan,
         bonificaciones, subs_alimeto, pay_alimento, pay_vivienda, pay_otros, primas, cost_arriendo, p5100)


stargazer(as.data.frame(Caract_laborales),
          type = "text",title = "Estadisticas Descriptivas Variables Ingresos y gastos",
          digits = 1, out = "table.txt",
          covariate.labels = c("Ingreso por trabajo", "Ingreso por arriendos", "Otros ingresos", "Remesas",
                               "Recibe ayudad del gobierno","Bonificaciones","Auxilio de alimentacion",
                               "Pago en alimento", "Pagos de vivienda","Otros pagos en especie", "primas","Costo de arriendos","Amortizaciones"))

##Variables de pobreza
variables_pobreza <- train %>%
  select(li, lp, pobre)


stargazer(as.data.frame(variables_pobreza),
          type = "text",title = "Estadisticas Descriptivas Variables de pobreza",
          digits = 1, out = "table.txt",
          covariate.labels = c("Linea de indigencia", "Linea de pobreza", "Pobre"))

# Costo de arriendo y hacinamiento
ggplot(train, aes(x=cuartos_per, y=cost_arriendo, color= pobre)) +
  geom_point() +
  theme_minimal() +
  labs(title="Hacinamiento vs. Costo de Arriendo", 
       x="Numero de cuartos por persona", 
       y="Costo de arriendo", 
       color="Pobre") + 
  scale_y_continuous(labels = scales::dollar_format()) 

##Costo de vivienda y tasa de afiliacion 
ggplot(train, aes(x = tasa_afil, y = cost_arriendo, color = pobre)) +
  geom_point() +
  theme_minimal() +
  labs(title = "tasa de afiliacion vs. Costo de Vivienda",
       x = "Tasa de Afiliación",
       y = "Costo de Vivienda",
       color = "Pobre") +
  scale_y_continuous(labels = scales::dollar_format())

## Educacion jefe y horas trabajadas

ggplot(train, aes(x = educ_jefe, y = mean_hrs_work, fill = pobre)) +
  geom_boxplot() +
  labs(title = "Educación del Jefe del Hogar vs. Horas Trabajadas Promedio",
       x = "Educación del Jefe del Hogar",
       y = "Horas Trabajadas Promedio",
       fill = "Categoría de Pobreza") +
  theme_minimal()

ggplot(train, aes(x = reg_salud, y = mean_hrs_work, fill = pobre)) +
  geom_boxplot() +
  labs(title = "Horas Trabajadas y Régimen de Salud",
       x = "Régimen de Salud",
       y = "Horas Trabajadas Promedio",
       fill = "Categoría de Pobreza") +
  theme_minimal()

## # Numero de adultos en el hogar y categoria de pobreza
ggplot(train, aes(x = as.factor(pobre), y = num_ocup)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Número de Ocupados según la Categoría de Pobreza",
       x = "Categoría de Pobreza",
       y = "Número de adultos") +
  theme_minimal()

## # Numero de adultos en el hogar y categoria de pobreza
ggplot(train, aes(x = as.factor(pobre), y = num_minors)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Número de Ocupados según la Categoría de Pobreza",
       x = "Categoría de Pobreza",
       y = "Número de Menores de edad") +
  theme_minimal()

























