
source("00_packages.R")

#===========================#
##### === 0.Import  === #####
#===========================#

rm(list = ls()) #Limpiar el entorno.

train_personas <- import("scripts/01_import/output/train_personas.rds") |> clean_names()
train_hogares <- import("scripts/01_import/output/train_hogares.rds") |> clean_names()

test_personas <- import("scripts/01_import/output/test_personas.rds") |> clean_names()
test_hogares <- import("scripts/01_import/output/test_hogares.rds") |> clean_names()

#===================================#
##### === 1.check the data  === #####
#===================================#

# Colnames  

colnames(test_hogares)
colnames(test_personas)

# skim(train_personas)

train_personas[,c("id", "orden")] |> duplicated() |> table()


#========================================#
##### === 2.Cracion de variables === #####
#========================================#

### Edad promedio del hogar ###

mean_edad<-train_personas %>% group_by(id) %>%
  summarize(mean_edad=mean(p6040,na.rm = TRUE)) 

train_hogares<-left_join(train_hogares,mean_edad)
rm(mean_edad)

mean_edad<-test_personas %>% group_by(id) %>% summarize(mean_edad=mean(p6040,na.rm = TRUE)) 

test_hogares<-left_join(test_hogares,mean_edad)
rm(mean_edad)

### Si jefe del hogar es mujer ### 

jefe_mujer  <- train_personas %>% group_by(id) %>% 
  summarize(jefe_mujer = as.numeric(any(p6020 == 2 & p6050 == 1))) 
  
train_hogares<-left_join(train_hogares,jefe_mujer)
rm(jefe_mujer)


jefe_mujer  <- test_personas %>% group_by(id) %>% 
  summarize(jefe_mujer = as.numeric(any(p6020 == 2 & p6050 == 1))) 

test_hogares<-left_join(test_hogares,jefe_mujer)
rm(jefe_mujer)


### Educacion del jefe del hogar ###

educ_jefe  <- train_personas %>% group_by(id) %>%
  summarize( educ_jefe  = first(p6210, order_by = p6050) ) 

train_hogares<-left_join(train_hogares,educ_jefe)
rm(educ_jefe)


educ_jefe  <- test_personas %>% group_by(id) %>%
  summarize( educ_jefe  = first(p6210, order_by = p6050) ) 

test_hogares<-left_join(test_hogares,educ_jefe)
rm(educ_jefe)

# Tasa de afiliacion

tasa_afil <- train_personas |>group_by(id) |>
  summarise(tasa_afil = sum(p6090==1, na.rm = T)) 

train_hogares <- left_join(train_hogares, tasa_afil) |>
  mutate(tasa_afil = tasa_afil/nper)
rm(tasa_afil)
  
tasa_afil <- test_personas |>group_by(id) |>
  summarise(tasa_afil = sum(p6090==1, na.rm = T)) 

test_hogares <- left_join(test_hogares, tasa_afil) |>
  mutate(tasa_afil = tasa_afil/nper)
rm(tasa_afil)

# regimen subsidiado

reg_salud <- train_personas |> group_by(id) |>
  summarise(reg_salud = sum(p6100 == 3, na.rm = T),
            num_adult = sum(p6040 >= 18, na.rm = T)) |>
  mutate(num_adult = ifelse(num_adult ==0, 1, num_adult)) |>
  mutate(reg_salud = reg_salud/num_adult)

train_hogares <- left_join(train_hogares, reg_salud)
rm(reg_salud) 

reg_salud <- test_personas |> group_by(id) |>
  summarise(reg_salud = sum(p6100 == 3, na.rm = T),
            num_adult = sum(p6040 >= 18, na.rm = T)) |>
  mutate(num_adult = ifelse(num_adult ==0, 1, num_adult)) |>
  mutate(reg_salud = reg_salud/num_adult)

test_hogares <- left_join(test_hogares, reg_salud)
rm(reg_salud) 


### numero de menores de edad en el hogar ###

num_minors <- train_personas %>%  
  mutate(menor = ifelse(p6040 <18, 1, 0)) %>% 
  group_by(id) %>% summarise(num_minors = sum(menor))

train_hogares <- left_join(train_hogares, num_minors)
rm(num_minors)  

num_minors <- test_personas %>%  
  mutate(menor = ifelse(p6040 <18, 1, 0)) %>% 
  group_by(id) %>% summarise(num_minors = sum(menor))

test_hogares <- left_join(test_hogares, num_minors)
rm(num_minors)  


### Numero de mayores de edad ocupados ###

num_ocup <- train_personas %>%  
  mutate(worker = ifelse(p6040 >=18 & oc == 1, 1, 0)) %>% 
  group_by(id) %>% summarise(num_ocup = sum(worker)) |>
  mutate(num_ocup = ifelse(is.na(num_ocup),0, num_ocup) )

train_hogares <- left_join(train_hogares, num_ocup) 
rm(num_ocup)  


num_ocup <- test_personas %>%  
  mutate(worker = ifelse(p6040 >=18 & oc == 1, 1, 0)) %>% 
  group_by(id) %>% summarise(num_ocup = sum(worker)) |>
  mutate(num_ocup = ifelse(is.na(num_ocup),0, num_ocup) )

test_hogares <- left_join(test_hogares, num_ocup)
rm(num_ocup)  

# Variable costo vvienda

train_hogares <- train_hogares |>
  mutate(cost_arriendo = case_when( is.na(p5130) ~ p5140,
                                    is.na(p5140) ~ p5130))

test_hogares <- test_hogares |>
  mutate(cost_arriendo = case_when( is.na(p5130) ~ p5140,
                                    is.na(p5140) ~ p5130))

# personas_por cuarto

train_hogares <- train_hogares |> 
  mutate(cuartos_per = nper/p5010)

test_hogares <- test_hogares |> 
  mutate(cuartos_per = nper/p5010)

# Acceso sistema financiero

access_finan <- train_personas |> group_by(id) |>
  summarise(access_finan = sum(p7510s5 == 1, na.rm = T))

train_hogares <- left_join(train_hogares, access_finan)
rm(access_finan)

access_finan <- test_personas |> group_by(id) |>
  summarise(access_finan = sum(p7510s5 == 1, na.rm = T))

test_hogares <- left_join(test_hogares, access_finan)
rm(access_finan)


# Primas, bonificaciones, auxilio de transporte / # ocupados

bonificaciones <- train_personas |> group_by(id) |>
  summarise(bonificaciones = sum(p6580 == 1|p6545 == 1|p6585s2==1 , na.rm = T))

train_hogares <- left_join(train_hogares, bonificaciones)|>mutate(bonificaciones = bonificaciones/num_adult)
rm(bonificaciones)

bonificaciones <- test_personas |> group_by(id) |>
  summarise(bonificaciones = sum(p6580 == 1|p6545 == 1|p6585s2==1 , na.rm = T))

test_hogares <- left_join(test_hogares, bonificaciones) |>mutate(bonificaciones = bonificaciones/num_adult)
rm(bonificaciones)


# Auxilio alimentacion

subs_alimeto <- train_personas |> group_by(id) |>
  summarise(subs_alimeto = sum(p6585s1==1 , na.rm = T))

train_hogares <- left_join(train_hogares, subs_alimeto)|>mutate(subs_alimeto = subs_alimeto/num_adult)
rm(subs_alimeto)

subs_alimeto <- test_personas |> group_by(id) |>
  summarise(subs_alimeto = sum(p6585s1==1 , na.rm = T))

test_hogares <- left_join(test_hogares, subs_alimeto) |>mutate(subs_alimeto = subs_alimeto/num_adult)
rm(subs_alimeto)


# p6590 - pago en alimento

pay_alimento <- train_personas |> group_by(id) |>
  summarise(pay_alimento = sum(p6590 == 1, na.rm = T))

train_hogares <- left_join(train_hogares, pay_alimento)
rm(pay_alimento)

pay_alimento <- test_personas |> group_by(id) |>
  summarise(pay_alimento = sum(p6590 == 1, na.rm = T))

test_hogares <- left_join(test_hogares, pay_alimento)
rm(pay_alimento)


# p6600 - pago en vivienda

pay_vivienda <- train_personas |> group_by(id) |>
  summarise(pay_vivienda = sum(p6600 == 1, na.rm = T))

train_hogares <- left_join(train_hogares, pay_vivienda)
rm(pay_vivienda)

pay_vivienda <- test_personas |> group_by(id) |>
  summarise(pay_vivienda = sum(p6600 == 1, na.rm = T))

test_hogares <- left_join(test_hogares, pay_vivienda)
rm(pay_vivienda)


# p6620 otros pagoos en especie

pay_otros <- train_personas |> group_by(id) |>
  summarise(pay_otros = sum(p6620 == 1, na.rm = T))

train_hogares <- left_join(train_hogares, pay_otros)
rm(pay_otros)

pay_otros <- test_personas |> group_by(id) |>
  summarise(pay_otros = sum(p6620 == 1, na.rm = T))

test_hogares <- left_join(test_hogares, pay_otros)
rm(pay_otros)

# p6630s1-6 primas 

primas <- train_personas |> group_by(id) |>
  summarise(primas = sum(p6630s1 == 1| p6630s2 == 1| p6630s3 == 1| p6630s4 == 1
                         |p6630s6 == 1 , na.rm = T))

train_hogares <- left_join(train_hogares, primas)
rm(primas)

primas <- test_personas |> group_by(id) |>
  summarise(primas = sum(p6630s1 == 1| p6630s2 == 1| p6630s3 == 1| p6630s4 == 1|
                                 p6630s6 == 1 , na.rm = T))

test_hogares <- left_join(test_hogares, primas)
rm(primas)

# Horas trabajadas en el hogar en promedio 

mean_hrs_work <- train_personas |> group_by(id) |>
  summarise(mean_hrs_work = mean(p6800 , na.rm = T)) |> 
  mutate(mean_hrs_work = ifelse(is.nan(mean_hrs_work), 0, mean_hrs_work))

train_hogares <- left_join(train_hogares, mean_hrs_work)
rm(mean_hrs_work)

mean_hrs_work <- test_personas |> group_by(id) |>
  summarise(mean_hrs_work = mean(p6800 , na.rm = T)) |> 
  mutate(mean_hrs_work = ifelse(is.nan(mean_hrs_work), 0, mean_hrs_work))

test_hogares <- left_join(test_hogares, mean_hrs_work)
rm(mean_hrs_work)

# p6870 tqamaño lugar de trabajo - jefe del hogar

work_size  <- train_personas %>% group_by(id) %>%
  summarize( work_size  = first(p6870, order_by = p6050) ) 

train_hogares<-left_join(train_hogares,work_size)
rm(work_size)


work_size  <- test_personas %>% group_by(id) %>%
  summarize( work_size  = first(p6870, order_by = p6050) ) 

test_hogares<-left_join(test_hogares,work_size)
rm(work_size)

colnames(test_hogares)
colnames(train_hogares)

# cotizantes/ numero adultos p6920

tasa_cotizantes <- train_personas %>% group_by(id) %>% 
  mutate(n_afiliados = ifelse(!is.na(p6920)&p6920 == 1 | p6920 == 3, 1, 0)) %>% 
  summarize(tasa_cotizantes = sum(n_afiliados == 1, na.rm=T))

train_hogares <- left_join(train_hogares,tasa_cotizantes) %>% 
  mutate(tasa_cotizantes=tasa_cotizantes/num_adult)

tasa_cotizantes <- test_personas %>% group_by(id) %>% 
  mutate(n_afiliados = ifelse(!is.na(p6920)&p6920 == 1 | p6920 == 3, 1, 0)) %>% 
  summarize(tasa_cotizantes = sum(n_afiliados == 1, na.rm=T))

test_hogares <- left_join(test_hogares,tasa_cotizantes) %>% 
  mutate(tasa_cotizantes=tasa_cotizantes/num_adult)

rm(tasa_cotizantes)


#  subempleo p7090 + p7110 + p7120

sub_empleo <- train_personas %>%
  group_by(id) %>%
  summarize(sub_empleo = sum(p7090 + p7110 + p7120, na.rm = TRUE))

train_hogares <- left_join(train_hogares, sub_empleo, by = "id") %>% 
  mutate(sub_empleo=sub_empleo/num_adult)

sub_empleo <- test_personas %>%
  group_by(id) %>%
  summarize(sub_empleo = sum(p7090 + p7110 + p7120, na.rm = TRUE))

test_hogares <- left_join(test_hogares, sub_empleo, by = "id") %>% 
  mutate(sub_empleo=sub_empleo/num_adult)

rm(sub_empleo)

# p7160 dispónible para trabajar

disp_trabajar <- train_personas %>% 
  group_by(id) %>% mutate(disp_trabajar=ifelse(p7160==1,1,0)) %>% 
  summarize(disp_trabajar=sum(disp_trabajar==1, na.rm=T))

train_hogares <- left_join(train_hogares,disp_trabajar) %>% 
  mutate(disp_trabajar=disp_trabajar/num_adult)

disp_trabajar <- test_personas %>% 
  group_by(id) %>% mutate(disp_trabajar=ifelse(p7160==1,1,0)) %>% 
  summarize(disp_trabajar=sum(disp_trabajar==1, na.rm=T))

test_hogares <- left_join(test_hogares,disp_trabajar) %>% 
  mutate(disp_trabajar=disp_trabajar/num_adult)

rm(disp_trabajar)

# p7422 si recibio plata por trabajo

dinero_trabajo <- train_personas %>% 
  group_by(id) %>% mutate(dinero_trabajo=ifelse(p7422==1,1,0)) %>% 
  summarize(dinero_trabajo=sum(dinero_trabajo==1,na.rm=T))

train_hogares <- left_join(train_hogares,dinero_trabajo)

dinero_trabajo <- test_personas %>% 
  group_by(id) %>% mutate(dinero_trabajo=ifelse(p7422==1,1,0)) %>% 
  summarize(dinero_trabajo=sum(dinero_trabajo==1,na.rm=T))

test_hogares <- left_join(test_hogares,dinero_trabajo)

rm(dinero_trabajo)

# p7495 recibio pagos por arriendos

dinero_arriendo <- train_personas %>% 
  group_by(id) %>% mutate(dinero_arriendo=ifelse(p7495==1,1,0)) %>% 
  summarize(dinero_arriendo=sum(dinero_arriendo==1,na.rm=T))

train_hogares <- left_join(train_hogares,dinero_arriendo)

dinero_arriendo <- test_personas %>% 
  group_by(id) %>% mutate(dinero_arriendo=ifelse(p7495==1,1,0)) %>% 
  summarize(dinero_arriendo=sum(dinero_arriendo==1,na.rm=T))

test_hogares <- left_join(test_hogares,dinero_arriendo)

rm(dinero_arriendo)

# p7505 ingresos de otras personas, hogares, etc. 

dinero_externo <- train_personas %>% 
  group_by(id) %>% mutate(dinero_externo=ifelse(p7505==1,1,0)) %>% 
  summarize(dinero_externo=sum(dinero_externo==1,na.rm=T))

train_hogares <- left_join(train_hogares,dinero_externo)

dinero_externo <- test_personas %>% 
  group_by(id) %>% mutate(dinero_externo=ifelse(p7505==1,1,0)) %>% 
  summarize(dinero_externo=sum(dinero_externo==1,na.rm=T))

test_hogares <- left_join(test_hogares,dinero_externo)

rm(dinero_externo)

# p7510s2 remesas

dinero_remesas <- train_personas %>% 
  group_by(id) %>% mutate(dinero_remesas=ifelse(p7510s2==1,1,0)) %>% 
  summarize(dinero_remesas=sum(dinero_remesas==1,na.rm=T))

train_hogares <- left_join(train_hogares,dinero_remesas)

dinero_remesas <- test_personas %>% 
  group_by(id) %>% mutate(dinero_remesas=ifelse(p7510s2==1,1,0)) %>% 
  summarize(dinero_remesas=sum(dinero_remesas==1,na.rm=T))

test_hogares <- left_join(test_hogares,dinero_remesas)

rm(dinero_remesas)

# Ayudas dinero del país p7510s3

ayuda_gob <- train_personas %>% 
  group_by(id) %>% mutate(ayuda_gob=ifelse(p7510s3==1,1,0)) %>% 
  summarize(ayuda_gob=sum(ayuda_gob==1,na.rm=T))

train_hogares <- left_join(train_hogares,ayuda_gob)

ayuda_gob <- test_personas %>% 
  group_by(id) %>% mutate(ayuda_gob=ifelse(p7510s3==1,1,0)) %>% 
  summarize(ayuda_gob=sum(ayuda_gob==1,na.rm=T))

test_hogares <- left_join(test_hogares,ayuda_gob)

rm(ayuda_gob)

#Oficio jefe del hogar
oficio  <- train_personas %>% group_by(id) %>%
  summarize( oficio  = first(oficio, order_by = p6050) ) 

train_hogares<-left_join(train_hogares,oficio)
rm(oficio)


oficio  <- test_personas %>% group_by(id) %>%
  summarize( oficio  = first(oficio, order_by = p6050) ) 

test_hogares<-left_join(test_hogares,oficio)
rm(oficio) 


#=======================================================#
##### === 3.seleccion de variables de variables === #####
#=======================================================#


train_f <- train_hogares
test_f <- test_hogares


colSums(is.na(train_f))
rio::export(train_f, "db_tandas/tanda1/train_f.rds")

rio::export(test_f, "db_tandas/tanda1/test_f.rds")







