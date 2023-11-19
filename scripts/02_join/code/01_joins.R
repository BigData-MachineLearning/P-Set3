
#===========================#
##### === 0.Import  === #####
#===========================#

train_personas <- import("scripts/01_import/output/train_personas.rds") |> clean_names()
train_hogares <- import("scripts/01_import/output/train_hogares.rds") |> clean_names()

test_personas <- import("scripts/01_import/output/test_personas.rds") |> clean_names()
test_hogares <- import("scripts/01_import/output/test_hogares.rds") |> clean_names()


#===================================#
##### === 1.check the data  === #####
#===================================#

# Colnames  


colnames(train_hogares)
colnames(train_personas)

# skim(train_personas)

train_personas[,c("id", "orden")] |> duplicated() |> table()





#========================================#
##### === 2.Cracion de variables === #####
#========================================#

### Edad promedio del hogar ###

mean_edad<-train_personas %>% group_by(id) %>% summarize(mean_edad=mean(p6040,na.rm = TRUE)) 

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

# Si el hogar tiene pensionados





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

#=======================================================#
##### === 3.seleccion de variables de variables === #####
#=======================================================#


train <- train_hogares
test <- test_hogares

train_vars <- c("id", "clase", "p5000","p5010", "p5090", "nper", "depto", "mean_edad", 
             "jefe_mujer", "educ_jefe", "num_minors", "num_ocup", "npersug",
             "ingtotug" , "ingtotugarr" ,"ingpcug",  "li" ,  "lp"  , "pobre" , 
             "indigente", "npobres" ,"nindigentes" )


test_vars <- c("id", "clase","p5000","p5010", "p5090", "nper", "depto", "mean_edad", 
               "jefe_mujer", "educ_jefe", "num_minors", "num_ocup" )

# train1 <- train |> 
#   select(all_of(train_vars))
# 
# test1 <- test |> 
#   select(all_of(test_vars))
# 
# 
# rio::export(train1, "db_tandas/tanda1/train1.rds")
# 
# rio::export(test1, "db_tandas/tanda1/test1.rds")
# 







