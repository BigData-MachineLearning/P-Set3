# Empezamos con OSM

if(require("pacman")==F){install.packages("pacman")}
# Cargar pacman (contiene la función p_load)
require(pacman) 

# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rio, # Import data easily
       plotly, # Gráficos interactivos
       tidymodels,
       skimr,
       rsample, # Para resampleos
       blockCV,# Hacer los folds para el cv 
       janitor,
       ranger,
       rpart,
       caret,
       stargazer,
       varhandle,
       themis
      ) 

