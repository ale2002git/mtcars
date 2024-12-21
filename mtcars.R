#Importacion de librerias
library(dplyr)
library(lubridate)
library(tidyr)

data(mtcars)

#Lectura de datos
df <- as.data.frame(mtcars)

#Selección de columnas y filtrado de filas
df <- df %>%
  select(mpg, cyl, hp, gear) %>%  
  filter(cyl > 4)                    

print(df)

#Ordenación y renombrado de columnas
df <- df %>%
  arrange(desc(hp)) %>%               
  rename(consumo = mpg, potencia = hp) 

print(df)

#Creación de nuevas columnas y agregación de datos
df <- df %>%
  mutate(eficiencia = consumo / potencia) %>%  
  group_by(cyl) %>%                            
  summarise(
    consumo_medio = mean(consumo),             
    potencia_max = max(potencia)              
  )

print(df)

#Creación del segundo dataframe y unión

tipo_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

#left_join
df <- df %>%
  left_join(tipo_transmision, by = "gear")

print(df)

# Transformar a formato largo
df_largo <- df %>%
  pivot_longer(cols = c(consumo_medio, potencia_max), 
               names_to = "medida", 
               values_to = "valor")

print(df_largo)

colnames(df)


#Identificar combinaciones duplicadas
duplicados <- df_largo %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

print("Duplicados identificados")
print(duplicados)

#Transformar de nuevo a formato ancho
df_ancho <- df_largo %>%
  pivot_wider(names_from = medida, 
              values_from = valor, 
              values_fn = mean)

print(df_ancho)

