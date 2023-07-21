#-----------
# Actividad práctica
#-----------

library(readxl)

library(tidyverse)

library(readxl)
datos<- read_excel("base.xlsx")
View(datos)
names(datos)

Tarea2 <- read_excel("base.xlsx")
View(Tarea1)
names(Tarea1)

# 1. Lea la base de datos base.xlsx en Rstudio y Mencione las variables que contiene
incendi <- read_excel("Material teórico Unidad 1-20230609/incendios.xlsx")
View(tarea1)
glimpse(incendios)

fBasics::basicStats(datos)

# 2. ¿Hay datos faltantes en el archivo? ¿Qué función utilizaría para verificarlo
# rápidamente?
summary(Tarea2)

library(fBasics)
basicStats(datos[,1:7])

basicStats(datos[,4])

basicStats(datos)


install.packages("fBasics")





# 3. Obtenga una tabla resumen utilizando el paquete dplyr, explorelo y obtenga
# la media y el desvío de la edad según la variable gran_area_descripcion

library(tidyverse)

datos %>% group_by(gran_area_descripcion) %>% summarise(n = n(), media = mean(edad),desvio = sd(edad))

# 4. Obtenga una tabla resumen para la variable edad, pero solo para el nivel de
# la variable gran_area_descripcion: HUMANIDADES

tbl4 <- incendios |>
  filter(fecha < 2010) |>
  group_by(provincia) |>
  summarise(
    n = n(),
    media = mean(total),
    desvio = sd(total)
  )


 datos %>% filter(gran_area_descripcion == "HUMANIDADES" ) %>% summarise( n = n(),media = mean(edad, na.rm= T),desvio = sd(edad, na.rm= T))
  
  
   
    
Media_edad=round(mean(edad, na.rm = T),2),
Desvio_edad=round(sd(edad, na.rm = T),2),
n=length(edad)



library(dplyr)
#Resumen%
group_by(sexo)%>%
  summarise(Media_edad=round(mean(edad, na.rm = T),2),
            Desvio_edad=round(sd(edad, na.rm = T),2),
            n=length(edad))


datos%>% filter(gran_area_descripcion=="humanidades")%>% group_by(gran_area_descripcion, "humanidades"
       %>% summarise ( mean(edad,na.rm= T ),sd(edad, na.rm = T)

datos%>% filter(gran_area_descripcion==humanidades)%>% group_by(humanidades)%>%summarise(Media_edad=round(mean(edad,na.rm =T),2),Desvio_edad=round(sd(edad,na.rm = T),2),n=length(edad))                     
                       
# 5. ¿Qué comandos utilizaría si quisiera exportar la tabla resumen obtenida en
# el punto anterior?

tablahumanidades <- datos %>% filter(gran_area_descripcion == "HUMANIDADES" ) %>% summarise( n = n(),media = mean(edad, na.rm= T),desvio = sd(edad, na.rm= T))

write.table (tablahumanidades,file="humanidades.csv" )  

writexl::write_xlsx(datos %>% filter(gran_area_descripcion == "HUMANIDADES" ) %>% summarise( n = n(),media = mean(edad, na.rm= T),desvio = sd(edad, na.rm= T))
)




# 6. Obtenga un histograma para la variable edad, personalice el título y los
# nombre de los ejes. ayuda (?hist)

hist(incendios$total)

incendios |> pull(total) |> hist()

hist(datos$edad, col="blue", main= "Histograma_edad", xlab= "Edad", ylab= "Frecuencia")


# 7. Obtenga un boxplot para la variable edad, personalice el título y los nombre
# de los ejes. ayuda (?boxplot)

boxplot(incendios$total)
boxplot(incendios$total, ylab = "Superficie total", main = "Incendios")
boxplot(total ~ provincia, incendios, ylab = "Superficie total", main = "Incendios")
boxplot(total ~ provincia, incendios, ylab = "Superficie total", main = "Incendios", horizontal = T)
boxplot(total ~ provincia, incendios, ylab = "Superficie total", main = "Incendios", las = 1)



boxplot(datos$edad)

boxplot(datos$edad, main= "Boxplot_edad", ylab= "Edad")



# 8. Obtenga un histograma para la variable edad solo para el sexo== Masculino,
# personalice el título y los nombre de los ejes. ayuda (?hist)

hist(incendios$total[incendios$provincia %in% c("Santa Fe", "Córdoba")])

incendios |>
  filter(provincia %in% c("Santa Fe", "Córdoba")) |>
  pull(total) |>
  hist()

hist(datos$edad[datos$sexo %in% c("Masculino")], main= "Edad_Masculino", xlab= "Edad", ylab= "Frecuencia")


# 9. Obtenga un boxplot para la variable edad solo para los valores que fueron
# registrados después de 2013, personalice el título y los nombre de los ejes. ayuda (?boxplot)

incendios |>
  filter(fecha > 2000) |>
  pull(total) |>
  boxplot()

boxplot(datos$edad)


boxplot(datos$edad, main= "Boxplot_edad", ylab= "Edad")

datos %>% filter(anio == anio > 2013= T)

edad2 <- datos$("anio, edad")




boxplot(datos$edad[datos$anio %in% c("anio > 2013")], main= "Edades_a_partir_2013", xlab= "Edad", ylab= "Frecuencia")


boxplot(datos$edad[datos$anio %in% c("anio > 2013")], main= "Edades_a_partir_2013", xlab= "Edad", ylab= "Frecuencia")

datos %>%
  filter(anio > 2013) %>%
  boxplot()
  
boxplot(datos$edad[datos$anio %in% c("anio > 2013", )])




        
View(datos)    

boxplot

boxplot(dato, datos = NULL, ..., subconjunto, na.action = NULL,

datos %in%
  filter(anio > 2013)


     main= "Edad_Masculino", xlab= "Edad", ylab= "Frecuencia")

# 10. Investigue la función par(frow) y coloque los últimos 4 gráficos juntos
# https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html



