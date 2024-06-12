# CUIA

# Create a data frame with the country names and number of species
cono_sur <- data.frame(
  Pais = c("Argentina", "Sur de Brasil", "Chile", "Paraguay", "Uruguay"),
  N_especies = c(10221, 9189, 5155, 5296, 2911)
)

orden_deseado <- c("Paraguay", "Chile", "Argentina", "Uruguay", "Sur de Brasil")

# Establecer el orden como factor con el orden deseado
cono_sur$Pais <- factor(cono_sur$Pais, levels = orden_deseado)

# Ordenar la base de datos según el factor
cono_sur_ordenado <- cono_sur[order(cono_sur$Pais), ]

# Mostrar la base de datos ordenada
cono_sur_ordenado

# Calculate the percentage for each country
cono_sur$Porcentaje <- round((cono_sur$N_especies / sum(cono_sur$N_especies)) * 100, 1)

# Add the total row for the Cono Sur
cono_sur <- rbind(cono_sur, 
                  data.frame(Pais = "Total Cono Sur", 
                             N_especies = sum(cono_sur$N_especies), 
                             Porcentaje = 100))

# Name the data frame "cono_sur"
names(cono_sur) <- c("Pais", "N_especies", "Porcentaje")

# Variables
cono_sur$Pais <- as.factor(cono_sur$Pais)

# Display the table
print(cono_sur)

# Define colors based on the original figure
colors <- c("white", "red3","skyblue2", "skyblue4" , "yellow")

cono_sur2 <- cono_sur[-c(6:7),]

# Load the ggplot2 package
library(ggplot2)

# Crear el gráfico de torta con etiquetas de porcentaje y bordes negros
gg_cono_sur <- ggplot(data = cono_sur2, aes(x = "", y = Porcentaje, fill = Pais, label = paste0(Porcentaje, "%"))) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Añadir bordes negros
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = paste0(Porcentaje, "%")), position = position_stack(vjust = 0.6), size = 20) +  # Aumentar tamaño de letras
  labs(title = "Riqueza de Especies de Plantas Vasculares del Cono Sur", 
       fill = "País") +
  theme_void() +
  theme(legend.position = "bottom",  # Colocar la leyenda a la derecha
        plot.title = element_text(size = 30),  # Aumentar tamaño del título
        legend.text = element_text(size = 35),  # Aumentar tamaño de letra en la leyenda
        plot.margin = margin(2, 2, 2, 2, "cm"))

# Mostrar el gráfico
gg_cono_sur



