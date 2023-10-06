
data_companion<-read_excel("database.xlsx",sheet="companion_pa")

datosm<-data_companion[,-c(1:2)]

# Convertir el tibble a un data frame
datosm_df <- as.data.frame(datosm)

# Asignar los nombres de fila
rownames(datosm_df) <- data_companion$Censo

datosm <- datosm_df

# mat.j<-vegdist(datosm, method = "jaccard", diag = T, binary = F)
mat.b<-vegdist(datosm, method = "bray", diag = T, binary = F)


## Clasificación

## Ordenamiento

### Escalamiento multi dimensional (EMD) o Análisis de coordenadas principales (PCoA)

library(FactoMineR)
library(factoextra)

# MULTIDIMENSINAL SCALING - PRINCIPAL COORDINATE ANALYSIS
mds.ALT <- pco(mat.b, k=3
               ) # k es n-1 censos

mds.eig<-((mds.ALT$eig)/sum(mds.ALT$eig))*100

# sum(mds.eig[1:1])

df.mds<-data.frame(mds.ALT$points) #posici?n de cada punto en los ejes. 

df.todo <- cbind(df.mds[,c(1:2)], data_companion)

df.todo$REG <- factor(df.todo$REG,
                      levels = c("CONCORDIA" ,"PALMAR" ,"GUALEGUAYCHU"),
                      labels = c("Conconrdia" ,"El Palmar" ,"Gualeguaychu"))

library(ggplot2)
library(ggrepel)

ggPCoA<-ggplot(df.todo, aes(x=X1, y=X2))+
  geom_hline(yintercept=0, col="black")+
  geom_vline(xintercept=0, col="black")+
  geom_point(size=,aes(col=REG, pch=as.factor(REG)))+
  theme_classic()+
  scale_color_manual(values = c("coral4","burlywood2","palegreen4"))+
  scale_shape_manual(values=c(16,15,17))+
  theme(panel.grid.major.y = element_blank())+
  theme(panel.grid.major.x = element_blank())+
  theme(text = element_text(size=20, color='black'))+
  theme(axis.text = element_text(color='black'))+
  # geom_text(hjust = 0, nudge_x = .01, size=3,aes(label=Censo))+
  theme(legend.direction = "horizontal")+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  labs(x = "Principal coordinate 1 (18.64%)", y = "Principal coordinate 2 (14.08%)",colour = "REG",pch="REG",
       groups = "REG")+
  stat_ellipse(aes(x=X1, y=X2,color=REG))+
  # geom_polygon(data = data_disc, aes(x = axis_1, y = axis_2, group = site, fill = site), alpha = 0.2) +
  # stat_chull(aes(x = axis_1, y = axis_2, group = site), geom = "path", col = "black", fill = NA)+
  geom_text_repel(
    data = df.todo,
    aes(label = Censo),
    size = 5,
    box.padding = unit(0.001, "lines"),
    point.padding = unit(1, "lines"))
ggPCoA

# AAO ----

data<-read.delim("tablaAAO.txt")

names(data)
str(data)
data$T<-factor(data$T, levels=c("Unripe", "Medium ripe", "Ripe", "Overripe"), labels = c("Inmaduro", "Medio maduro", "Maduro", "Sobre maduro"))
data$Conc<-as.factor(Conc)

# library(dplyr)
#              
# table_aao <- data %>% dplyr::group_by()
                          
library(ggplot2)                        
gg_mad_color_L<-ggplot(data, aes(x=MADUREZ, y=(Mean), color=MADUREZ, fill=MADUREZ)) + 
               geom_bar(stat="identity", size=.05, color="black") +
               labs(y="", x= "")+
               geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd),width=.05, color="black")+
               theme_classic() +
               theme(
                 legend.position = "bottom",
                 legend.background = element_rect(fill = "white"),
                 legend.title = element_blank(),
                 panel.grid.major.y = element_line(size = 0.05, color = 'black'),
                 panel.grid.major.x = element_blank(),
                 text = element_text(size = 20, color = 'black'),
                 plot.background = element_rect(fill = "white"),
                 panel.background = element_rect(fill = "white", color = "white"),
                 panel.border = element_rect(fill = "transparent"),
                 axis.text.x = element_blank(),
                 axis.text.y = element_text(angle = 0, hjust = 1, color = 'black')
               )+
               scale_color_manual(values = c("palegreen4","yellowgreen","gold","gold4"))+
               scale_fill_manual(values = c("palegreen4","yellowgreen","gold","gold4"))+
               # scale_y_continuous(breaks=seq(0, 50, 5))+
               geom_text(aes(label = round(Mean,2)), vjust = -0.5, hjust = -0.1, size=5)+
               theme(axis.text.y = element_text(angle = 0, hjust = 1, color='black'))+
               scale_y_continuous(breaks = seq(0,90, 30), limits = c(0, 90))
             
gg_mad_color_L


library(ggplot2)
# library(forcats) x = fct_reorder2(species, site, n)

ggaao_f <- ggplot(data, aes(x = T, y = inh, fill = Conc)) +
  geom_bar(stat = "identity", size = 1, width= 0.5, position = position_dodge(0.5)) +
  labs(y = "inh (%)", x = "") +
  # facet_grid(. ~ T) +
  # facet_wrap(~site, ncol = 3, scales = "free") +
  geom_errorbar(aes(ymin=inh-DS, ymax=inh+DS),width=.05, color="black", position = position_dodge(0.5))+
  scale_fill_manual(values = c("lightblue","grey", "lightblue4","#358999","blue")) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(size = 0.05, color = 'black'),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 20, color = 'black'),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(fill = "transparent"),
    axis.text.x = element_text(angle = 60, hjust = 1, color = 'black'),
    axis.text.y = element_text(angle = 0, hjust = 1, color = 'black')
  ) +
  scale_y_continuous(breaks = seq(0, 110, 10), limits = c(-10, 110))

# ggaao_f + coord_cartesian(ylim = c(20, 100))
ggaao_f

filtered_data <- subset(data, Conc != 0)

ggaao_f2 <- ggplot(filtered_data, aes(x = Conc, y = inh, fill = T, color="")) +
  geom_bar(stat = "identity", size = 1, width = 0.5, position = position_dodge(0.5)) +
  labs(y = "inh (%)", x = "") +
  # facet_grid(. ~ Conc) +
  # facet_wrap(~site, ncol = 3, scales = "free") +
  geom_errorbar(aes(ymin=inh, ymax=inh+DS), size=1, width=.1, color="black", position = position_dodge(0.5))+
  scale_fill_manual(values = c("palegreen4","yellowgreen","gold","gold4")) +
  scale_color_manual(values = c("black")) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    panel.grid.major.y = element_line(size = 0.05, color = 'black'),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 20, color = 'black'),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(fill = "transparent"),
    axis.text.x = element_text(angle = 0, hjust = 1, color = 'black'),
    axis.text.y = element_text(angle = 0, hjust = 1, color = 'black')
  ) +
  scale_y_continuous(breaks = seq(0, 110, 10), limits = c(-10, 110))+
  guides(color = "none")

# ggaao_f + coord_cartesian(ylim = c(20, 100))
ggaao_f2

# Bibliografía

# Leer el contenido del archivo
bibliografia <- readLines("bibliografia.txt", encoding = "UTF-8")

# Dividir la bibliografía en cada cita (suponiendo que las citas están separadas por punto y aparte)
citas <- strsplit(bibliografia, "\n\n")[[1]]

# Función para limpiar el formato de cada cita
limpiar_cita <- function(cita) {
  cita <- gsub("\n", " ", cita)  # Unir el texto de la cita
  cita <- gsub("\\[CrossRef\\]", "", cita)  # Eliminar [CrossRef]
  cita <- gsub("\\[PubMed\\]", "", cita)  # Eliminar [PubMed]
  return(cita)
}

# Organizar citas por letra del alfabeto
citas_por_letra <- vector("list", length = 26)
for (letra in 1:26) {
  citas_por_letra[[letra]] <- sapply(citas, function(cita) {
    if (substr(cita, 1, 1) == toupper(letters[letra])) {
      limpiar_cita(cita)
    } else {
      NULL
    }
  })
  citas_por_letra[[letra]] <- citas_por_letra[[letra]][!is.null(citas_por_letra[[letra]])]
}

# Generar el nuevo listado ordenado
nuevo_listado <- character()
for (letra in 1:26) {
  if (length(citas_por_letra[[letra]]) > 0) {
    nuevo_listado <- append(nuevo_listado, paste("Letra", toupper(letters[letra]), ":", "\n\n"))
    nuevo_listado <- append(nuevo_listado, citas_por_letra[[letra]])
    nuevo_listado <- append(nuevo_listado, "\n\n")
  }
}

# Convierte la lista de citas a una cadena de texto
texto_listado <- paste(nuevo_listado, collapse = "\n")

# Escribe la cadena de texto en un archivo
writeLines(texto_listado, "bibliografia_ordenada.txt")


## bibliografía 2.0 #####################################################################

# Leer el archivo completo como una sola cadena de texto
archivo <- readLines("bibliografia.txt", encoding = "UTF-8")

# Unir todas las líneas en una sola cadena de texto
texto_completo <- paste(archivo, collapse = " ")

# Dividir el texto en entradas bibliográficas utilizando patrones de separación
entradas <- strsplit(texto_completo, "(?<=\\.)\\s+(?=[A-Z])", perl = TRUE)[[1]]

# Procesar cada entrada bibliográfica
nuevo_listado <- lapply(entradas, function(entrada) {
  # Eliminar "[CrossRef]" y "[PubMed]"
  entrada <- gsub("\\[(CrossRef|PubMed)\\]", "", entrada, perl = TRUE)
  # Eliminar saltos de línea innecesarios dentro de las citas
  entrada <- gsub("\\s+", " ", entrada, perl = TRUE)
  # Eliminar saltos de línea innecesarios al final de la entrada
  entrada <- gsub("\\s+$", "", entrada)
  # Devolver la entrada procesada
  entrada
})

# Filtrar entradas que no estén vacías o nulas
nuevo_listado <- nuevo_listado[sapply(nuevo_listado, function(x) !is.null(x) && x != "")]

# Convertir la lista de citas a una cadena de texto
texto_listado <- paste(nuevo_listado, collapse = "\n\n")

# Escribir la cadena de texto en un archivo
writeLines(texto_listado, "bibliografia_ordenada.txt")

## Acomodar bibliografía con saltos de línea ###########################################

# Leer el archivo completo como una sola cadena de texto
archivo <- readLines("bibliografia.txt", encoding = "UTF-8")

# Unir todas las líneas en una sola cadena de texto
texto_completo <- paste(archivo, collapse = " ")

# Dividir el texto en entradas bibliográficas utilizando patrones de separación
entradas <- strsplit(texto_completo, "(?<=\\.)\\s+(?=[A-Z])", perl = TRUE)[[1]]

# Procesar cada entrada bibliográfica
nuevo_listado <- lapply(entradas, function(entrada) {
  # Eliminar saltos de línea innecesarios dentro de las citas
  entrada <- gsub("\\s+", " ", entrada, perl = TRUE)
  # Devolver la entrada procesada
  entrada
})

# Convertir la lista de citas a una cadena de texto
texto_listado <- paste(nuevo_listado, collapse = "\n\n")

# Escribir la cadena de texto en un archivo
writeLines(texto_listado, "bibliografia_ordenada.txt")

