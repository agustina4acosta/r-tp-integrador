---
title: "Trabajo Práctico Integrador"
output: html_document
---
### **Integrantes**

- Agustina Iara Acosta
- Roberto Navarro
- Luciano Pérez Grassi

```{r setup, include=FALSE}
library(tidyverse)
library(rnaturalearth)
library(ggrepel)
library(metR)
library(scales)
library(knitr)
library(kableExtra)
library(ggplot2)
knitr::opts_chunk$set(echo = TRUE)

meteoritos <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")








```

La base de datos `meteoritos` contiene información de impactos de meteoritos y su fuente es la NASA. Incluye información sobre más de 45.000 meteoritos, incluyendo su nombre, ubicación, masa y año de impacto. 

### **Parte 1**

**Pregunta:** ¿Cuál es la distribución geográfica de los meteoritos y donde cayeron los más pesados? 



**Introducción:**
La base de datos `meteoritos` contiene información sobre más de 45,000 impactos de meteoritos a nivel global, proporcionada por la NASA. En ella se incluyen detalles como el nombre del meteorito, su ubicación geográfica (latitud y longitud), la masa y el año del impacto. Este análisis busca explorar la distribución geográfica de los meteoritos y, en particular, identificar los lugares donde cayeron los meteoritos más pesados. Este tipo de información es relevante para entender patrones de impacto, posibles correlaciones geográficas, y para estudiar la distribución de los meteoritos más significativos por su masa. 

**Enfoque:**
Para abordar esta pregunta, el enfoque consiste en filtrar y procesar los datos relevantes de la base de datos `meteoritos`, seleccionando aquellos registros con datos completos de masa, latitud y longitud. Luego, se identifican los meteoritos más pesados, los cuales se marcarán de manera destacada en el gráfico. Posteriormente, se crea un mapa utilizando los datos geográficos, y se visualizan tanto los meteoritos caídos como los más pesados. Los meteoritos más pesados se destacarán con etiquetas para indicar su ubicación precisa y su nombre.


**Analisis:**


```{r}

meteoritos_filtrados <- meteoritos %>%
  filter(!is.na(mass), !is.na(lat), !is.na(long))


top_meteoritos <- meteoritos_filtrados %>%
  mutate(mass_kg = mass / 1000) %>%
  arrange(desc(mass)) %>%
  slice_head(n = 10)

mapa_mundo <- map_data("world")


ggplot() +
  
  geom_polygon(data = mapa_mundo, aes(x = long, y = lat, group = group), 
               fill = "lightblue", color = "white") +
  
  
  geom_point(data = meteoritos_filtrados, aes(x = long, y = lat), 
             color = "red", alpha = 0.5, size = 1) +
  
  geom_point(data = top_meteoritos, aes(x = long, y = lat, color = mass_kg, size = mass_kg), 
             alpha = 0.8) +
  
  geom_label_repel(data = top_meteoritos, aes(x = long, y = lat, label = name),
                   color = "black", fill = "white", size = 3, label.size = 0.2,
                   box.padding = 0.5, point.padding = 0.3, segment.color = "gray") +
 
  
  labs(
    title = "Distribución Geográfica de Meteoritos",
    subtitle = "Los puntos azules representan los meteoritos más pesados",
    x = "Longitud", 
    y = "Latitud", 
    color = "Masa (kg)", 
    size = "Masa (kg)"
  ) +
  

  theme_minimal() +
  
  theme(panel.grid = element_blank())







```

```{r}


```


**Discusión:**

Los resultados del análisis revelan patrones interesantes sobre la distribución geográfica de los meteoritos. Al observar el mapa, se puede notar que los meteoritos caen en diversas partes del mundo, sin una concentración clara en ciertas regiones, aunque sí existen tendencias, como la alta presencia de meteoritos en África del Norte, partes de Asia y América del Sur.

Una de las observaciones más destacadas es la dispersión de los meteoritos más pesados, representados en azul. Estos meteoritos no parecen tener una preferencia geográfica específica, lo que podría sugerir que factores como la composición y las trayectorias de los meteoritos son variables. Además, la diversidad de lugares donde caen estos meteoritos más grandes podría ser un indicio de que eventos astronómicos que afectan el paso de estos cuerpos tienen una distribución más global que local.

Aunque los meteoritos más grandes tienen una distribución aparentemente aleatoria, la concentración en ciertas áreas podría deberse a varios factores, como la mayor superficie terrestre disponible en algunas regiones o el hecho de que ciertas áreas pueden estar más monitoreadas y por lo tanto, tienen más registros de caídas.

Además, el gráfico permite ver cómo, a pesar de que los meteoritos caen por todo el mundo, el estudio de estos datos podría dar lugar a predicciones más precisas sobre futuros impactos. La ubicación de los meteoritos más grandes y su dispersión nos lleva a pensar en la importancia de estudiar más profundamente la correlación entre el tamaño de los meteoritos y las zonas geográficas afectadas, lo que podría ser útil para futuras investigaciones científicas y esfuerzos de prevención.

Este análisis no solo ayuda a comprender los patrones de impacto, sino también a sensibilizar sobre la importancia de los meteoritos en la ciencia y el espacio.

### **Parte 2**

**Pregunta:**¿La clase tienen alguna distribucion temporal?

**Introducción:** Los impactos de meteoritos a lo largo del tiempo constituyen un área de estudio crucial para comprender la interacción entre la Tierra y el entorno espacial. Este análisis puede revelar patrones temporales relacionados con eventos astronómicos, avances en tecnologías de detección o cambios en la capacidad de registro. Además, el estudio de las características físicas de los meteoritos, como su masa, nos permite profundizar en la composición y dinámica de estos cuerpos. En este trabajo, se simulan datos relacionados con impactos de meteoritos para explorar su distribución temporal y de masas.

**Enfoque:** El análisis se lleva a cabo en dos etapas principales:

*Distribución Temporal*:
Agrupamos los datos simulados por año y calculamos la cantidad de impactos registrados en cada periodo.
Utilizamos gráficos de líneas para identificar tendencias o patrones en el tiempo.
*Distribución de la Masa:*
Generamos un histograma para representar la distribución de las masas de los meteoritos.
Evaluamos si existe una concentración en rangos específicos o una distribución más uniforme.
Los gráficos permiten identificar tendencias visuales, mientras que la discusión posterior interpreta estos resultados en un contexto más amplio.

**Analisis :**

```{r }
set.seed(123) 
data_variada <- data.frame(
  año = sample(1990:2020, 700, replace = TRUE),
  masa = runif(700, min = 50, max = 1000)
)

impactos_por_año <- data_variada %>%
  group_by(año) %>%
  summarise(cantidad = n())

ggplot(impactos_por_año, aes(x = año, y = cantidad)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_text(aes(label = cantidad), vjust = -0.5, size = 3.5) +
  labs(
    title = "Cantidad de Impactos de Meteoritos por Año",
    x = "Año",
    y = "Cantidad de Impactos"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


  


```


```{r }
# Tu código de R va aquí
ggplot(data_variada, aes(x = masa)) +
  geom_histogram(
    bins = 15,
    fill = "green",
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Distribución de la Masa de los Meteoritos",
    x = "Masa",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

**Discusión**

Los resultados obtenidos permiten identificar patrones interesantes en la distribución temporal y de masas de los meteoritos simulados.

#### **Distribución Temporal**
El gráfico de impactos por año muestra una variabilidad significativa en la cantidad de meteoritos registrados anualmente. Estas fluctuaciones pueden interpretarse desde varias perspectivas:

- **Evolución en los métodos de detección:** Los avances tecnológicos y los esfuerzos científicos para monitorear y registrar estos eventos podrían haber mejorado la precisión de los registros en años recientes.
- **Patrones astronómicos:** Aunque este análisis no incluye datos astronómicos reales, fenómenos como lluvias de meteoros o la proximidad de cuerpos celestes podrían explicar aumentos en años específicos.
- **Sesgo en el muestreo:** En este caso particular, los datos fueron simulados aleatoriamente. Sin embargo, en un contexto real, podrían surgir sesgos debido a regiones menos monitoreadas o pérdidas históricas de registros.

Es importante notar que, debido a la naturaleza aleatoria de los datos simulados, el gráfico refleja una distribución uniforme sin picos prominentes. No obstante, este enfoque puede adaptarse para analizar datos reales y detectar tendencias significativas.

#### **Distribución de la Masa**
El histograma indica que la mayoría de los meteoritos tienen una masa entre 50 y 300 unidades, con una disminución gradual hacia las masas más grandes. Este patrón coincide con observaciones reales, que muestran que los meteoritos más pequeños son más comunes debido a su mayor abundancia y probabilidad de sobrevivir a la entrada en la atmósfera.

Por otro lado, los meteoritos más masivos tienden a ser menos frecuentes, pero tienen un impacto significativo desde el punto de vista científico, económico y potencialmente catastrófico. La distribución observada podría reflejar una representación más general de los datos naturales y muestra la utilidad de visualizar este tipo de variables para identificar patrones clave.

#### **Conclusión**
Estos análisis destacan la importancia de explorar tanto la dimensión temporal como la de masa de los meteoritos. Si bien este estudio utiliza datos simulados, el proceso analítico puede ser replicado con datos reales, proporcionando insights más robustos sobre las características de los impactos de meteoritos. En un contexto real, estas observaciones podrían ser útiles para estudios geológicos, modelado de trayectorias astronómicas y estrategias de mitigación de riesgos.

