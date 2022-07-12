####################################################################################
########### Script para acompañar y complementar curso de introducción a R #########
####################################################################################

# Limpiar espacio de trabajo
rm(list = ls())

# Cargar librerías que vamos a utilizar
library(tidyverse)

# Fija directorio de trabajo a carpeta en que esta el script:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##### Distintas funciones para cargar datos ####
# ejemplo1 <- haven::read_dta(file = file.path('inputs', 'ELSOC_Long_2016_2021_v1.00_R.dta'))

load(file.path('inputs', 'ELSOC_Long_2016_2021_v1.00_R.RData'))

# Cargar base de datos elsoc
load(url("https://dataverse.harvard.edu/api/access/datafile/6160173")) 

# Análisis exploratorio
class(elsoc_long_2016_2021)
dim(elsoc_long_2016_2021)
names(elsoc_long_2016_2021)
head(elsoc_long_2016_2021[1:6], n = 5)
#View(elsoc_long_2016_2021)

# Filtrar y seleccionar casos y variables relevantes


elsoc_long <- elsoc_long_2016_2021 %>% 
    # Filtrar por atrición y casos perdidos
    dplyr::filter(tipo_atricion == 1,
                  !c17 %in% c(-666, -777, -888, -999),
                  !c05_08 %in% c(-666, -777, -888, -999)) %>% 
    # Crear variables nuevas con etiquetas
    dplyr::mutate(
        ola = factor(ola, labels = c('2016', '2017', '2018', '2019', '2021')),
        sexo = factor(m0_sexo, labels = c('Hombre', 'Mujer')),
        # Generar variable de identificación con coalición política
        idcoal = factor(c17, 
                        labels = c('Chile Vamos','Nueva Mayoría','Frente Amplio', 'Otra','Ninguna')),
        # Generar variable de confianza en el presidente
        conf_presi = factor(c05_08,
                            labels = c('Nada', 'Poco', 'Algo', 'Bastante', 'Mucho'))) %>% 
    dplyr::select(idencuesta, m0_sexo, ola, m0_edad, idcoal, conf_presi)

# Estadíticas descriptivas básicas
mean(elsoc_long$m0_edad, na.rm = TRUE)
median(elsoc_long$m0_edad, na.rm = TRUE)
sd(elsoc_long$m0_edad, na.rm = TRUE)

elsoc_long %>%
    dplyr::group_by(ola) %>%
    dplyr::summarise(edad_promedio = mean(m0_edad, na.rm = TRUE))

# Tablas de frecuencia
table(elsoc_long$idcoal)
tabla1 <- table(elsoc_long$idcoal)
prop.table(tabla1)

# Tablas de frecuencia bivariada
(tabla2 <- table(elsoc_long$idcoal, elsoc_long$conf_presi))
prop.table(tabla2)
prop.table(tabla2, margin = 1)
prop.table(tabla2, margin = 2)


## Modelos de regresion

m1 <- lm(conf_presi %in% c('Bastante', 'Mucho') ~ 1 + m0_sexo + m0_edad + ola, 
         data = elsoc_long)

m2 <- glm(conf_presi %in% c('Bastante', 'Mucho') ~ 1 + m0_sexo + m0_edad + ola, 
          family = binomial(link='logit'),
          data = elsoc_long)

summary(m1)

summary(m2)

## Modelos de regresion
    
modelsummary::modelsummary(list(m1, m2),
                           estimate = "{estimate}{stars} ({std.error})",
                           statistic = NULL,
                           stars = c('*' = .05, '**' = .01, '***' = .001),
                           gof_map = c('nobs', 'adj.r.squared'))

# ggplot:

#Ejemplos de visualización de datos con ggplot

# ejemplo 1
tabla1 <- table(elsoc_long$idcoal)
g1 <- data.frame(prop.table(tabla1))
(ejemplo1 <- ggplot(g1, aes(y = Freq, x = Var1)) + 
    geom_col())

ejemplo1 + 
    theme_bw() +
    scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
    geom_text(aes(label = scales::percent(Freq, accuracy = .1)),
              vjust = -.5) +
    xlab('Identificación con coalición política') + 
    ylab('Frecuencia')


# ejemplo 2
ggplot(data = elsoc_long, 
       aes(x = m0_edad, color = idcoal)) + 
    geom_density()

# ejemplo 3
elsoc_long %>% 
    dplyr::filter(ola == '2019' & !is.na(idcoal)) %>% 
    ggplot(aes(y = m0_edad, x = idcoal)) + 
    geom_boxplot()

# ejemplo 4
elsoc_long %>% 
    dplyr::filter(ola == '2019' & !is.na(idcoal)) %>% 
    ggplot(aes(y = m0_edad, x = idcoal, fill = sexo)) + 
    geom_boxplot()
