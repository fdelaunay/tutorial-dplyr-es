## ------------------------------------------------------------------------
??dplyr

## ------------------------------------------------------------------------
devtools::install_github("splatsh/tarjetasblack")
library(tarjetasblack)

## ------------------------------------------------------------------------
str(movimientos)

## ------------------------------------------------------------------------
str(miembros)

## ------------------------------------------------------------------------
# data frame
head(miembros)
##    funcion                                 nombre    organizacion
## 1 consejal         Alberto Recarte García Andrade Partido Popular
## 2 consejal               Alejandro Couceiro Ojeda            CEIM
## 3 consejal Ángel Eugenio Gómez del Pulgar Perales            PSOE
## 4 consejal                Angel Rizaldos González Izquierda Unida
## 5 consejal                 Antonio Cámara Eguinoa Partido Popular
## 6 consejal  Antonio Rey de Viñas Sánchez-Majestad           CC OO

# convertimos a la clase "tbl"
miembros <- tbl_df(miembros)

# los objetos "tbl" son mas facil de visualisar en la consola:
miembros
## Source: local data frame [83 x 3]
## 
##     funcion                                 nombre    organizacion
## 1  consejal         Alberto Recarte García Andrade Partido Popular
## 2  consejal               Alejandro Couceiro Ojeda            CEIM
## 3  consejal Ángel Eugenio Gómez del Pulgar Perales            PSOE
## 4  consejal                Angel Rizaldos González Izquierda Unida
## 5  consejal                 Antonio Cámara Eguinoa Partido Popular
## 6  consejal  Antonio Rey de Viñas Sánchez-Majestad           CC OO
## 7  consejal                  Antonio Romero Lázaro            PSOE
## 8  consejal          Arturo Luis Fernández Álvarez            CEIM
## 9  consejal              Beltrán Gutiérrez Moliner Partido Popular
## 10 consejal                 Cándido Cerón Escudero Partido Popular
## ..      ...                                    ...             ...

glimpse(miembros) # parecido a str()
## Observations: 83
## Variables:
## $ funcion      (fctr) consejal, consejal, consejal, consejal, consejal...
## $ nombre       (fctr) Alberto Recarte García Andrade, Alejandro Coucei...
## $ organizacion (fctr) Partido Popular, CEIM, PSOE, Izquierda Unida, Pa...

## ------------------------------------------------------------------------
# todas las columnas menos 'funcion'
select(miembros, -funcion)
# las columnas entre 'nombre' y 'fecha'
select(movimientos, nombre:fecha)
# las columns con 'om'
select(movimientos, contains("om"))
# las columnas que empiezan por 'nom'
select(movimientos, starts_with("nom"))
# las columnas que respectan una expresión regular
select(movimientos, matches("?uto"))

## ------------------------------------------------------------------------
# guardamos esta versión simplifacada de 'movimientos' renombrando las columnas
mov <- select(movimientos, nom = nombre, imp =  importe, act = actividad)

## ------------------------------------------------------------------------
filter(miembros, organizacion %in% c("PSOE", "Partido Popular"))
## Source: local data frame [42 x 3]
## 
##     funcion                                 nombre    organizacion
## 1  consejal         Alberto Recarte García Andrade Partido Popular
## 2  consejal Ángel Eugenio Gómez del Pulgar Perales            PSOE
## 3  consejal                 Antonio Cámara Eguinoa Partido Popular
## 4  consejal                  Antonio Romero Lázaro            PSOE
## 5  consejal              Beltrán Gutiérrez Moliner Partido Popular
## 6  consejal                 Cándido Cerón Escudero Partido Popular
## 7  consejal    Rafael Darío Fernández Yruegas Moro Partido Popular
## 8  consejal   Estanislao Rodríguez-Ponga Salamanca Partido Popular
## 9  consejal                 Fernando Serrano Antón Partido Popular
## 10 consejal            Francisco José Moure Bourio Partido Popular
## ..      ...                                    ...             ...
filter(miembros, grepl("Antonio", nombre))
## Source: local data frame [4 x 3]
## 
##    funcion                                nombre    organizacion
## 1 consejal                Antonio Cámara Eguinoa Partido Popular
## 2 consejal Antonio Rey de Viñas Sánchez-Majestad           CC OO
## 3 consejal                 Antonio Romero Lázaro            PSOE
## 4 consejal             José Antonio Moral Santín Izquierda Unida
filter(movimientos, importe > 10000)
## Source: local data table [10 x 8]
## 
##                                  nombre      fecha hora minuto  importe
## 1  Ricardo Romero de Tejada y Picatoste 2007-11-26   10     59 11930.00
## 2         Ildefonso José Sánchez Barcoj 2006-02-15    9     13 11000.00
## 3         Ildefonso José Sánchez Barcoj 2009-12-31    2     40 16921.76
## 4              Miguel Blesa de la Parra 2006-04-05   16     51 12597.27
## 5              Miguel Blesa de la Parra 2006-07-20   14     50 13148.30
## 6                  Ramón Ferraz Ricarte 2007-12-20   14      9 13549.00
## 7                      Matías Amat Roca 2006-12-27   17      6 15000.00
## 8                      Matías Amat Roca 2008-11-12   12     16 10400.00
## 9          Enrique de la Torre Martínez 2007-11-29    9     57 12000.00
## 10         Enrique de la Torre Martínez 2008-03-07   16      1 11075.00
## Variables not shown: comercio (chr), actividad_completa (chr), actividad
##   (fctr)
filter(movimientos, importe > 10000 & hora < 4)
## Source: local data table [1 x 8]
## 
##                          nombre      fecha hora minuto  importe
## 1 Ildefonso José Sánchez Barcoj 2009-12-31    2     40 16921.76
## Variables not shown: comercio (chr), actividad_completa (chr), actividad
##   (fctr)

## ------------------------------------------------------------------------
slice(miembros, 50:55)

## ------------------------------------------------------------------------
arrange(miembros, desc(organizacion), nombre)

## ------------------------------------------------------------------------
top_n(mov, 2, imp)
## Source: local data table [2 x 3]
## 
##                             nom      imp           act
## 1 Ildefonso José Sánchez Barcoj 16921.76 COMPRA BIENES
## 2              Matías Amat Roca 15000.00         HOGAR
top_n(miembros, 1) # por defecto, ordena por la ultima columna
## Selecting by organizacion
## Source: local data frame [4 x 3]
## 
##    funcion                       nombre organizacion
## 1 consejal Rafael Eduardo Torres Posada          UGT
## 2 consejal       Gonzalo Martín Pascual          UGT
## 3 consejal José Ricardo Martínez Castro          UGT
## 4 consejal     Miguel Ángel Abejón Resa          UGT

## ------------------------------------------------------------------------
summarise(movimientos, max(importe))
## Source: local data table [1 x 1]
## 
##   max(importe)
## 1     16921.76
summarise(group_by(mov, nom), max_personal = max(imp))
## Source: local data table [83 x 2]
## 
##                                       nom max_personal
## 1          Alberto Recarte García Andrade      3509.20
## 2                Alejandro Couceiro Ojeda      1150.00
## 3  Ángel Eugenio Gómez del Pulgar Perales      4906.00
## 4                 Angel Rizaldos González       843.00
## 5                  Antonio Cámara Eguinoa      2742.00
## 6   Antonio Rey de Viñas Sánchez-Majestad      1751.08
## 7                   Antonio Romero Lázaro      4500.00
## 8           Arturo Luis Fernández Álvarez      2550.00
## 9               Beltrán Gutiérrez Moliner      2439.50
## 10                 Cándido Cerón Escudero      2800.00
## ..                                    ...          ...
summarise(group_by(miembros, organizacion), n())
## Source: local data frame [11 x 2]
## 
##           organizacion n()
## 1                       19
## 2                CC OO   6
## 3                 CEIM   3
## 4                 CEOE   1
## 5  Comisión de Control   1
## 6     Conf. de Cuadros   1
## 7      Izquierda Unida   5
## 8      Partido Popular  27
## 9   Patronal (Unipyme)   1
## 10                PSOE  15
## 11                 UGT   4

## ------------------------------------------------------------------------
mutate(mov, total = sum(imp))
## Source: local data table [77,202 x 4]
## 
##                               nom    imp           act    total
## 1  Alberto Recarte García Andrade  38.70          ROPA 11806773
## 2  Alberto Recarte García Andrade  14.60         HOTEL 11806773
## 3  Alberto Recarte García Andrade  95.62   RESTAURANTE 11806773
## 4  Alberto Recarte García Andrade  49.13         COCHE 11806773
## 5  Alberto Recarte García Andrade  13.94         COCHE 11806773
## 6  Alberto Recarte García Andrade  80.00         HOTEL 11806773
## 7  Alberto Recarte García Andrade  53.37         COCHE 11806773
## 8  Alberto Recarte García Andrade  42.00 COMPRA BIENES 11806773
## 9  Alberto Recarte García Andrade 263.30   RESTAURANTE 11806773
## 10 Alberto Recarte García Andrade   2.10         COCHE 11806773
## ..                            ...    ...           ...      ...
mutate(group_by(mov, nom), total_personal = sum(imp), pp = imp/total_personal)
## Source: local data table [77,202 x 5]
## 
##                               nom    imp           act total_personal
## 1  Alberto Recarte García Andrade  38.70          ROPA         136504
## 2  Alberto Recarte García Andrade  14.60         HOTEL         136504
## 3  Alberto Recarte García Andrade  95.62   RESTAURANTE         136504
## 4  Alberto Recarte García Andrade  49.13         COCHE         136504
## 5  Alberto Recarte García Andrade  13.94         COCHE         136504
## 6  Alberto Recarte García Andrade  80.00         HOTEL         136504
## 7  Alberto Recarte García Andrade  53.37         COCHE         136504
## 8  Alberto Recarte García Andrade  42.00 COMPRA BIENES         136504
## 9  Alberto Recarte García Andrade 263.30   RESTAURANTE         136504
## 10 Alberto Recarte García Andrade   2.10         COCHE         136504
## ..                            ...    ...           ...            ...
## Variables not shown: pp (dbl)

## ------------------------------------------------------------------------
top_n(
  arrange(
   summarize(
      group_by(
          filter(movimientos, importe > 0)
          , nombre)
        , total = sum(importe)
      )
    , desc(total)
    )
  , 10
  )

## ------------------------------------------------------------------------
# top 10 miembros con más gastos
movimientos %>%
  group_by(nombre) %>%
  summarize(total = sum(importe)) %>%
  arrange(desc(total)) %>%
  top_n(10)

## ------------------------------------------------------------------------
movimientos %>% 
  group_by(nombre) %>%
  summarize(gasto_max = max(importe))

## ------------------------------------------------------------------------
res <- movimientos %>%
  group_by(hora) %>%
  summarise(total = sum(importe))

library(ggplot2)
ggplot(res, aes(x=hora, y=total))+geom_bar(stat="identity")

## ------------------------------------------------------------------------
res <- movimientos %>%
  group_by(actividad) %>%
  summarise(n = n()) %>%
  top_n(10)
## Selecting by n

res$actividad <- reorder(res$actividad, res$n)

ggplot(arrange(res, n), aes(x=actividad, y=n)) +
  geom_bar(stat="identity") + 
  coord_flip()

## ------------------------------------------------------------------------
movimientos %>%
  filter(actividad == "RESTAURANTE") %>%
  group_by(nombre) %>%
  summarise(total_gastro = sum(importe)) %>%
  top_n(1)
## Selecting by total_gastro
## Source: local data table [1 x 2]
## 
##                              nombre total_gastro
## 1 Guillermo Ricardo Marcos Guerrero     114908.8

## ------------------------------------------------------------------------
# los 10 miembros con más gastos
despilfarradores <- movimientos %>%
  group_by(nombre) %>%
  summarize(total = sum(importe)) %>%
  arrange(desc(total)) %>%
  top_n(10)
## Selecting by total

left_join(despilfarradores, movimientos) %>%
  group_by(nombre, actividad_completa) %>%
  summarise(total_actividad = sum(importe)) %>%
  top_n(1)
## Joining by: "nombre"
## Selecting by total_actividad
## Source: local data table [10 x 3]
## Groups: nombre
## 
##                               nombre
## 1       Enrique de la Torre Martínez
## 2      Ildefonso José Sánchez Barcoj
## 3          José Antonio Moral Santín
## 4       Juan Manuel Astorqui Portera
## 5  Maria Mercedes de la Merced Monge
## 6               Mariano Pérez Claver
## 7                   Matías Amat Roca
## 8           Miguel Blesa de la Parra
## 9               Ramón Ferraz Ricarte
## 10           Ricardo Morado Iglesias
## Variables not shown: actividad_completa (chr), total_actividad (dbl)

## ------------------------------------------------------------------------
all <- left_join(tbl_df(movimientos), miembros, by="nombre")

res <- all %>% filter(!is.na(actividad) & actividad != '' & organizacion %in% c("Izquierda Unida", "Partido Popular", "PSOE")) %>%
  group_by(organizacion, actividad) %>%
  summarise(total = sum(importe))

ggplot(res, aes(x=actividad, y=total, fill=organizacion)) +
  geom_bar(stat="identity", position = "fill") + 
  coord_flip()

## ------------------------------------------------------------------------
res <- res %>%
  filter(total > 50000) %>%
  group_by(organizacion) %>%
  mutate(total_partido = sum(total))

#to to: normalisar
ggplot(res, aes(x=actividad, y=total/total_partido, fill=organizacion)) +
  geom_bar(stat="identity") + 
  coord_flip()

## ------------------------------------------------------------------------
res <- all %>% filter(!is.na(actividad)) %>%
  group_by(funcion, actividad) %>%
  summarise(total = sum(importe)) %>%
  arrange(desc(total))

ggplot(res, aes(x=actividad, y=total, fill=funcion)) +
  geom_bar(stat="identity", position = "fill") + 
  coord_flip()

## ------------------------------------------------------------------------
despilfarradores <- movimientos %>%
  group_by(nombre) %>%
  mutate(total = sum(importe)) %>%
  filter(dense_rank(-total) < 10)

res <- ungroup(despilfarradores) %>%
  group_by(nombre, dia = strftime(fecha, format = "%w-%a")) %>%
  summarise(gasto = sum(importe)) 
summary(res)

## ------------------------------------------------------------------------
myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space="Lab")

ggplot(data = res, aes(x = nombre, y = dia, fill = gasto)) +
  geom_tile() +
  scale_fill_gradientn(colours = myPalette(100)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ------------------------------------------------------------------------
modelos <- all %>% 
            filter(organizacion %in% c("Izquierda Unida", "Partido Popular", "PSOE")) %>%
            mutate(hora_num = hora+minuto/60) %>%
            group_by(organizacion) %>%
            do(mod = lm(importe ~ hora_num, data = .))
modelos

## ------------------------------------------------------------------------
# extraimos los coeficientes
modelos %>%
  rowwise %>%
  do(data.frame(
     grupo = .[[1]],
     var = names(coef(.$mod)),
     coef(summary(.$mod))
  ))

## ------------------------------------------------------------------------
# añadimos un campo "datetime"
all$t <- as.POSIXct(as.numeric(all$fecha) + all$hora*60*60 + all$minuto*60, origin="1970-01-01")

all %>% arrange(t) %>%
  filter(as.numeric(t -lag(t)) < 5*60, nombre != lag(nombre))

## ------------------------------------------------------------------------
library(microbenchmark)
options(digits = 3, microbenchmark.unit = "ms")

# reset
rm(movimientos)
library(tarjetasblack)
movimientos_df <- tbl_df(movimientos)
movimientos_dt <- tbl_dt(movimientos)

bch1 <- microbenchmark(
  base =     tapply(movimientos_df$importe, movimientos_df$nombre, FUN = mean),
  dplyr_df = movimientos_df %>% group_by(nombre) %>% summarise(a = mean(importe)),
  dplyr_dt = movimientos_dt %>% group_by(nombre) %>% summarise(a = mean(importe)),
  dt_raw =   movimientos_dt[, list(a = mean(importe)), by = nombre],
  times = 5
)
autoplot(bch1)

## ------------------------------------------------------------------------
res1 <- microbenchmark(
  base =    movimientos_df[ave(movimientos_df$importe, movimientos_df$nombre, FUN = max) == movimientos_df$importe,],
  dplyr_df = movimientos_df %>% group_by(nombre) %>% filter(importe == max(importe)),
  dplyr_dt = movimientos_dt %>% group_by(nombre) %>% filter(importe == max(importe)),
  times = 5
)
plot(res1)

## ------------------------------------------------------------------------
res <- microbenchmark(
  a = movimientos %>% select(actividad_completa) %>% arrange(actividad_completa) %>% distinct(),
  b = movimientos %>% select(actividad_completa) %>% distinct() %>% arrange(actividad_completa)
  , times = 10)
res
autoplot(res)

## ------------------------------------------------------------------------
n <- 1e+7
factores <- c("A", "B", "C", "D", "E", "F")

big <- data.frame(factores = as.factor(sample(factores, n, replace = TRUE))
                  , percent = round(runif(n, min = 0, max = 1), digits = 2)
                  )

str(big)
print(object.size(big), units = "GB")

big.dt <- tbl_dt(big)

plot(microbenchmark(
  #plyr = ddply(big, .(factores), summarise, total = sum(percent)) ,
  dplyr = big %>% group_by(factores) %>% summarise(total = sum(percent)),
  data.table = big.dt[, list(total = sum(percent)), by = factores],
  dplyr.dt = big.dt %>% group_by(factores) %>% summarise(total = sum(percent)),
  times = 10))

## ------------------------------------------------------------------------
require(data.table)
N=2e7; K=100
set.seed(1)
DT <- data.table(
  id1 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id2 = sample(sprintf("id%03d",1:K), N, TRUE),      # large groups (char)
  id3 = sample(sprintf("id%010d",1:(N/K)), N, TRUE), # small groups (char)
  id4 = sample(K, N, TRUE),                          # large groups (int)
  id5 = sample(K, N, TRUE),                          # large groups (int)
  id6 = sample(N/K, N, TRUE),                        # small groups (int)
  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
  v2 =  sample(5, N, TRUE),                          # int in range [1,5]
  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
)


DF <- as.data.frame(DT)
tbl_dt <- tbl_dt(DT)
library(microbenchmark)
library(dplyr)
library(ggplot2)

res <- microbenchmark(
  dt = DT[, sum(v1), keyby=id1],
  dplyr = tbl_dt %>% group_by(id1) %>% summarise(sum(v1)),
  base = tapply(DF$v1, DF$id1, FUN = sum)
  , times = 10, unit = "ms")
res

## ------------------------------------------------------------------------
autoplot(res) + expand_limits(x = 0)

## ------------------------------------------------------------------------
library(xts)
library(dygraphs)
ts <- movimientos_dt %>% group_by(fecha) %>% summarise(total=sum(importe))
ts <- xts(ts$total, order.by=ts$fecha)

dygraph(ts) %>% 
  dyRoller(rollPeriod = 30) %>%
  dyRangeSelector()

