tutorial-dplyr-es
=================

## objetivo 

`tutorial-dplyr-es` es un tutorial en español para:

 - aprender a usar **dplyr** (paquete *R*)
 - entender los verbos y los "pipes" (tubos)
 - comparar la velocidad con otros metodos
 
En apendice el lector encontrará como fueron creado los datos. 

## Programa

### 0. Entender como funcionan **dplyr** y las **data.table**s

Como lo presenta su autor, Hadley Wickham, **[dplyr](https://github.com/hadley/dplyr)** es la *nueva* iteración del paquete **plyr**, enfocado a las **data.frames**, con 3 objectivos:

* identificar cual son las manipulaciones más importantes para analisar datos y hacerlas faciles con R.

* escribir las partes-llaves en [C++](http://www.rcpp.org/) para manipular los datos en memoria muy rapidamente.

* usar las misma interace para trabajar donde sea los datos: data frame, data table o database.




### 1. Pillar y organisar los datos

Los datos que serviran de base al tutorial son los famosos gastos de la [tarjetas *black*](https://es.wikipedia.org/wiki/Caja_Madrid#Caso_de_las_Tarjetas_.22opacas.22) de *Caja Madrid*.


### 2. Analisar los datos gracias a **dplyr**

- Uso de **dplyr** para analisar los gastos. 
- Uso de **ggplot2** para graficar las estatisticas


> ### Nota
> 
> #### Formato
> - **.Rmd**: algunos programas estaran escrito por comandos de **R** encapsulados dentro de un texto Markdown. **[Rstudio](https://support.rstudio.com/hc/en-us/articles/200552086-Using-R-Markdown)** propone una muy buena interface para ejecutar/editar esto archivo. 
> - **.R** simple escripte **R**
>
> #### Estilo
>
> Se usa cuando posible [el estilo recomendado por Hadley Wickham](http://r-pkgs.had.co.nz/style.html).
