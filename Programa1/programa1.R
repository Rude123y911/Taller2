library(dplyr)
library(ggplot2)
library(gapminder)
library(writexl)
library(readxl)

opcion_1 <- function(df){
  # 1. Implementar un programa con las siguientes opciones:
  
  # Exportar el conjunto de datos gapminder en formato “xlsx”. El 10 %
  #  de los valores de las columnas lifeEx, pop, y gdpPercap se debe
  #  reemplazar de forma aleatoria por valores no asignados NA.
  
  len = length(df$country)
  
  for (i in c("pop", "lifeExp", "gdpPercap")){
    
    r_list = sample(len, as.integer(len*0.1), replace = FALSE)
    
    df[r_list,i] = NA
  }
  
  write_xlsx(df, "gapminder.xlsx")
}

opcion_2 <- function(){
  # Importar el archivo gapminder en formato “xlsx”.

  return (df1 = read_excel("gapminder.xlsx"))
}

opcion_3 <- function(df){
  # Graficar el diagrama de dispersíon lifeEx vs pop.

  p <- df %>%
    ggplot(aes(x=lifeExp,y=log(pop), col=continent))+
    geom_point(alpha=0.4)
  print(p)
}

opcion_4 <- function(df){
  
  # Graficar el diagrama de dispersíon gdpPercap vs pop.
  
  p <- df %>%
    ggplot(aes(x=log(gdpPercap),y=log(pop), col=continent))+
    geom_point(alpha=0.4)
  print(p)
}

opcion_5 <- function(df){

  #  Graficar los diagramas de cajas discriminados por continentes desde 1990 a 2007.
  
  p <- df %>%
    filter(year>1989) %>%
    ggplot(aes(y=log(gdpPercap), col=continent))+
    geom_boxplot(alpha=0.4)
  print(p)
}

main <- function(){
  
  df = data.frame(gapminder)
  
  print(df)
  
  print("Bienvenido a mi programa!!1")
  
  while (TRUE){
  
    print("Que desea hacer?")
    
    writeLines("
  Opcion (1): Guardar el dataframe con valores NA.
  Opcion (2): Leer el dataframe.
  Opcion (3): Hacer la grafica de dispersion de la expectativa de vida y poblacion.
  Opcion (4): Hacer la grafica de dispersion del PIB contra poblacion.
  Opcion (5): Hacer la grafica de cajas del PIB por contienente desde 1990.
  Opcion (6): Salir.
               
  Escribir el numero de la opcion deseada.")
      
      opcion <- readline()
      
      switch (opcion,
              "1" = opcion_1(df),
              "2" = df <- opcion_2(),
              "3" = opcion_3(df),
              "4" = opcion_4(df),
              '5' = opcion_5(df),
              '6' = break
      )
  }
}
main()
