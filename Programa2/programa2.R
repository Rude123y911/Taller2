library(readr)
library(writexl)


mat <- matrix(data = rnorm(50, mean = 45, sd=9), nrow = 50, ncol = 1)
mat2 = matrix(data = rnorm(50, mean=87, sd=4), nrow = 50, ncol = 1)

write.csv(mat,"Experimento_a.csv", row.names=FALSE)
write.csv(mat2, "Experimento_b.csv", row.names = FALSE)

Experimento_a.csv <- read_csv("Experimento_a.csv", show_col_types = FALSE)
Experimento_b.csv <- read_csv("Experimento_b.csv", show_col_types = FALSE)
main <- function(){
  cat("Hola, buen día. ¿Cuál de las siguientes operaciones quieres ver? 
    1. Significancia entre las medias.
    2. Correlación de Pearson y Spearman de los datos.
    3. graficar el diagrama de dispersión y regresión lineal.
    Elije sólo el numero de la opción")
  opc <- readline(prompt="Ingrese el numero de su opción: ")
  if (opc == 1){
    Ttest=t.test(Experimento_a.csv$V1,Experimento_b.csv$V1,paired = TRUE) #Even we can assign  paired=TRUE using rquery statement.
    pValor=Ttest$p.value
    cat("P-valor es igual a: ", pValor)
    cat("\nEntre más bajo el p-valor hay más probabilidades que las medias vengan de distribuciones distintas. \nEntre más alto el p-valor quiere decir que las medias son similares y podrían venir de distribuciones parecidas.")
  }
  if (opc == 2){
    result = cor(Experimento_b.csv$V1, Experimento_a.csv$V1, method = "pearson")
    print(paste("Pearson correlation coefficient is:", result))
    result2 = cor(Experimento_b.csv$V1, Experimento_a.csv$V1, method = "spearman")
    print(paste("Spearman correlation coefficient is:", result2))
  }
  if(opc == 3){
    plot(Experimento_b.csv$V1, Experimento_a.csv$V1, pch = 19)
    scatter.smooth(x=Experimento_a.csv$V1,y=Experimento_b.csv$V1,main="Regresión lineal por
    mínimos cuadrados.")
  }
}
main()
