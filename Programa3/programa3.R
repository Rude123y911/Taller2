main <- function(){
  while(TRUE){
    cat("\nSeleccione una de las siguientes opciones:
      1. Graficar una funcion de densidad de una distribucion uniforme
      2. Graficar una funcion de densidad de una distribucion Bernoulli
      3. Graficar una funcion de densidad de una distribucion Poisson
      4. Graficar una funcion de densidad de una distribucion Exponencial
      5. Salir")
    valor = as.integer(readline("Seleccione una opcion: "))
    desicion = switch (valor,
      disUniforme(),
      disBernoulli(),
      disPoisson(),
      disExponencial(),
      break
  )
  }
}
disUniforme <- function(){
  Vec = c(0:10)
  mini <- as.integer(readline("Seleccione un valor minimo "))
  maxi <- as.integer(readline("Seleccione un valor maximo "))
  if (mini > maxi){
    mini = NULL
    maxi = NULL
    cat("Ha ingresado el minimo mayor que el maximo")
    disUniforme()
  }else{
  dis = dunif(x= Vec, min = mini, max = maxi)
  dis
  plot(Vec, dis, type="l", col="blue", ylab = "Probabilidad", main = "disUniforme")
  }
  }

disBernoulli <- function(){
  Vec1= c(1:50)
  ens <- as.integer(readline("Digite el numero de ensayos "))
  prob <-  as.integer(readline("Digite el porcentaje de exito "))
  cat("El numero de ensayos es ", ens)
  cat("\nEl porcentaje de exito es ", prob)
  b = dbinom(x = Vec1, size = ens, prob = (prob/100))
  plot(b, type="h",lwd="2", xlab="Numero de exitos", col="red", ylab = "Probabilidad", main = "disBernoulli")
}

disPoisson <- function(){
  Vec = c(1:200)
  nEventos = as.integer(readline("Digite el numero de eventos "))
  if (nEventos > 0) {
    cat("El numero de eventos es", nEventos)
    dis = dpois(Vec, lambda = nEventos)
    plot(dis, type="h", xlab= "Numero de eventos", lwd="2", col="red", ylab = "Probabilidad", main = "disPoisson")
  }else{
    nEventos = NULL
    cat("Numero de eventos debe ser mayor a 0")
    disPoisson()
  }
}

disExponencial <- function(){
  Vec = c(1:10)
  tEventos = as.integer(readline("Digite la tasa de ocurrencia de eventos "))
  cat("La tasa de ocurrencia de eventos es ", tEventos)
  dist = dexp(Vec, rate = tEventos)
  plot(Vec, dist, type="l", lwd="2", col="blue", ylab = "Probabilidad", main = "disExponencial")
}

main()


