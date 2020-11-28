
numCompoments <- 10
dataset <- read_excel("C:/Users/User/Documents/Proyecto01/Revista_03/Base_RAcademico_Vacios.xlsx")
datosnuevos <- data.frame(dataset)
datos<-data.matrix(datosnuevos[2:19])
#FUNCION_OBJETIVO
funcion_objetivo <- function(x){
  vDegree <- x[1]
  if(vDegree==0){
    vDegree <- 0.00001
  }
  kergauss<-rbfdot(sigma = vDegree)
  K<-kernelMatrix(kergauss,datos)
  jd_kpls = plsreg1(K[,],datosnuevos[,20:20],comps = numCompoments)
  Q2 <- jd_kpls$Q2
  Res <- data.frame(Q2)
  v <- c() #Declaramos un vector vacio
  q <- length(Res) #Cantidad de columnas

  for (i in 1:numCompoments){
    for (j in 1:q){
      if(is.null(Q2[i,j])){
        Q2[i,j] <- 0
      }else {
        Q2[i,j] <- Q2[i,j]
      }
    }
  }

  for (i in 1:numCompoments){
    v[i] <- Q2[i,q]
  }

  s <- c() #Declaramos un vector vacio
  s[1] <- max(v)
  for (i in 1:numCompoments){
    if(s[1]==v[i]){
      s[2] <- i
    }
  }

  return(s[1])  # Q2cum -> retorna el valor maximo

}

##
funcion_objetivo_2 <- function(x){ #Para evaluacion final
  vDegree <- x[1]
  if(vDegree==0){
    vDegree <- 0.00001
  }
  kergauss<-rbfdot(sigma = vDegree)
  K<-kernelMatrix(kergauss,datos)
  jd_kpls = plsreg1(K[,],datosnuevos[,20:20],comps = numCompoments)
  Q2 <- jd_kpls$Q2
  Res <- data.frame(Q2)
  v <- c() #Declaramos un vector vacio
  q <- length(Res) #Cantidad de columnas

  for (i in 1:numCompoments){
    for (j in 1:q){
      if(is.null(Q2[i,j])){
        Q2[i,j] <- 0
      }else {
        Q2[i,j] <- Q2[i,j]
      }
    }
  }

  for (i in 1:numCompoments){
    v[i] <- Q2[i,q]
  }

  s <- c() #Declaramos un vector vacio
  s[1] <- max(v)
  for (i in 1:numCompoments){
    if(s[1]==v[i]){
      s[2] <- i
      s[3] <- vDegree
    }
  }

  return(s)  # retorna vector de valores

}



resultado <- matrix(0,30,6)
eF <- 0.75
for (iter in 1:30) {
  tinicial <- proc.time() # Inicia el cron?metro
  ## calculate the optimum solution using
  res.fun <- malschains(function(x) {-funcion_objetivo(x)}, lower=c(0), upper=c(10), maxEvals = 5000,
                         control = malschains.control(popsize = 50, ls = "cmaes", istep = 500, effort = eF))
  print(res.fun$sol)
  print(res.fun$fitness)
  result <- res.fun$sol
  tfinal <- proc.time()-tinicial    # Detiene el cron?metro
  ## calculate the optimum value using funcion_objetivo function
  optimum.value <- funcion_objetivo_2(result)

  ##########################################################################
  #Guardar resultados
  ##########################################################################
  resultado[iter,1]<-iter             #iter
  resultado[iter,2]<-optimum.value[1] #Q2cum maximo funcion 2
  resultado[iter,3]<-optimum.value[2] #Num componente
  resultado[iter,4]<-optimum.value[3] #degree
  resultado[iter,5]<-tfinal[3]        #Tiempo de computo
  resultado[iter,6]<-eF               #effort
  print(resultado)
}
Res <- data.frame(resultado)
write.csv2(Res,"C:/Users/User/Documents/Proyecto01/Revista_03/MMA_R3_075.csv")
