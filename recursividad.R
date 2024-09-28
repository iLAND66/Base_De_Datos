#Recursividad
factorial <- funtion(n){
  if(n==0)
    return(1)
  return(n*factorial(n-1))
}

factorial(3)
factorial(0)

#Ejercicio: Calcular la sumatoria de k=1 hasta 3
sumatoria <- funtion(n){
  if(n==0)
    return(0)
  return(n+factorial(n-1))
}

sumatoria(21)

#Fibonacci
fibonacci <- funtion(n){
  if(n==1 | n==2)
    return(1)
  if(n==0)
    return(0)
  return(fibonacci(n-1) + fibonacci(n-2))
}

fibonacci(0)
fibonacci(1)
fibonacci(2)
fibonacci(6)
