#Ejercicio 1: Suma de digitos
sumaDigito <- function(n){
  if (n == 0)
    return(0)
  else
    return(n %% 10 + sumaDigito(n %/% 10))
}

suma_digitos <- 4321
resultado <- sumaDigito(suma_digitos)
print(resultado)

#Ejercicio 2: Potenciacion
potencia <- function(base, exponente){
  if (exponente == 0)
    return(1)
  else
    return(base * potencia(base, exponente - 1 ))
}

resultado <- potencia(3,4)
print(resultado)

#Ejercicio 3: Numero de caminos en una cuadricula
caminos <- function(m, n){
  if(m == 0 || n == 0)
    return(1)
  return(caminos(m - 1, n) + caminos(m, n - 1))
}

caminos(2, 2)

#Ejercicio 4: Palindromo
esPalindromo <- function(palabra){
  palabraInvertida <- paste(rev(strsplit(palabra, NULL)[[1]]), collapse = "")
  if (palabra == palabraInvertida)
    return(T)
  return(F)
}

esPalindromo("hola")
esPalindromo("reconocer")

#Ejercicio 5: Mayor divisor comun
mcd <- function(a, b){
  if(b == 0)
    return(a)
  return(mcd(b, a%%b))
}

mcd(56, 98)


