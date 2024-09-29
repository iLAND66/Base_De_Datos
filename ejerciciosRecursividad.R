# 1.- Suma de los dijitos
suma.digito <- function(n){
  if (n==0)
    return(0)
  else
    return(n %%10 +suma.digito(n%/%10))
}

suma_digitos <- 12345
resultado <- suma.digito(suma_digitos)
print(resultado)

# 2.- Potenciacion
Potencia <- function(base, exponente){
  if (exponente==0)
    return(1)
  else
    return(base * Potencia(base, exponente - 1 ))
}

resultado <- Potencia(3,4)
print(resultado)

# 3.- Numeros de caminos en una cuadricula


