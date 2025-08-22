--Funcion sucesora
sucesor::Int->Int
sucesor n=n+1

--Funcion predecesora
predecesor::Int->Int
predecesor n =n-1

--Suma con funcion sucesora
add::Int->Int->Int
add a 0 =a
add a b =add (sucesor a) (predecesor b)

--Multiplicacion como suma repetida
multiplicar::Int->Int->Int
multiplicar _ 0=0
multiplicar a b=add a (multiplicar a (predecesor b))

--Resta con funcion predecesora
restar::Int->Int->Int
restar a 0=a
restar a b=restar (predecesor a) (predecesor b)

--Division como restas sucesivas
dividir::Int->Int->Int
dividir a b
  | a < b     = 0
  | otherwise=sucesor (dividir (restar a b) b)

--Sucesor para reales
sucesorReal::Float->Float
sucesorReal x=x+1.0

--Suma para numeros reales
addReal::Float->Float->Float
addReal x y=x+y

main::IO ()
main=do
  putStrLn "==Pruebas con enteros=="
  putStrLn ("Suma 3+4= "++show(add 3 4))
  putStrLn ("Multiplicacion 3*4= "++show(multiplicar 3 4))
  putStrLn ("Resta 7-2= "++show(restar 7 2))
  putStrLn ("Division 10/2= "++show(dividir 10 2))
  putStrLn ("Division 8/4= "++show(dividir 8 4))

  putStrLn "==Pruebas con reales=="
  putStrLn ("Suma 6+3= "++show(addReal 6 3))
  putStrLn ("Sucesor real de 7= "++show(sucesorReal 7))
