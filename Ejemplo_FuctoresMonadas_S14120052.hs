{-
Lee una cadena la parsea a entero y regresa ese entero mas uno
-}

leeEntero =
do
putStr "Introduce un n"
line <- getLine 
x <- return (read line ::Int) 
return (x +1)    

{- Función auxiliar -} 

suman n  
| n==0 = 0 
| otherwise = n + (suman (n-1))       

{- Suma los primeros n numeros naturales, la n  es introuducida por el usuario -} 

sumaN = 
do 
putStr "Hasta que numero quieres sumar?" 
line <-getLine 
n <- return (read line ::Int) 
let suma = suman n 
return suma   

{- Regresa true en caso de que la palabra introducida sea  un palindroma -} 
esPalin = 
do  
putStr "Introduce la palabra a probar: " 
word <- getLine 
let es = (reverse word ) == word 
return es 
