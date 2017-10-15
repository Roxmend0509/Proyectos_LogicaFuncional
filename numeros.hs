numletra :: Int -> String
numletra num
	| num > 0 && num<16 = 
	let answers = ["uno", "dos", "tres", "cuatro", "cinco","seis","siete","ocho","nueve","dies","once","doce","trece","catorce","quince"]
	in answers!!(num-1)
	 | (num<20) = "DIECI"++numletra(num-10)
     | (num==20) = "VEINTE"
     | (num<30) = "VEINTI "++numletra(num-20)
     | (num==30) = "TREINTA"
     | (num==40) = "CUARENTA"
     | (num==50) = "CINCUENTA"
     | (num==60) = "SESENTA"
     | (num==70) = "SETENTA"
     | (num==80) = "OCHENTA"
     | (num==90) = "NOVENTA"
     | (num<100) = numletra((num `div` 10) * 10) ++ " Y " ++ numletra(num `mod` 10)
     | (num==100) = "CIEN"
     | (num <200) = "CIENTO " ++ numletra(num-100)
     | (or [num==200,num==300,num==400,num==600,num==800])= numletra((num `div` 100)) ++ "CIENTOS"
     | (num==500) = "QUINIENTOS"
     | (num==700) = "SETECIENTOS"
     | (num==900) = "NOVECIENTOS"
     | (num<1000) = numletra((num `div` 100)*100)++ " " ++ numletra(num `mod` 100)
     | (num==1000)="MIL"
     | (num<2000)= "MIL " ++ numletra(num `mod` 1000)
     | (num<1000000) = numletra(num `div` 1000) ++ " MIL " ++ numletra(num `mod` 1000)
     | (num==1000000)="UN MILLON"
     |(num>1000000) = "EL NUMERO DEBE SER MENOR O IGUAL A UN MILLON"