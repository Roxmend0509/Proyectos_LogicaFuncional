-- Distancia entre dos puntos --
dista (p1,q1) (p2,q2) = sqrt((p1-p2)^2+(q1-q2)^2)


--  Permutaci¢n c¡clica de una lista --
inve [] = []
inve lis = last lis : init lis


-- Formula de Her¢n --
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
	where s = (a+b+c)/2