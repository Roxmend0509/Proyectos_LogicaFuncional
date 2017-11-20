

%Enrique7

hijo('Enrique8','Enrique7').
hijo('Arturo','Enrique7').
hija('Margarita','Enrique7').

nuera('Catalina','Enrique7').
nuera('Ana','Enrique7').
nuera('Juana','Enrique7').

nieta('Maria','Enrique7').
nieta('Isabel','Enrique7').
nieto('Eduardo','Enrique7').

%Enrique8

padre('Enrique7','Enrique8').

hermano('Arturo','Enrique8').
hermana('Margarita','Enrique8').

esposa('Catalina','Enrique8').
esposa('Ana','Enrique8').
esposa('Juana','Enrique8').

hija('Maria','Enrique8').
hija('Isabel','Enrique8').
hijo('Eduardo','Enrique8').


%Arturo

padre('Enrique7','Arturo').

hermano('Enrique8','Arturo').
hermana('Margarita','Arturo').

cuniada('Catalina'.'Arturo').
cuniada('Ana'.'Arturo').
cuniada('Juana'.'Arturo').

sobrina('Maria','Arturo').
sobrina('Isabel','Arturo').
sobrino('Eduardo','Arturo').

%Margarita

padre('Enrique7','Margarita').

hermano('Enrique8','Margarita').
hermano('Arturo','Margarita').

cuniada('Catalina'.'Margarita').
cuniada('Ana'.'Margarita').
cuniada('Juana'.'Margarita').

sobrina('Maria','Margarita').
sobrina('Isabel','Margarita').
sobrino('Eduardo','Margarita').

%Catalina

suegro('Enrique7','Catalina').

esposo('Enrique8','Catalina').

cuniado('Arturo','Catalina').
cuniada('Margarita','Catalina').

hija('Maria','Catalina').

hijastra('Isabel','Catalina').
hijastro('Eduardo','Catalina').

socia('Ana','Catalina').
socia('Juana','Catalina').

%Ana

suegro('Enrique7','Ana').

esposo('Enrique8','Ana').

cuniado('Arturo','Ana').
cuniada('Margarita','Ana').

hija('Isabel','Ana').

hijastra('Maria','Ana').
hijastro('Eduardo','Ana').

socia('Catalina','Ana').
socia('Juana','Ana').

%Juana

suegro('Enrique7','Juana').

esposo('Enrique8','Juana').

cuniado('Arturo','Juana').
cuniada('Margarita','Juana').

hijo('Eduardo','Juana').

hijastra('Isabel','Juana').
hijastra('Maria','Juana').

socia('Catalina','Juana').
socia('Ana','Juana').

%Maria

abuelo('Enrique7','Maria').

padre('Enrique8','Maria').
madre('Catalina','Maria').

tio('Arturo','Maria').
tia('Margarita','Maria').

hermana('Isabel','Maria').
hermano('Eduardo','Maria').

madrastra('Ana','Maria').
madrastra('Juana','Maria').

%Isabel

abuelo('Enrique7','Isabel').

padre('Enrique8','Isabel').
madre('Ana','Isabel').

tio('Arturo','Isabel').
tia('Margarita','Isabel').

hermana('Maria','Isabel').
hermano('Eduardo','Isabel').

madrastra('Catalina','Isabel').
madrastra('Juana','Isabel').

%Eduardo

abuelo('Enrique7','Eduardo').

padre('Enrique8','Eduardo').
madre('Juana','Eduardo').

tio('Arturo','Eduardo').
tia('Margarita','Eduardo').

hermana('Maria','Eduardo').
hermana('Isabel','Eduardo').

madrastra('Catalina','Eduardo').
madrastra('Ana','Eduardo').




