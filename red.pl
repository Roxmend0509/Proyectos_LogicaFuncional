%animal
puede('animal','respirar').

%Ave
es_un('ave','animal').
puede('ave','respirar').
vuela('ave','bien').
tiene('ave','plumas').
pone('ave','huevos').

%Avestruz
es_un('avestruz','ave').
patas('avestruz','largas').
vuela('avestruz','no_puede').
pone('avestruz','huevos').
tiene('avestruz','plumas').
es_un('avestruz','animal').
puede('avestruz','respirar').

%Albatros
es_un('albatros','ave').
vuela('albatros','muy_bien').
tiene('albatros','plumas').
pone('albatros','huevos').
es_un('albatros','animal').
puede('albatros','respirar').


%mamifero
es_un('mamifero','animal').
puede('mamifero','respirar').
tiene('mamifero','pelo').
da('mamifero','leche').

%tigre
es_un('tigre','mamifero').
come('tigre','carne').
es_un('tigre','animal').
puede('tigre','respirar').
tiene('tigre','pelo').
da('tigre','leche).

%ballena
es_un('ballena','mamifero').
vive_en('ballena','mar').
tiene('ballena','piel').
es_un('ballena','animal').
puede('ballena','respirar').
da('ballena','leche').

