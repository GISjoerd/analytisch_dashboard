# Langdurigmonitoren Rietendaken Dashboard(Rietdekkersgilde)

In dit project is er een dashboard ontwikkeld voor het langdruig monitoren van rietendaken. Om de app te ontwikkelen is er Shiny in R gebruikt. De repository is georganiseerd in verschillende R scripten en Static files die samen de Shiny app vormen. Hieronder wordt de structuur van de app verder toegelicht. Wanneer u een account heeft kunt u het dashboard bekijken in de productie omgeving:
https://dilab.shinyapps.io/Dashboard_Rietdekkersgilde/
Wanneer u geen account heeft, dient u deze aan te vragen bij het DI-Lab op de HAS green academy. U kunt alleen een account aanvragen als u betrokken bent bij het project of door goedkeuring van na goedkeuring van de RietdekkersGilde. 

De bestanden die samen het dashboard vormen zijn te vinden in:

## bo_2324_23200379_rietdekkers/Dashboard/

### app.R
Hier worden ui.R en server.R opgeroepen.

### ui.R
In ui.R zijn alle visuele elementen gedefineerd.

### server.R
In server.R is de server logica van de app te vinden. De serverlogica is opgedeelt in verschillende bestanden, hier worden andere server bestanden in aangeroepen.
De bestanden zijn uitgesplitst per pagina: monitoring.R, analyse.R, informatie.R, introductie.R

### elements.R 
Hiernaast wordt er een elements.R gebruikt waar een thema en lijn kleuren worden gedefineerd.  
 
### javaScript.js 
Er wordt javascript gebruikt om style verandering van action buttons mogelijk te maken wanneer deze gebruikt worden.

## /WWW
In de WWW folder worden de static files gedefineerd. Hierstaan de bestanden in die niet veranderen wanneer de applicatie draaid. Hieronder vallen: 
Media bestanden
style.css


## /Python 
Hier zijn de Python scripts te vinden die gebruikt zijn binnen dit project. 
Dit zijn twee python scripts: 
register_new_device.py ---> wordt gebruikt voor het toevoegen van nieuwe fieldlabs en sensoren.
MeteoAPI.py ---> doormiddel van batch proccesing wordt dit script binnen een Azure omgeving iedere 15 minuten aangeroepen. (toegevoegd aan repo als backup)


## /SQL
Hier staan de SQL queries die gebruikt zijn om views te maken van de database. Deze view worden weer gebruikt om dataframes van te maken die gebruikt worden om data te visualiseren in het dashboard. 








 
