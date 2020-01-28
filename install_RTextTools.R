install.packages("devtools")
library(devtools)
install_version("RTextTools",version="1.4.2")
#cette derniere commande va vous installer Rtools : c'est normal si sur la console de R 
#vous aurez un message d'erreur. Apres l'installation de maxent, vous aurez besoin de la relancer.
install_version("maxent",version="1.3.3.1")
install_version("RTextTools",version="1.4.2")
library(RTextTools)
library(tm)
