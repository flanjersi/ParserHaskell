Binôme :
-Gros Jérémy
-Drouard Loïc

Objectif :
L'objectif du projet est de développer un programme qui permet d'interpréter des commandes et d'évaluer des expressions arithmétiques.

Ce qui a été accompli :
- Ajout d'autres opérateurs :
	Opérateurs unaires : sin / cos / acos
	Opérateurs binaires : Division
- Ajout de nouvelles commandes :
	unsetAll : Vide le store
	load : Execute les commandes et expressions d'un fichier
	info : A partir d'une string, affiche son expression sans l'évaluer. Il ne doit pas y avoir d'espace dans l'expression.
- Remplacement des Maybe par Either

Bugs :
- Problème avec le parser, on ne peut plus avoir de variable commencant par une lettre s, c ou a à cause des opérateurs sin, cos et acos. (voir ParseTest)