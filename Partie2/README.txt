Bin�me :
-Gros J�r�my
-Drouard Lo�c

Objectif :
L'objectif du projet est de d�velopper un programme qui permet d'interpr�ter des commandes et d'�valuer des expressions arithm�tiques.

Ce qui a �t� accompli :
- Ajout d'autres op�rateurs :
	Op�rateurs unaires : sin / cos / acos
	Op�rateurs binaires : Division
- Ajout de nouvelles commandes :
	unsetAll : Vide le store
	load : Execute les commandes et expressions d'un fichier
	info : A partir d'une string, affiche son expression sans l'�valuer. Il ne doit pas y avoir d'espace dans l'expression.
- Remplacement des Maybe par Either

Bugs :
- Probl�me avec le parser, on ne peut plus avoir de variable commencant par une lettre s, c ou a � cause des op�rateurs sin, cos et acos. (voir ParseTest)