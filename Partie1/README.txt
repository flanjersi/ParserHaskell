Bin�me :
-Gros J�r�my
-Drouard Lo�c

Objectif :
L'objectif du projet est de d�velopper un programme qui permet d'interpr�ter des commandes et d'�valuer des expressions arithm�tiques.

Ce qui a �t� accompli :
- module Expression : Fait
- Test des expressions : Fait
- module Parse : Fait
- Test du parser : Fait
- module EnvInteractif : Fait
- Test de l'environnement interactif : Pas fait, nos fonctions retournent toutes un IO Store donc pas tr�s utile de tester ce qu'elle renvoie.

Sources vers les codes qui nous ont inspir�s :
-Nous avons seulement regard� des exemples pour utiliser buildExpressionParser

Bugs :
-parseExpression "1.1.a" renvoie Just (Const 1.1) au lieu de Nothing (voir ParseTest)
-parseExpression "a.1" renvoie Just (Variable "a") au lieu de Nothing (voir ParseTest)