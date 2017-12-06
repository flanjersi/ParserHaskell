Binôme :
-Gros Jérémy
-Drouard Loïc

Objectif :
L'objectif du projet est de développer un programme qui permet d'interpréter des commandes et d'évaluer des expressions arithmétiques.

Ce qui a été accompli :
- module Expression : Fait
- Test des expressions : Fait
- module Parse : Fait
- module EnvInteractif : Fait

- Test du parser : Fait
- Test de l'environnement interactif : Fait

Sources vers les codes qui nous ont inspirés :
-Nous avons seulement regardé des exemples pour utiliser buildExpressionParser

Bugs :
-parseExpression "1.1.a" renvoie Just (Const 1.1) au lieu de Nothing (voir ParseTest)
-parseExpression "a.1" renvoie Just (Variable "a") au lieu de Nothing (voir ParseTest)