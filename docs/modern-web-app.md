# CuRveR : application web moderne

## Local (Mac)

Installer R >= 4.1 et les outils de compilation correspondant au Mac (Xcode Command Line Tools si des dépendances doivent être compilées). Depuis la racine du checkout :

```r
install.packages(c("remotes", "testthat"))
remotes::install_local(".", dependencies = NA, upgrade = "never")
CuRveR::run_curver()
# Ancienne interface, conservée sans modification :
CuRveR::run_curver(legacy = TRUE)
```

L’interface s’ouvre dans le navigateur ; les calculs restent en R local. Le serveur ne nécessite ni compte ni API. CSV : virgule comme séparateur et point décimal ; TSV : tabulation. Pour Excel, choisir une feuille. Ajouter chaque signal explicitement ; les fichiers multiples peuvent alimenter plusieurs signaux. Temps et mesures doivent être numériques, finis, et le temps trié. Les temps Excel sont convertis en minutes écoulées depuis la première mesure de chaque feuille (les taux sont donc exprimés par minute). Les temps numériques CSV gardent leur unité d’origine.

Les conditions définissent des réplicats et des blancs ; elles peuvent être importées/exportées en YAML. Ajouter un signal réinitialise les conditions et fenêtres : préparer tous les signaux avant ces étapes. Les fenêtres se règlent par puits/signal ou globalement. Un ajustement demande au moins cinq temps distincts et un signal non constant. Les figures disposent de vues courbes, paramètres et annotations ; le zoom se fait par sélection puis activation du zoom. Les tableaux complets sont téléchargeables.

## Architecture et fidélité scientifique

- `web_analysis.R` : validation, adaptation du pipeline historique, graphiques indépendants de Shiny.
- `web_modules.R` : modules d’import/préparation, conditions et fenêtres.
- `web_app.R` : composition bslib, résultats, exports et cycle de vie du processus callr.
- `models.R`, `optimizer.R`, `fitness.R`, `evaluation.R`, `utils.R` : code scientifique historique inchangé.

Richard et l’évolution différentielle sont les méthodes opérationnelles proposées. Les fonctions de coût R existantes sont exposées ; contrairement à l’ancienne interface, la sélection est appliquée. Le modèle linéaire et Nelder–Mead historiques sont incomplets ; ils ne sont pas annoncés comme opérationnels. Leur code et l’interface historique restent accessibles.

Les blancs sont moyennés dans leurs fenêtres par condition et signal, puis soustraits aux réplicats. Sans blanc, les valeurs sont conservées. La graine fixe rend un run reproductible dans le même environnement. Les résultats sont invalidés visuellement lorsque les paramètres changent et leur export est bloqué jusqu’au recalcul. Le sélecteur EN/FR en haut à gauche change la langue sans réinitialiser les données ; l’anglais est sélectionné au lancement.

Une erreur ou annulation conserve les anciens résultats sans les présenter comme ceux du calcul interrompu.

L’exécution via `callr` utilise un processus R séparé, arrêté à l’annulation ou à la fermeture de session. L’indicateur affiche un état de calcul, pas un pourcentage artificiel. L’export YAML contient les conditions, fenêtres, coût, graine, version R et version du package. Ce fichier est un compte rendu ; seul le YAML des conditions dispose actuellement d’un import dédié.

## Tests

```r
testthat::test_local(".")
```

Les tests comparent la préparation historique, LAD/OLS avec blancs, paramètres, prédictions, métriques et fonctions numériques de référence. La référence `tests/testthat/fixtures/scientific.rds` provient du moteur inchangé au commit de base `33a71b3`, graine 42, R 4.5.0, GA 3.2.5. Ne pas la régénérer pour masquer une différence. Une tolérance numérique explicite est utilisée ; un changement de moteur ou de dépendance doit être examiné scientifiquement. Les tests ne certifient pas la validité biologique des formules historiques. Le wrapper historique `fit_data()` exige un identifiant explicite avec la version installée de tidyr ; le test d’équivalence lui fournit `well`.

Un test navigateur reproductible est fourni dans `tests/browser/modern.cjs` (Node, Playwright et Chromium nécessaires). Démarrer l’application sur le port 3839, puis `node tests/browser/modern.cjs`. Il vérifie import, conditions, calcul asynchrone et téléchargement CSV.

## Serveur

Pour un essai local :

```r
options(shiny.host = "127.0.0.1", shiny.port = 3839)
shiny::runApp("inst/myapp", launch.browser = FALSE)
```

Déployer le package et `inst/myapp` sur Shiny Server ou Posit Connect, avec HTTPS et authentification configurés par l’hébergeur si nécessaire. Prévoir WebSockets, limites d’upload, CPU/mémoire et limites de sessions : chaque analyse active crée un processus R. Les fichiers téléversés sont temporaires et propres à la session ; ne pas exposer le sélecteur de fichiers de l’interface historique sur un serveur partagé. Aucun serveur distant n’a été déployé et macOS n’a pas été exécuté dans cet environnement Linux.

Les versions des dépendances ne sont pas encore verrouillées pour toutes les plateformes : lancer les tests après installation ou mise à jour. Pour une publication scientifique, archiver les données sources, la configuration et les versions des dépendances.
