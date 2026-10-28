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

## Axes de l’aperçu

L’OD (noir, gauche) et le signal (rouge, droite) utilisent une transformation affine par puits : minimum OD aligné sur minimum signal et maximum OD aligné sur maximum signal. Chaque graphique affiche ses propres axes gradués ; les plages diffèrent entre puits. Les axes affichent les unités réelles ; les données et les calculs ne sont pas transformés. Les croisements dépendent de ces échelles d’affichage et ne constituent pas une égalité de mesures entre OD et fluorescence. Les séries constantes utilisent une plage élargie pour garder une transformation inversible.

## Interface simplifiée

La couleur principale est RGB(0, 40, 75), soit #00284B. La barre latérale ne contient plus de réglages de calcul ni de bouton de lancement. L’analyse se lance depuis Résultats, avec LAD et une graine reproductible de 42 ; ces valeurs sont conservées dans l’export de configuration. Les autres fonctions de coût restent disponibles dans le moteur R et l’interface historique.

## Conditions techniques par défaut

Après import, les puits sont pré-affectés par triplets de colonnes au sein de chaque ligne : A1–A3, A4–A6, A7–A9, A10–A12, puis B1–B3, etc. Les groupes restent modifiables et renommables. Les puits manquants créent un groupe partiel sans décaler les triplets suivants. Aucun blanc biologique n’est deviné ; il faut vérifier les affectations avant analyse. Le champ de nom d’une nouvelle condition est vide.

## Blanc commun

Dans Conditions, sélectionner une seule fois le triplet blanc dans le panneau latéral. Ce groupe est exclu des conditions à ajuster ; ses puits sont affectés comme blancs à toutes les conditions restantes. Le choix « Aucun » conserve les mesures sans correction. Le blanc reste dans la liste des groupes éditables, mais ne devient jamais une condition à ajuster. Son nom et ses puits suivent les modifications enregistrées. Les groupes incomplets restent sélectionnables si la plaque n’inclut pas leurs trois puits.

La correction conserve le calcul historique : pour chaque signal (OD et fluorescence séparément), soustraction de la moyenne des mesures des puits blancs dans leurs fenêtres sélectionnées. Il s’agit d’une moyenne globale dans ces fenêtres, pas d’une soustraction point par point dans le temps. Les fenêtres affichent les données corrigées ; l’analyse repart des mesures brutes et applique la correction une seule fois. Les données ajustées et exportées sont corrigées. Import et Aperçu restent des vues des mesures brutes. Modifier le choix du blanc invalide les anciens résultats et nécessite de relancer l’analyse.

L’export YAML des conditions (version 2) contient les groupes éditables et le nom du groupe blanc. Les anciens fichiers avec un blanc commun sont importés en reconstruisant un groupe Blank modifiable. Des blancs différents par condition donnent un message d’erreur explicite.

Les groupes préremplis sont des propositions : ils peuvent être renommés et contenir deux, trois, quatre puits ou davantage. Retirer un puits l’exclut du groupe ; ajouter un puits le déplace depuis son groupe précédent pour éviter les doublons. Enregistrer applique les changements. Supprimer le groupe blanc désactive la correction jusqu’à un nouveau choix.

Les fenêtres restent propres à chaque couple puits/signal. Le passage à un autre puits efface la sélection graphique précédente et recharge ses bornes enregistrées. Cocher la case applique immédiatement la fenêtre affichée à tous les puits et signaux.

La sélection graphique de Time Windows est enregistrée immédiatement dans la session, sans bouton Appliquer. La zone ombrée et les pointillés montrent l’intervalle enregistré lorsque l’on revient sur le puits/signal. Les champs numériques sont aussi enregistrés automatiquement après une courte pause de saisie. Le bouton Appliquer a été supprimé. Le partage global reste conditionné à la case dédiée ; les données et fenêtres ne sont pas conservées après fermeture de la session.

## Analyse et résultats

Le panneau affiche explicitement le modèle de Richard à quatre paramètres, l’évolution différentielle et LAD ; les autres modèles historiques incomplets ne sont pas proposés comme opérationnels. Le signal et la vue (courbes, comparaison des paramètres ou annotations) sont côte à côte. Le menu des conditions est une multisélection compacte. Les options de tracé sont dépliables et le graphique conserve une hauteur de 550 px.

Sous le graphique, la table récapitulative regroupe les paramètres et les indicateurs de qualité par signal/condition. Les données complètes sont parcourables par pages de 50, 100 ou 200 lignes, sans l’ancienne limite des 200 premières lignes. Les tables couvrent l’ensemble des résultats ; le choix du signal et des conditions filtre la figure. L’export CSV conserve toutes les lignes.

## Correction de l’ajustement (moteur scaled)

L’équation Richard historique à quatre paramètres est conservée. Son initialisation historique estimait toutefois les dérivées en multipliant par le pas temporel et regroupait les observations selon leur valeur plutôt que leur temps. Le passage des heures aux minutes pouvait donc rendre les bornes de vitesse très grandes et favoriser des sigmoïdes presque discontinues.

Le nouveau moteur ajuste temps et amplitude normalisés, calcule des bornes de pente à partir des médianes aux temps observés et conserve l’évolution différentielle avec le coût choisi (LAD dans l’interface). Un raffinement borné L-BFGS-B est retenu seulement s’il améliore ce même objectif. Les paramètres sont ensuite reconvertis dans les unités des données : p_max/p_min en unités du signal, r_max en unités du signal par unité de temps et s dans l’unité temporelle. Les observations ne sont pas lissées ou modifiées.

Les résultats numériques changent volontairement par rapport au moteur défectueux ; le mode interne `web_analyze(..., engine="legacy")` et les références gelées permettent de vérifier séparément la compatibilité historique. Les nouveaux tests vérifient l’ajustement de courbes connues croissantes/décroissantes et la cohérence lors de conversions heures/minutes ou de changements d’amplitude. Une sigmoïde unique ne décrit pas nécessairement une courbe biphasique ; consulter les résidus et indicateurs de qualité.

Les conditions sont des cases à cocher disposées en lignes. Les tableaux récapitulatif et complet sont dans deux sections dépliables indépendantes, fermées par défaut. Les définitions des quatre paramètres sont accessibles au survol ou au focus clavier des abréviations au-dessus de la figure ; les en-têtes correspondants du tableau reçoivent les mêmes info-bulles.

## Affichage des résultats et unités

Après analyse, l’OD choisie à l’import est sélectionnée en premier. Les axes temporels affichent « Time (in min) » ; les dates/heures Excel sont converties en minutes et les colonnes temporelles numériques doivent donc être fournies en minutes. Les paramètres de vitesse restent exprimés par unité temporelle des données.

Les courbes utilisent par défaut un cadrage Y à partir de zéro, sans modifier les valeurs. Les valeurs corrigées négatives restent dans l’ajustement, les tableaux et les exports ; un message indique combien de mesures sont hors du cadrage positif. L’option « Show negative corrected values » permet de les revoir. Une mesure inférieure à la moyenne du groupe blanc donne un signal corrigé négatif : vérifier le blanc plutôt que rectifier artificiellement les données. Les graphiques de paramètres gardent leurs valeurs signées, notamment r_max pour une décroissance. Les info-bulles des paramètres ont été retirées.
