# Installer CuRveR 2.0

Ce guide décrit l’installation dans RStudio. Il peut être ouvert comme un fichier texte dans RStudio ou affiché dans MacDown.

> **Important :** CuRveR 2.0 est disponible sur la branche `feature/modern-web-app`. La branche `main` contient encore l’ancienne version.

## Cas 1 — CuRveR n’a jamais été installé

### 1. Installer R et RStudio

Installer, dans cet ordre :

1. **R** depuis [CRAN](https://cran.r-project.org/). Une version récente est recommandée.
2. **RStudio Desktop** depuis [Posit](https://posit.co/download/rstudio-desktop/).

### 2. Installer CuRveR 2.0

Ouvrir RStudio et copier le code suivant dans la **Console** :

```r
install.packages(
  "remotes",
  repos = "https://cloud.r-project.org"
)

remotes::install_github(
  "santinlab-code/CuRveR",
  ref = "feature/modern-web-app",
  dependencies = NA,
  upgrade = "never",
  force = TRUE
)
```

Attendre la fin de l’installation. Le téléchargement et l’installation des dépendances peuvent prendre plusieurs minutes.

### 3. Lancer l’application

Exécuter dans la Console :

```r
options(shiny.launch.browser = TRUE)
CuRveR::run_curver()
```

L’application doit s’ouvrir dans le navigateur avec **CuRveR 2.0** affiché en haut.

## Cas 2 — L’ancienne version de CuRveR est déjà installée

### 1. Arrêter l’application et redémarrer R

Dans RStudio, choisir **Session → Restart R**.

Cela décharge l’ancienne version avant son remplacement.

### 2. Désinstaller l’ancien package

Exécuter dans la Console :

```r
for (lib in .libPaths()) {
  if ("CuRveR" %in% rownames(installed.packages(lib.loc = lib))) {
    remove.packages("CuRveR", lib = lib)
  }
}
```

Cette commande supprime le package installé. Elle ne supprime pas les fichiers Excel ni les résultats déjà enregistrés.

### 3. Installer CuRveR 2.0

```r
install.packages(
  "remotes",
  repos = "https://cloud.r-project.org"
)

remotes::install_github(
  "santinlab-code/CuRveR",
  ref = "feature/modern-web-app",
  dependencies = NA,
  upgrade = "never",
  force = TRUE
)
```

Attendre la fin de l’installation.

### 4. Lancer l’application

```r
options(shiny.launch.browser = TRUE)
CuRveR::run_curver()
```

Vérifier que le bandeau affiche **CuRveR 2.0**.

## Pour les utilisations suivantes

Il n’est pas nécessaire de réinstaller le package. Ouvrir RStudio et exécuter :

```r
options(shiny.launch.browser = TRUE)
CuRveR::run_curver()
```

**Garder RStudio ouvert pendant l’utilisation.**

Pour arrêter l’application, cliquer sur **Stop** dans la Console de RStudio.

## En cas de problème

Si l’installation échoue, conserver le message d’erreur complet et résoudre le problème avant de lancer l’application.

Si le dépôt GitHub est privé, la personne qui installe CuRveR doit disposer d’un accès autorisé à ce dépôt.

## Lire ce guide

- **MacDown :** ouvrir le fichier `installation-fr.md` pour afficher sa mise en page Markdown.
- **RStudio :** utiliser **File → Open File…** pour ouvrir le fichier. Copier uniquement les blocs de code R dans la Console, sans les lignes contenant les trois accents graves.

Un fichier `.md` est un document Markdown : il ne s’exécute pas directement avec `source()` dans R.
