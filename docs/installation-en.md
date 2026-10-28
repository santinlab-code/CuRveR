# Installing CuRveR 2.0

This guide explains how to install CuRveR in RStudio. You can open it as a text file in RStudio or preview it in MacDown.

> **Important:** CuRveR 2.0 is available on the `feature/modern-web-app` branch. The `main` branch still contains the old version.

## Case 1 — CuRveR has never been installed

### 1. Install R and RStudio

Install these applications in this order:

1. **R** from [CRAN](https://cran.r-project.org/). A recent version is recommended.
2. **RStudio Desktop** from [Posit](https://posit.co/download/rstudio-desktop/).

### 2. Install CuRveR 2.0

Open RStudio and copy the following code into the **Console**:

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

Wait for installation to finish. Downloading and installing dependencies may take several minutes.

### 3. Launch the application

Run this code in the Console:

```r
options(shiny.launch.browser = TRUE)
CuRveR::run_curver()
```

The application should open in your browser, with **CuRveR 2.0** displayed at the top.

## Case 2 — An older version of CuRveR is already installed

### 1. Stop the application and restart R

In RStudio, choose **Session → Restart R**.

This unloads the old version before replacing it.

### 2. Uninstall the old package

Run this code in the Console:

```r
for (lib in .libPaths()) {
  if ("CuRveR" %in% rownames(installed.packages(lib.loc = lib))) {
    remove.packages("CuRveR", lib = lib)
  }
}
```

This command removes the installed package. It does not delete your Excel files or previously saved results.

### 3. Install CuRveR 2.0

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

Wait for installation to finish.

### 4. Launch the application

```r
options(shiny.launch.browser = TRUE)
CuRveR::run_curver()
```

Check that the application header displays **CuRveR 2.0**.

## Subsequent use

You do not need to reinstall the package each time. Open RStudio and run:

```r
options(shiny.launch.browser = TRUE)
CuRveR::run_curver()
```

**Keep RStudio open while using the application.**

To stop the application, click **Stop** in the RStudio Console.

## Troubleshooting

If installation fails, keep the complete error message and resolve the problem before launching the application.

If the GitHub repository is private, the person installing CuRveR must have authorized access to it.

## Reading this guide

- **MacDown:** open `installation-en.md` to preview the formatted Markdown document.
- **RStudio:** choose **File → Open File…** to open the file. Copy only the R code blocks into the Console, without the lines containing the three backticks.

A `.md` file is a Markdown document: do not execute it directly with `source()` in R.
