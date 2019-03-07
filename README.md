
sdcHierarchies
--------------

[![Travis build status](https://travis-ci.org/bernhard-da/sdcHierarchies.svg?branch=master)](https://travis-ci.org/bernhard-da/sdcHierarchies) [![Coverage status](https://codecov.io/gh/bernhard-da/sdcHierarchies/branch/master/graph/badge.svg)](https://codecov.io/gh/bernhard-da/sdcHierarchies/branch/master) [![GitHub last commit](https://img.shields.io/github/last-commit/bernhard-da/sdcHierarchies.svg?logo=github)](https://github.com/bernhard-da/sdcHierarchies/commits/master) [![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/bernhard-da/sdcHierarchies.svg?logo=github)](https://github.com/bernhard-da/sdcHierarchies) [![Mentioned in Awesome Official Statistics](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software)

R-Package to (interactively) generate or modify (nested) hierarchies that are required as input for packages such as [**sdcTable**](https://CRAN.R-project.org/package=sdcTable) or [**cellKey**](https://github.com/sdcTools/cellKey).

The package is in its early stages and may change in future versions. Still, any feedback/pull requests are very much welcome!

### Installation

#### CRAN

The latest version from CRAN can be installed via

    install.pacakges("sdcHierarchies")

#### Github

The latest development-version can be installed directly via:

    install.packages("remotes")
    remotes::install_github(
      repo = "bernhard-da/sdcHierarchies",
      dependencies = TRUE,
      build_opts = c("--no-resave-data", "--no-manual")
    )

### Usage

Please have a look at the package vignette `sdcHierarchies::hier_vignette()` or by clicking [**here**](https://bernhard-da.github.io/sdcHierarchies/articles/usage.html) or browse the automatically generated [**documentation**](https://bernhard-da.github.io/sdcHierarchies/).

### Updates

Updates/Changes are listed [**here**](https://bernhard-da.github.io/sdcHierarchies/news/index.html).
