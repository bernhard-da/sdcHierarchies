
sdcHierarchies
--------------

[![Travis build status](https://travis-ci.org/bernhard-da/sdcHierarchies.svg?branch=master)](https://travis-ci.org/bernhard-da/sdcHierarchies) [![Coverage status](https://codecov.io/gh/bernhard-da/sdcHierarchies/branch/master/graph/badge.svg)](https://codecov.io/github/bernhard-da/sdcHierarchies?branch=master) [![GitHub last commit](https://img.shields.io/github/last-commit/bernhard-da/sdcHierarchies.svg?logo=github)](https://github.com/bernhard-da/sdcHierarchies/commits/master) [![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/bernhard-da/sdcHierarchies.svg?logo=github)](https://github.com/bernhard-da/sdcHierarchies)

R-Package to (interactively) generate or modify (nested) hierarchies that are required as input for packages such as [**sdcTable**](https://cran.r-project.org/web/packages/sdcTable/index.html) or [**cellKey**](https://github.com/sdcTools/cellKey)

The package is not tested and will likely change a lot in future versions. Still, any feedback/pull requests are very much welcome!

### Installation

    install_github("bernhard-da/sdcHierarchies", build_vignette=TRUE, dependencies=TRUE)

### Usage

Please have a look at the package-vignette

    library(sdcHierarchies)
    sdcHier_vignette()

### Updates

#### `0.11.0`

-   `sdcHier_convert()` can now convert a hierarchy to the format tau-argus understands using `format='argus'`
-   `sdcHier_export()` writes hierarchies in specific formats to disk
-   `sdcHier_import()` gained the ability to return sdc hierarchies from `hrc`-files in tau-argus format as well as from code and json
-   `sdcHier_info()` now shows the children of a node (if existing)
-   `sdcHier()` gained the feature to export hierarchies as json-encoded strings and hrc-files suitable as input for tau-argus
-   started with vignette that can be started with `sdcHier_vignette()` \#\#\#\# `0.10.0`
-   implement *undo*-feature in `sdcHier()`

#### `0.9.0`

-   combine `dynHier()` and `fixedHier()` to `sdcHier()`

#### `0.8.0`

-   `sdcHier_convert()` gained an argument `path` to write the output to a file
-   new function `sdcHier_compute_fromList()` to create a hierarchy from a named list
-   invisibly return results to avoid duplicated printing
-   start with tabbed interface for shiny-apps

#### `0.7.0`

-   some bugfixes
-   removed argument `full_names` froms `sdcHier_compute()`
-   show code to generated hierarchies in `dynHier()` and `fixedHier()`

#### `0.6.0`

-   fixes for shiny-Apps `dynHier()` and `fixedHier()`
-   renamed `dim_by_position()` to `sdcHier_compute()`
-   `sdcHier_convert()` can output the required code to compute the hierarchy

#### `0.5.0`

-   `sdcHier_convert()` can output the json-code
-   fixes to documentation and shinyApps `dynHier()` and `fixedHier()`

#### `0.4.0`

-   Functions (`sdcHier_create()`, `sdcHier_add()`, `sdcHier_delete()`, `sdcHier_rename()`, `sdcHier_info()`, `sdcHier_nodenames()` and `sdcHier_convert()`) dealing with creation and updating of nested hierarchies and extracting information have been added
