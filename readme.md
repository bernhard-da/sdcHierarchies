## sdcHierarchies

R-Package to (interactively) generate or modify (nested) hierarchies that are required as input for packages such as [**sdcTable**](https://cran.r-project.org/web/packages/sdcTable/index.html) or [**cellKey**](https://github.com/sdcTools/cellKey)

The package is not tested and will likely change a lot in future versions. Still, any feedback/pull requests are very much welcome!

### Installation
```
devtools::install_github("bernhard-da/sdcHierarchies")
```

### Main Functions
```
# shiny-app to create/modify a tree by dragging levels around,...
?dynHier

# shiny-app to generate a hierarchy from a given input that contains the nested level-structure
?fixedHier

# function that computes hierarchies given positions
?sdcHier_compute
```

### Updates
#### `0.7.0`
- some bugfixes
- removed argument `full_names` froms `sdcHier_compute()`
- show code to generated hierarchies in `dynHier()` and `fixedHier()`

#### `0.6.0`
- fixes for shiny-Apps `dynHier()` and `fixedHier()`
- renamed `dim_by_position()` to `sdcHier_compute()`
- `sdcHier_convert()` can output the required code to compute the hierarchy

#### `0.5.0`
- `sdcHier_convert()` can output the json-code
- fixes to documentation and shinyApps `dynHier()` and `fixedHier()`

#### `0.4.0`
- Functions (`sdcHier_create()`, `sdcHier_add()`, `sdcHier_delete()`, `sdcHier_rename()`, `sdcHier_info()`, `sdcHier_nodenames()` and `sdcHier_convert()`) dealing with creation and updating of nested hierarchies and extracting information have been added
