## sdcHierarchies

R-Package to (interactively) generate or modify (nested) hierarchies that are required as input for packages such as [**sdcTable**](https://cran.r-project.org/web/packages/sdcTable/index.html) or [**cellKey**](https://github.com/sdcTools/cellKey)

The package is not tested and will likely change a lot in future versions. Still, any feedback/pull requests are very much welcome!

### Installation
```
devtools::install_github("bernhard-da/sdcHierarchies")
```

### Main Functions
```
# shiny-app to import or create/modify a tree by dragging levels around,...
?sdcHier

## run an example starting with a vector
x <- c("01","02","03")
res < sdcHier(x)

## run an example starting with a hierarchy
h <- sdcHier_create("tot", letters[1:5])
sdcHier_add(h, "a", "a1")
sdcHier_add(h, "b", c("b1","b2"))
sdcHier_add(h, "b2", c("b2a","b2b"))
res <- sdcHier(h)

# functions that computes hierarchies given positions
?sdcHier_compute
?sdcHier_compute_fromList

# functions that create/modify hierarchies
?sdcHier_create
?sdcHier_add
?sdcHier_rename
?sdcHier_delete 

# functions that convert hierarchies
?sdcHier_import
?sdcHier_convert 

# get information about the hierarchy
?sdcHier_info 
```

### Updates
#### `0.10.0`
- implement *undo*-feature in `sdcHier()`

#### `0.9.0`
- combine `dynHier()` and `fixedHier()` to `sdcHier()`

#### `0.8.0`
- `sdcHier_convert()` gained an argument `path` to write the output to a file
- new function `sdcHier_compute_fromList()` to create a hierarchy from a named list
- invisibly return results to avoid duplicated printing
- start with tabbed interface for shiny-apps

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
