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
?dim_by_position
```
