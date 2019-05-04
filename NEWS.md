# sdcHierarchies 0.19
### Bugfixes
- remove `data.tree` from shiny app;
- other small fixes in the shiny-app

# sdcHierarchies 0.18
### Features
- new function `hier_grid()` that returns all combinations of levels (and optionally levels) of some hierarchies

### Bugfixes
- fix references to `hier_app()` in vignette

# sdcHierarchies 0.17

### Features
- `hier_match()` returns a mapping between original and default codes, fixing [~~issue #9~~](https://github.com/bernhard-da/sdcHierarchies/issues/9)
- `hier_to_tree()` returns a hierarchy in default format for any object created for example with `hier_convert()`, fixing [~~issue #10~~](https://github.com/bernhard-da/sdcHierarchies/issues/10)

### Bugfixes
- correctly compute default codes, fixing [~~issue #7~~](https://github.com/bernhard-da/sdcHierarchies/issues/7)
- correctly convert to `R` code, fixing [~~issue #8~~](https://github.com/bernhard-da/sdcHierarchies/issues/8)

# sdcHierarchies 0.16
- do not rely on `data.tree`
- examples for all exported function
- new function `hier_display()`
- harmonize function arguments
- first CRAN version

# sdcHierarchies 0.15
- preps for CRAN

# sdcHierarchies 0.14
- rename functions/methods to `hier_xxx()`
- improve documentation
- rewrite import of hrc-files from tau-argus in `hier_import`
- improve test coverage

# sdcHierarchies 0.13.2
- code style improves

# sdcHierarchies 0.13.1
- various fixes and improvements; 
- rewrote `h_min_contributing_codes()`
- started to add unit-tests

# sdcHierarchies 0.13.0
- in `sdcHier_info()`, the codes really contributing to a given node are computed and returned 
- `sdcHier_convert()` allows to return a `data.table`
- `sdcHier_convert()` allows to return a list object suitable for `sdcTable` (`format = "sdc"`)
- `sdcHier_import()` allows to create a hierarchy from a list-input created from `sdcHier_convert(..., format = "sdc")`

# sdcHierarchies 0.12.0
- continuous integration using travis
- check code is linted correctly
- building docs using pkgsdown, site is [**here**](https://bernhard-da.github.io/sdcHierarchies)

# sdcHierarchies 0.11.0
- `sdcHier_convert()` can now convert a hierarchy to the format tau-argus understands using `format='argus'`
- `sdcHier_export()` writes hierarchies in specific formats to disk
- `sdcHier_import()` gained the ability to return sdc hierarchies from `hrc`-files in tau-argus format as well as from code and json
- `sdcHier_info()` now shows the children of a node (if existing)
- `sdcHier()` gained the feature to export hierarchies as json-encoded strings and hrc-files suitable as input for tau-argus
- started with vignette that can be started with `sdcHier_vignette()`

# sdcHierarchies 0.10.0
- implement *undo*-feature in `sdcHier()`

# sdcHierarchies 0.9.0
- combine `dynHier()` and `fixedHier()` to `sdcHier()`

# sdcHierarchies 0.8.0
- `sdcHier_convert()` gained an argument `path` to write the output to a file
- new function `sdcHier_compute_fromList()` to create a hierarchy from a named list
- invisibly return results to avoid duplicated printing
- start with tabbed interface for shiny-apps

# sdcHierarchies 0.7.0
- some bugfixes
- removed argument `full_names` froms `sdcHier_compute()`
- show code to generated hierarchies in `dynHier()` and `fixedHier()`

# sdcHierarchies 0.6.0
- fixes for shiny-Apps `dynHier()` and `fixedHier()`
- renamed `dim_by_position()` to `sdcHier_compute()`
- `sdcHier_convert()` can output the required code to compute the hierarchy

# sdcHierarchies 0.5.0
- `sdcHier_convert()` can output the json-code
- fixes to documentation and shinyApps `dynHier()` and `fixedHier()`

# sdcHierarchies 0.4.0
- Functions (`sdcHier_create()`, `sdcHier_add()`, `sdcHier_delete()`, `sdcHier_rename()`, `sdcHier_info()`, `sdcHier_nodenames()` and `sdcHier_convert()`) dealing with creation and updating of nested hierarchies and extracting information have been added
