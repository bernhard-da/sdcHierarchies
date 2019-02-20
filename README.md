
## sdcHierarchies

[![Travis build
status](https://travis-ci.org/bernhard-da/sdcHierarchies.svg?branch=next)](https://travis-ci.org/bernhard-da/sdcHierarchies)
[![Coverage
status](https://codecov.io/gh/bernhard-da/sdcHierarchies/branch/next/graph/badge.svg)](https://codecov.io/gh/bernhard-da/sdcHierarchies/branch/next)
[![GitHub last
commit](https://img.shields.io/github/last-commit/bernhard-da/sdcHierarchies.svg?logo=github)](https://github.com/bernhard-da/sdcHierarchies/commits/next)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/bernhard-da/sdcHierarchies.svg?logo=github)](https://github.com/bernhard-da/sdcHierarchies)

R-Package to (interactively) generate or modify (nested) hierarchies
that are required as input for packages such as
[**sdcTable**](https://CRAN.R-project.org/package=sdcTable) or
[**cellKey**](https://github.com/sdcTools/cellKey).

The package is in its early stages and will likely change a lot in
future versions. Still, any feedback/pull requests are very much
welcome\!

### Installation

    remotes::install_github(
      repo = "bernhard-da/sdcHierarchies",
      dependencies = TRUE,
      build_opts = c("--no-resave-data", "--no-manual")
    )

### Usage

Please have a look at the package vignette
`sdcHierarchies::hier_vignette()` or by clicking
[**here**](https://bernhard-da.github.io/sdcHierarchies/articles/usage.html)
or browse the automatically generated
[**documentation**](https://bernhard-da.github.io/sdcHierarchies/).

### Updates

Updates/Changes are listed
[**here**](https://bernhard-da.github.io/sdcHierarchies/news/index.html).

### Future development

  - [ ] Harmonize input names of exported functions (tree, root, leaf)
  - [ ] check where documentation can be re-used
  - [ ] Allow to create and import in the following format: `Tot/A/a1/x`
  - [ ] Implement a summary method containing information such as the
    number of nodes, the depth of the tree, some balancing measures, …
  - [ ] `hier_import()` should gain an `as` argument similar to
    `hier_compute()`
  - [ ] Implement a plot method similar to `data.tree::plot`
