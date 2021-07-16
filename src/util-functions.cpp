#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
/* returns all possible nodes */
CharacterVector rcpp_all_nodes(DataFrame tree) {
  CharacterVector res = tree["leaf"];
  return(res);
}

// [[Rcpp::export]]
/* returns the name of the root node */
CharacterVector rcpp_rootnode(DataFrame tree) {
  CharacterVector root = tree["root"];
  CharacterVector leaf = tree["leaf"];

  LogicalVector lgl = root == leaf;
  CharacterVector res = root[lgl];
  return(res);
}

// [[Rcpp::export]]
/* returns true if the specified leaf is the rootnode */
bool rcpp_is_rootnode(DataFrame tree, CharacterVector leaf) {
  CharacterVector rn = rcpp_rootnode(tree);
  bool res = rn[0] == leaf[0];
  return(res);
}

// [[Rcpp::export]]
/* returns all direct children of the given leaf */
CharacterVector rcpp_children(DataFrame tree, CharacterVector leaf) {
  int nr_rows = tree.nrows();
  CharacterVector res;
  LogicalVector lgl(nr_rows);
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  CharacterVector rn = rcpp_rootnode(tree);
  if (rn[0] == leaf[0]) {
    for (int i = 0; i < nr_rows; i++) {
      lgl[i] = (v_root[i] == rn[0]) and (v_root[i] != v_leaf[i]);
    }
  } else {
    for (int i = 0; i < nr_rows; i++) {
      lgl[i] = v_root[i] == leaf[0];
    }
  }
  return(v_leaf[lgl]);
}

// [[Rcpp::export]]
/* returns the number of children of a given leaf */
int rcpp_nr_children(DataFrame tree, CharacterVector leaf) {
  CharacterVector res = rcpp_children(tree, leaf);
  return(res.size());
}

// [[Rcpp::export]]
/* returns true, if the given leaf has no children */
bool rcpp_is_leaf(DataFrame tree, CharacterVector leaf) {
  int nr = rcpp_nr_children(tree, leaf);
  return(nr == 0);
}

// [[Rcpp::export]]
/* returns the siblings of the given leaf */
CharacterVector rcpp_siblings(DataFrame tree, CharacterVector leaf) {
  int nr_rows = tree.nrows();
  LogicalVector lgl(nr_rows);
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];

  for (int i = 0; i < nr_rows; i++) {
    lgl[i] = v_leaf[i] == leaf[0];
  }

  CharacterVector parent = v_root[lgl];
  if (parent.size() == 0) {
    stop("error: leaf not found");
  }

  /* subset to parent */
  for (int i = 0; i < nr_rows; i++) {
    lgl[i] = v_root[i] == parent[0] and (v_leaf[i] != leaf[0]) and (v_leaf[i] != parent[0]);
  }
  CharacterVector res = v_leaf[lgl];
  return(res);
}

// [[Rcpp::export]]
/* returns the number of siblings of the given leaf */
int rcpp_nr_siblings(DataFrame tree, CharacterVector leaf) {
  CharacterVector res = rcpp_siblings(tree, leaf);
  return(res.size());
}

// [[Rcpp::export]]
/* returns true, if the leaf exists in the tree */
bool rcpp_exists(DataFrame tree, CharacterVector leaf) {
  CharacterVector all_nodes = rcpp_all_nodes(tree);
  for (int i = 0; i < all_nodes.size(); i++) {
    if (all_nodes[i] == leaf[0]) {
      return(true);
    }
  }
  return(false);
}


// [[Rcpp::export]]
List rcpp_rowinfo(DataFrame tree, CharacterVector leaf) {
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  CharacterVector v_level = tree["level"];
  IntegerVector matchidx = match(leaf, v_leaf);
  int idx = matchidx[0] - 1;
  CharacterVector root; root = v_root[idx];

  IntegerVector lev; lev = v_level[idx];

  return(List::create(
    Named("root") = root,
    Named("leaf")= leaf,
    Named("level") = lev
  ));
}

// [[Rcpp::export]]
CharacterVector rcpp_parent(DataFrame tree, CharacterVector leaf) {
  CharacterVector rootnode = rcpp_rootnode(tree);
  if (leaf[0] == rootnode[0]) {
    return(CharacterVector::get_na());
  }
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  IntegerVector matchidx = match(leaf, v_leaf);
  int idx = matchidx[0] - 1;
  CharacterVector res;
  res = v_root[idx];
  return(res);
}

// [[Rcpp::export]]
/* numeric level of the given leaf */
int rcpp_level(DataFrame tree, CharacterVector leaf) {
  if (!rcpp_exists(tree, leaf)) {
    stop("non-existing leaf detected");
  }

  CharacterVector v_leaf = tree["leaf"];
  IntegerVector idx = match(leaf, v_leaf) - 1;
  IntegerVector v_levels = tree["level"];
  IntegerVector res = v_levels[idx];
  int out = res[0];
  return(out);
}

// [[Rcpp::export]]
IntegerVector rcpp_levels(DataFrame tree) {
  IntegerVector res = tree["level"];
  res.names() = rcpp_all_nodes(tree);
  return(res);
}

// [[Rcpp::export]]
int rcpp_nr_levels(DataFrame tree) {
  IntegerVector res = tree["level"];
  return(max(res));
}

// [[Rcpp::export]]
CharacterVector rcpp_path_new(DataFrame tree, CharacterVector leaf) {
  CharacterVector rootnode = rcpp_rootnode(tree);
  int lev = rcpp_level(tree, leaf);
  CharacterVector path(lev);
  path[0] = rootnode[0];
  if (lev == 1) {
    // special-case: rootnode
    return(path);
  }

  path[lev - 1] = leaf[0];
  if (lev == 2) {
    // special-case: only two levels
    return(path);
  }
  int start = lev - 2;
  CharacterVector cur_code = leaf;
  for (int i = start; i >= 0; i--) {
    CharacterVector parent = rcpp_parent(tree, cur_code);
    path[i] = parent[0];
    cur_code = parent[0];
  }
  return(path);
}

// [[Rcpp::export]]
/* returns a character-vector with the path from total to leaf */
CharacterVector rcpp_path(DataFrame tree, CharacterVector leaf) {
  CharacterVector rootnode = rcpp_rootnode(tree);

  if (leaf[0] == rootnode[0]) {
    return(leaf);
  }

  bool finished = false;
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  int nr_rows = tree.nrows();
  LogicalVector lgl(nr_rows);
  CharacterVector path = leaf;
  CharacterVector cur_code = leaf;
  while (!finished) {
    for (int i = 0; i < nr_rows; i++) {
      lgl[i] = v_leaf[i] == cur_code[0];
    }
    CharacterVector parent = v_root[lgl];
    path.push_front(parent[0]);
    if (parent[0] == rootnode[0]) {
      finished = true;
    } else {
      cur_code = parent;
    }
  }
  return(path);
}

// [[Rcpp::export]]
bool rcpp_is_bogus(DataFrame tree, CharacterVector leaf) {
  bool cond1 = rcpp_nr_children(tree, leaf) < 2;
  bool cond2 = rcpp_nr_siblings(tree, leaf) == 0;
  return(cond1 and cond2);
}

// [[Rcpp::export]]
CharacterVector rcpp_bogus_codes(DataFrame tree) {
  if (tree.nrows() == 1) {
    return(CharacterVector());
  }

  CharacterVector all_codes = rcpp_all_nodes(tree);
  int nr_codes = all_codes.size();
  LogicalVector lgl(nr_codes);
  CharacterVector leaf;
  for (int i = 0; i < nr_codes; i++) {
    leaf = all_codes[i];
    lgl[i] = rcpp_is_bogus(tree, leaf);
  }
  CharacterVector bogus = all_codes[lgl];
  return(bogus);
}

// [[Rcpp::export]]
CharacterVector rcpp_contributing_leaves(DataFrame tree, CharacterVector leaf) {
  /* no nodes contributes to a leaf-node */
  if (rcpp_is_leaf(tree, leaf)) {
    return(leaf);
  }

  CharacterVector res;
  CharacterVector childs = rcpp_children(tree, leaf);
  bool finished = false;
  CharacterVector cur_code;
  while(!finished) {
    cur_code = childs[0];
    if (rcpp_is_leaf(tree, cur_code)) {
      /* leaf -> is a contributing code;
       * if bogus -> its parent is added
      */
      if (rcpp_is_bogus(tree, cur_code)) {
        CharacterVector v_parent = rcpp_parent(tree, cur_code);
        res.push_back(v_parent[0]);
      } else {
        res.push_back(childs[0]);
      }
    } else {
      /* this is not a leaf -> we add its children to the search list */
      CharacterVector new_childs = rcpp_children(tree, cur_code);
      for (int j = 0; j < new_childs.size(); j++) {
        childs.push_back(new_childs[j]);
      }
    }
    childs.erase(0);
    finished = childs.size() == 0;
  }

  CharacterVector v_leaf = tree["leaf"];
  IntegerVector idx = match(res, v_leaf) - 1;
  res = v_leaf[idx.sort()];
  return(res);
}

// [[Rcpp::export]]
LogicalVector rcpp_is_minimal_code(DataFrame tree) {
  CharacterVector codes = rcpp_all_nodes(tree);
  int nr_codes = codes.size();
  CharacterVector leaf;
  LogicalVector out(nr_codes);
  for (int i = 0; i < nr_codes; i++) {
    leaf = codes[i];
    out[i] = rcpp_is_leaf(tree, leaf);
  }
  out.names() = codes;
  return(out);
}

// [[Rcpp::export]]
CharacterVector rcpp_minimal_codes(DataFrame tree) {
  LogicalVector idx = rcpp_is_minimal_code(tree);
  CharacterVector res = rcpp_all_nodes(tree);
  return(res[idx]);
}

// [[Rcpp::export]]
LogicalVector rcpp_is_subtotal(DataFrame tree) {
  LogicalVector res = !rcpp_is_minimal_code(tree);
  res.names() = rcpp_all_nodes(tree);
  return(res);
}

// [[Rcpp::export]]
CharacterVector rcpp_subtotals(DataFrame tree) {
  LogicalVector idx = rcpp_is_subtotal(tree);
  CharacterVector all_codes = rcpp_all_nodes(tree);
  return(all_codes[idx]);
}

// [[Rcpp::export]]
DataFrame rcpp_prune(DataFrame tree, CharacterVector leaf) {
  if (!rcpp_exists(tree, leaf)) {
    return(tree);
  }
  if (rcpp_is_rootnode(tree, leaf)) {
    stop("rootnode cannot be removed");
  }

  bool finished = false;
  CharacterVector todos;
  CharacterVector code;
  todos = leaf;

  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  IntegerVector v_level = tree["level"];

  while (!finished) {
    code = todos[0];
    CharacterVector new_childs = rcpp_children(tree, code);
    for (int j = 0; j < new_childs.size(); j++) {
      todos.push_back(new_childs[j]);
    }

    // remove all rows where current code is the root column
    int len = v_root.size();
    IntegerVector poss = seq_len(len) - 1;
    IntegerVector matchidx = match(code, v_root) - 1;
    IntegerVector keep = setdiff(poss, matchidx).sort();
    v_root = v_root[keep];
    v_leaf = v_leaf[keep];
    v_level = v_level[keep];

    // remove all rows where current code is the leaf column
    len = v_root.size();
    poss = seq_len(len) - 1;
    matchidx = match(code, v_leaf) - 1;
    keep = setdiff(poss, matchidx).sort();
    v_root = v_root[keep];
    v_leaf = v_leaf[keep];
    v_level = v_level[keep];

    todos.erase(0);
    finished = todos.size() == 0;
  }

  DataFrame res = DataFrame::create(
    Named("root")  = v_root,
    Named("leaf")  = v_leaf,
    Named("level") = v_level
  );

  // return a data.table and add class-info
  res.attr("class") = CharacterVector::create("sdc_hierarchy", "data.table", "data.frame");
  return(res);
}

// [[Rcpp::export]]
List rcpp_info(DataFrame tree, CharacterVector leaf) {
  return(List::create(
    Named("name") = leaf,
    Named("is_rootnode") = rcpp_is_rootnode(tree, leaf),
    Named("level") = rcpp_level(tree, leaf),
    Named("is_leaf") = rcpp_is_leaf(tree, leaf),
    Named("siblings") = rcpp_siblings(tree, leaf),
    Named("contributing_codes") = rcpp_contributing_leaves(tree, leaf),
    Named("children") = rcpp_children(tree, leaf),
    Named("parent") = rcpp_parent(tree, leaf),
    Named("is_bogus") = rcpp_is_bogus(tree, leaf)
  ));
}
