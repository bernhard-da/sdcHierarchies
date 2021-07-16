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
/* returns the siblings of the given leaf */
CharacterVector rcpp_siblings(DataFrame tree, CharacterVector leaf) {
  int nr_rows = tree.nrows();
  if (nr_rows == 1) {
    return(CharacterVector());
  }

  CharacterVector parent = rcpp_parent(tree, leaf);
  LogicalVector lgl(nr_rows);
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];

  /* subset to parent */
  for (int i = 0; i < v_root.size(); i++) {
    lgl[i] = v_root[i] == parent[0] and (v_leaf[i] != leaf[0]) and (v_leaf[i] != parent[0]);
  }
  CharacterVector res = v_leaf[lgl];
  return(res);
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
/* a list with bogus codes and its parent levels */
List rcpp_bogus_codes(DataFrame tree) {
  if (tree.nrows() == 1) {
    List empty = List::create(
      Named("bogus_parent") = CharacterVector(),
      Named("bogus") = CharacterVector()
    );
    return(empty);
  }

  CharacterVector v_root = tree["root"];
  IntegerVector res = table(v_root);
  CharacterVector parent = res.names();

  LogicalVector idx = res == 1;
  parent = parent[idx];

  IntegerVector ii;
  int n = parent.size();
  CharacterVector v_bogus(n), code, up;
  if (n > 0) {
    CharacterVector v_leaf = tree["leaf"];
    CharacterVector bogus(n);
    for (int i = 0; i < n; i++) {
      code = parent[i];
      IntegerVector ii = match(code, v_root) - 1;
      up = v_leaf[ii[0]];
      v_bogus[i] = up[0];
    }

    // we replace the parent code if necessary when we have
    // multiple nested bogus-codes
    bool finished = false;
    while (!finished) {
      finished = true;
      for (int i = 0; i < n; i++) {
        code = parent[i];
        ii = match(code, v_bogus) - 1;
        if (ii[0] >= 0) {
          finished = false;
          ii = match(code, v_bogus) - 1;
          parent[i] = parent[ii[0]];
        }
      }
    }
  }
  List out = List::create(
    Named("bogus_parent") = parent,
    Named("bogus") = v_bogus
  );
  return(out);
}

// [[Rcpp::export]]
/* utility fn to replace bogus codes with corrent parent codes */
CharacterVector rcpp_replace_with_bogusparent(List bogus_info, CharacterVector leaf) {
  CharacterVector bogus = bogus_info["bogus"];
  CharacterVector bogus_parent = bogus_info["bogus_parent"];
  int n = bogus.length();
  if (n == 0) {
    return(leaf);
  }

  IntegerVector ii = match(leaf, bogus) - 1;
  int idx = ii[0];
  if (idx < 0) {
    return(leaf);
  }
  CharacterVector res;
  res = bogus_parent[idx];
  return(res);
}

// [[Rcpp::export]]
/* basically all information required except for contributing leafs */
List rcpp_leafinfo(DataFrame tree, CharacterVector leaf) {
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  IntegerVector v_level = tree["level"];

  CharacterVector parent;
  CharacterVector tree_rootnode;

  int nr_codes = v_root.size();
  int level;

  LogicalVector is_children(nr_codes);

  bool is_leaf = true;
  for (int i = 0; i < nr_codes; i++) {
    if (v_root[i] == v_leaf[i]) {
      tree_rootnode = v_root[i];
    }
    if (v_root[i] == leaf[0]) {
      is_leaf = false;
      if (v_leaf[i] != v_root[i]) {
        is_children[i] = true;
      }
    }
    if (v_leaf[i] == leaf[0]) {
      level = v_level[i];
      parent = v_root[i];
    }
  }

  CharacterVector children = v_leaf[is_children];
  if (parent.size() == 0) {
    stop("invalid leaf detected");
  }

  bool is_rootnode = leaf[0] == parent[0];
  CharacterVector siblings;
  if (!is_rootnode) {
    LogicalVector is_sibling(nr_codes); // children of the parent!

    /* subset to parent */
    for (int i = 0; i < nr_codes; i++) {
      if ((v_leaf[i] != leaf[0]) and (v_root[i] == parent[0]) and (v_leaf[i] != parent[0])) {
        is_sibling[i] = true;
      }
    }
    siblings = v_leaf[is_sibling];
  }

  // bogus
  int nr_children = children.size();
  int nr_siblings = siblings.size();
  bool is_bogus = (nr_children < 2) and (nr_siblings == 0);

  if (is_rootnode) {
    is_bogus = false;
  }

  CharacterVector v_parent;
  if (is_bogus) {
    List bogus_info = rcpp_bogus_codes(tree);
    v_parent = rcpp_replace_with_bogusparent(bogus_info, leaf);
  }
  List out = List::create(
    Named("tree_rootnode") = tree_rootnode,
    Named("leaf") = leaf,
    Named("parent") = parent,
    Named("level") = level,
    Named("is_rootnode") = is_rootnode,
    Named("children") = children,
    Named("nr_children") = nr_children,
    Named("siblings") = siblings,
    Named("nr_siblings") = nr_siblings,
    Named("is_leaf") = is_leaf,
    Named("is_bogus") = is_bogus,
    Named("is_bogus_parent") = v_parent
  );
  return(out);
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
  return(rcpp_leafinfo(tree, leaf)["is_bogus"]);
}

// [[Rcpp::export]]
CharacterVector rcpp_all_leaves(DataFrame tree) {
  CharacterVector v_root = tree["root"];
  CharacterVector v_leaf = tree["leaf"];
  v_root = unique(v_root);

  if (tree.nrows() == 1) {
    return(v_root);
  }

  CharacterVector res = setdiff(v_leaf, v_root);

  // possible replace bogus-codes with their parents
  List bogus_info = rcpp_bogus_codes(tree);
  CharacterVector bogus_codes = bogus_info["bogus"];
  int nr_bogus = bogus_codes.size();
  if (nr_bogus > 0) {
    CharacterVector bogus_parent = bogus_info["bogus_parent"];
    IntegerVector idx;
    CharacterVector code;
    for (int i = 0; i < nr_bogus; i++) {
      code = bogus_codes[i];
      idx = match(code, res) - 1;
      if (idx[0] >= 0) {
        res[idx[0]] = bogus_parent[i];
      }
    }
  }

  // fix order
  IntegerVector idx = match(res, v_leaf) - 1;
  res = v_leaf[idx.sort()];
  return(res);
}

// [[Rcpp::export]]
bool rcpp_contains_hier(DataFrame tree, CharacterVector leaf, CharacterVector upper) {
  //List info = rcpp_leafinfo(tree, leaf);
  CharacterVector path = rcpp_path(tree, leaf);

  if (upper.size() != 1) {
    stop("scalar input required for argument `upper`");
  }

  IntegerVector res = match(upper, path);
  if (res[0] < 0) {
    return(false);
  }
  return(true);
}

// [[Rcpp::export]]
CharacterVector rcpp_contributing_leaves(DataFrame tree, CharacterVector leaf) {
  List info = rcpp_leafinfo(tree, leaf);
  if (info["is_rootnode"]) {
    return(rcpp_all_leaves(tree));
  }

  if (info["is_leaf"]) {
    return(leaf);
  }

  List bogus_info = rcpp_bogus_codes(tree);

  CharacterVector to_insert;
  CharacterVector childs = info["children"];
  bool finished = false;
  CharacterVector cur_code;

  int insert_cnt = 0;
  CharacterVector res(tree.nrows());

  while (!finished) {
    cur_code = childs[0];
    CharacterVector new_childs = rcpp_children(tree, cur_code);
    int nr_new_childs = new_childs.size();
    if (nr_new_childs == 0) {
      to_insert = rcpp_replace_with_bogusparent(bogus_info, cur_code);
      res[insert_cnt] = to_insert[0];
      insert_cnt = insert_cnt + 1;
    } else {
      /* this is not a leaf -> we add its children to the search list */
      for (int j = 0; j < new_childs.size(); j++) {
        childs.push_back(new_childs[j]);
      }
    }
    childs.erase(0);
    finished = childs.size() == 0;
  }

  CharacterVector v_leaf = tree["leaf"];
  IntegerVector use = seq(0, insert_cnt - 1);
  res = res[use];
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
    int nr_children = rcpp_children(tree, leaf).size();
    out[i] = nr_children == 0;
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
  List info = rcpp_leafinfo(tree, leaf);
  bool is_bogus = info["is_bogus"];
  CharacterVector parent_bogus;

  if (is_bogus) {
    List bogus_info = rcpp_bogus_codes(tree);
    CharacterVector vbogus = bogus_info["bogus"];
    parent_bogus = bogus_info["bogus_parent"];
    IntegerVector ii = match(leaf, vbogus) - 1;
    parent_bogus = parent_bogus[ii[0]];
  }

  return(List::create(
    Named("name") = leaf,
    Named("is_rootnode") = info("is_rootnode"),
    Named("level") = info("level"),
    Named("is_leaf") = info("is_leaf"),
    Named("siblings") = info("siblings"),
    Named("contributing_codes") = rcpp_contributing_leaves(tree, leaf),
    Named("children") = info("children"),
    Named("parent") = info["parent"],
    Named("is_bogus") = is_bogus,
    Named("parent_bogus") = parent_bogus
  ));
}
