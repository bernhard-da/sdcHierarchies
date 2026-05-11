#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <string>
#include <deque>
#include <algorithm>
#include <set>

using namespace Rcpp;

// internal utils
std::unordered_map<std::string, std::string> build_parent_map(CharacterVector roots, CharacterVector leaves) {
  std::unordered_map<std::string, std::string> p_map;
  for (int i = 0; i < roots.size(); ++i) {
    std::string r = as<std::string>(roots[i]);
    std::string l = as<std::string>(leaves[i]);
    if (r != l) p_map[l] = r;
  }
  return p_map;
}

std::unordered_map<std::string, std::vector<std::string>> build_adj_map(CharacterVector roots, CharacterVector leaves) {
  std::unordered_map<std::string, std::vector<std::string>> adj;
  for (int i = 0; i < roots.size(); ++i) {
    std::string r = as<std::string>(roots[i]);
    std::string l = as<std::string>(leaves[i]);
    if (r != l) adj[r].push_back(l);
  }
  return adj;
}

// exported function

// [[Rcpp::export]]
CharacterVector rcpp_all_nodes(DataFrame tree) {
  return tree["leaf"];
}

// [[Rcpp::export]]
CharacterVector rcpp_rootnode(DataFrame tree) {
  CharacterVector root = tree["root"], leaf = tree["leaf"];
  for (int i = 0; i < root.size(); ++i) {
    if (root[i] == leaf[i]) return CharacterVector::create(root[i]);
  }
  return CharacterVector::create(NA_STRING);
}

// [[Rcpp::export]]
bool rcpp_exists(DataFrame tree, CharacterVector leaf) {
  if (leaf.size() == 0) return false;
  CharacterVector nodes = tree["leaf"];
  std::string target = as<std::string>(leaf[0]);
  for (int i = 0; i < nodes.size(); ++i) {
    if (as<std::string>(nodes[i]) == target) return true;
  }
  return false;
}

// [[Rcpp::export]]
bool rcpp_is_rootnode(DataFrame tree, CharacterVector leaf) {
  if (leaf.size() == 0) return false;
  CharacterVector rn = rcpp_rootnode(tree);
  return !CharacterVector::is_na(rn[0]) && rn[0] == leaf[0];
}

// [[Rcpp::export]]
CharacterVector rcpp_parent(DataFrame tree, CharacterVector leaf) {
  if (leaf.size() == 0) return CharacterVector::get_na();
  std::string target = as<std::string>(leaf[0]);
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  for (int i = 0; i < v_leaf.size(); ++i) {
    if (as<std::string>(v_leaf[i]) == target) {
      if (v_root[i] == v_leaf[i]) return CharacterVector::get_na();
      return CharacterVector::create(v_root[i]);
    }
  }
  return CharacterVector::get_na();
}

// [[Rcpp::export]]
CharacterVector rcpp_children(DataFrame tree, CharacterVector leaf) {
  if (leaf.size() == 0) return CharacterVector::create();
  std::string target = as<std::string>(leaf[0]);
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  std::vector<std::string> children;
  for (int i = 0; i < v_root.size(); ++i) {
    if (as<std::string>(v_root[i]) == target && v_root[i] != v_leaf[i]) {
      children.push_back(as<std::string>(v_leaf[i]));
    }
  }
  return wrap(children);
}

// [[Rcpp::export]]
CharacterVector rcpp_siblings(DataFrame tree, CharacterVector leaf) {
  CharacterVector p = rcpp_parent(tree, leaf);
  if (CharacterVector::is_na(p[0])) return CharacterVector::create();
  CharacterVector kids = rcpp_children(tree, p);
  std::string target = as<std::string>(leaf[0]);
  std::vector<std::string> sibs;
  for (int i = 0; i < kids.size(); ++i) {
    if (as<std::string>(kids[i]) != target) sibs.push_back(as<std::string>(kids[i]));
  }
  return wrap(sibs);
}

// [[Rcpp::export]]
List rcpp_bogus_codes(DataFrame tree) {
  if (tree.nrows() <= 1) {
    return List::create(Named("bogus_parent") = CharacterVector(), Named("bogus") = CharacterVector());
  }
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  std::unordered_map<std::string, int> child_counts;
  for (int i = 0; i < v_root.size(); ++i) {
    if (v_root[i] != v_leaf[i]) child_counts[as<std::string>(v_root[i])]++;
  }
  std::string rn = as<std::string>(rcpp_rootnode(tree)[0]);
  std::vector<std::string> bogus_p, bogus_c;
  for (int i = 0; i < v_leaf.size(); ++i) {
    std::string leaf_node = as<std::string>(v_leaf[i]);
    if (leaf_node == rn) continue;
    std::string parent_node = as<std::string>(v_root[i]);
    if (parent_node == leaf_node) continue;
    if (child_counts[parent_node] == 1) {
      bogus_c.push_back(leaf_node);
      bogus_p.push_back(parent_node);
    }
  }
  return List::create(
    Named("bogus_parent") = wrap(bogus_p),
    Named("bogus") = wrap(bogus_c)
  );
}

// [[Rcpp::export]]
CharacterVector rcpp_replace_with_bogusparent(List bogus_info, CharacterVector leaf) {
  if (leaf.size() == 0) return leaf;
  CharacterVector bc = bogus_info["bogus"], bp = bogus_info["bogus_parent"];
  std::string current = as<std::string>(leaf[0]);
  bool found = true;
  while (found) {
    found = false;
    for (int i = 0; i < bc.size(); ++i) {
      if (as<std::string>(bc[i]) == current) {
        current = as<std::string>(bp[i]);
        found = true;
        break;
      }
    }
  }
  return CharacterVector::create(current);
}

// [[Rcpp::export]]
CharacterVector rcpp_contributing_leaves(DataFrame tree, CharacterVector leaf) {
  if (leaf.size() == 0) return CharacterVector::create();

  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  int n = v_root.size();

  // Build adjacency map (parent -> children vector)
  std::unordered_map<std::string, std::vector<std::string>> adj;
  // Build reverse map (child -> parent)
  std::unordered_map<std::string, std::string> parent_of;
  // Collect all valid nodes
  std::set<std::string> all_nodes;

  for (int i = 0; i < n; ++i) {
    std::string r = as<std::string>(v_root[i]);
    std::string l = as<std::string>(v_leaf[i]);
    all_nodes.insert(r);
    all_nodes.insert(l);
    if (r != l) {
      adj[r].push_back(l);
      parent_of[l] = r;
    }
  }

  std::string target = as<std::string>(leaf[0]);

  // Check if target exists in hierarchy
  if (all_nodes.find(target) == all_nodes.end()) {
    stop("invalid leaf detected");
  }

  // If target is a leaf itself, return it directly
  if (adj.find(target) == adj.end()) {
    return wrap(CharacterVector::create(target));
  }

  // Find all actual leaf nodes (appear in data but not as parents)
  std::set<std::string> leaves;
  for (int i = 0; i < n; ++i) {
    std::string l = as<std::string>(v_leaf[i]);
    if (adj.find(l) == adj.end()) {
      leaves.insert(l);
    }
  }

  // For each leaf, find its real (non-bogus) ancestor
  std::set<std::string> effective_leaves;
  for (auto& leaf_node : leaves) {
    std::string current = leaf_node;
    // Walk up while current is the only child of its parent
    while (parent_of.find(current) != parent_of.end() &&
      adj[parent_of[current]].size() == 1) {
      current = parent_of[current];
    }
    effective_leaves.insert(current);
  }

  std::set<std::string> result;
  std::vector<std::string> stack;
  stack.push_back(target);

  // Traversal to collect all descendants
  while (!stack.empty()) {
    std::string current = stack.back();
    stack.pop_back();

    auto it = adj.find(current);
    if (it == adj.end()) {
      // current is a leaf -> add its real (non-bogus) ancestor
      std::string effective = current;
      while (parent_of.find(effective) != parent_of.end() &&
        adj[parent_of[effective]].size() == 1) {
        effective = parent_of[effective];
      }
      result.insert(effective);
    } else {
      // current has children -> push to explore
      for (auto& child : it->second) {
        stack.push_back(child);
      }
    }
  }

  return wrap(CharacterVector(result.begin(), result.end()));
}

// [[Rcpp::export]]
List rcpp_leafinfo(DataFrame tree, CharacterVector leaf) {
  std::string target = as<std::string>(leaf[0]);
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  IntegerVector v_level = tree["level"];
  std::string parent = "", tree_rn = "";
  int level = 0;
  std::vector<std::string> children;
  std::unordered_map<std::string, int> counts;

  for (int i = 0; i < v_root.size(); ++i) {
    std::string r = as<std::string>(v_root[i]), l = as<std::string>(v_leaf[i]);
    if (r == l) tree_rn = r;
    if (l == target) { parent = r; level = v_level[i]; }
    if (r == target && r != l) children.push_back(l);
    if (r != l) counts[r]++;
  }

  bool is_rootnode = (target == parent);
  std::vector<std::string> siblings;
  if (!is_rootnode) {
    for (int i = 0; i < v_root.size(); ++i) {
      if (as<std::string>(v_root[i]) == parent && as<std::string>(v_leaf[i]) != target && v_root[i] != v_leaf[i])
        siblings.push_back(as<std::string>(v_leaf[i]));
    }
  }

  bool is_bogus = (!is_rootnode && counts[parent] == 1);
  CharacterVector v_parent = CharacterVector::create(parent);
  if (is_bogus) v_parent = rcpp_replace_with_bogusparent(rcpp_bogus_codes(tree), leaf);

  return List::create(
    Named("tree_rootnode") = tree_rn,
    Named("leaf") = leaf,
    Named("parent") = parent,
    Named("level") = level,
    Named("is_rootnode") = is_rootnode,
    Named("children") = wrap(children),
    Named("nr_children") = (int)children.size(),
    Named("siblings") = wrap(siblings),
    Named("nr_siblings") = (int)siblings.size(),
    Named("is_leaf") = children.empty(),
    Named("is_bogus") = is_bogus,
    Named("is_bogus_parent") = v_parent
  );
}

// [[Rcpp::export]]
int rcpp_level(DataFrame tree, CharacterVector leaf) {
  std::string target = as<std::string>(leaf[0]);
  CharacterVector v_leaf = tree["leaf"]; IntegerVector v_level = tree["level"];
  for (int i = 0; i < v_leaf.size(); ++i) {
    if (as<std::string>(v_leaf[i]) == target) {
      return v_level[i];
    }
  }
  stop("non-existing leaf");
}

// [[Rcpp::export]]
IntegerVector rcpp_levels(DataFrame tree) {
  IntegerVector res = tree["level"];
  res.names() = rcpp_all_nodes(tree);
  return res;
}

// [[Rcpp::export]]
int rcpp_nr_levels(DataFrame tree) {
  IntegerVector res = tree["level"];
  return max(res);
}

// [[Rcpp::export]]
CharacterVector rcpp_path(DataFrame tree, CharacterVector leaf) {
  std::string target = as<std::string>(leaf[0]), rn = as<std::string>(rcpp_rootnode(tree)[0]);
  if (target == rn) return leaf;
  auto p_map = build_parent_map(tree["root"], tree["leaf"]);
  std::deque<std::string> path; path.push_front(target);
  while (target != rn && p_map.count(target)) {
    target = p_map[target];
    path.push_front(target);
  }
  return wrap(path);
}

// [[Rcpp::export]]
bool rcpp_is_bogus(DataFrame tree, CharacterVector leaf) {
  return as<bool>(rcpp_leafinfo(tree, leaf)["is_bogus"]);
}

// [[Rcpp::export]]
CharacterVector rcpp_all_leaves(DataFrame tree) {
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  std::set<std::string> roots;
  for (int i = 0; i < v_root.size(); ++i) roots.insert(as<std::string>(v_root[i]));
  std::vector<std::string> res;
  for (int i = 0; i < v_leaf.size(); ++i) {
    std::string l = as<std::string>(v_leaf[i]);
    if (roots.find(l) == roots.end() || v_root[i] == v_leaf[i]) {
      res.push_back(l);
    }
  }
  return wrap(res);
}

// [[Rcpp::export]]
bool rcpp_contains_hier(DataFrame tree, CharacterVector leaf, CharacterVector upper) {
  CharacterVector path = rcpp_path(tree, leaf);
  std::string up = as<std::string>(upper[0]);
  for (int i = 0; i < path.size(); ++i) {
    if (as<std::string>(path[i]) == up) {
      return true;
    }
  }
  return false;
}

// [[Rcpp::export]]
LogicalVector rcpp_is_minimal_code(DataFrame tree) {
  CharacterVector codes = rcpp_all_nodes(tree);
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  std::set<std::string> parent_set;
  for (int i = 0; i < v_root.size(); ++i) {
    if (v_root[i] != v_leaf[i]) {
      parent_set.insert(as<std::string>(v_root[i]));
    }
  }
  LogicalVector out(codes.size());
  for (int i = 0; i < codes.size(); ++i) {
    out[i] = parent_set.find(as<std::string>(codes[i])) == parent_set.end();
  }
  out.names() = codes;
  return out;
}

// [[Rcpp::export]]
CharacterVector rcpp_minimal_codes(DataFrame tree) {
  LogicalVector idx = rcpp_is_minimal_code(tree);
  return rcpp_all_nodes(tree)[idx];
}

// [[Rcpp::export]]
LogicalVector rcpp_is_subtotal(DataFrame tree) {
  LogicalVector res = !rcpp_is_minimal_code(tree);
  res.names() = rcpp_all_nodes(tree);
  return res;
}

// [[Rcpp::export]]
CharacterVector rcpp_subtotals(DataFrame tree) {
  LogicalVector idx = rcpp_is_subtotal(tree);
  return rcpp_all_nodes(tree)[idx];
}

// [[Rcpp::export]]
DataFrame rcpp_prune(DataFrame tree, CharacterVector leaf) {
  if (!rcpp_exists(tree, leaf)) {
    return tree;
  }
  if (rcpp_is_rootnode(tree, leaf)) {
    stop("rootnode cannot be removed");
  }
  std::string target = as<std::string>(leaf[0]);
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  auto adj = build_adj_map(v_root, v_leaf);
  std::set<std::string> to_rem; std::deque<std::string> q; q.push_back(target);
  while (!q.empty()) {
    std::string c = q.front();
    q.pop_front();
    to_rem.insert(c);
    if (adj.count(c)) {
      for (const auto& kid : adj[c]) {
        q.push_back(kid);
      }
    }
  }
  std::vector<int> keep;
  for (int i = 0; i < v_root.size(); ++i) {
    if (to_rem.find(as<std::string>(v_root[i])) == to_rem.end() && to_rem.find(as<std::string>(v_leaf[i])) == to_rem.end()) {
      keep.push_back(i);
    }
  }
  int n = keep.size();
  CharacterVector r_o(n), l_o(n);
  IntegerVector lev_o(n);
  IntegerVector v_lev = tree["level"];
  for (int i = 0; i < n; ++i) {
    r_o[i] = v_root[keep[i]];
    l_o[i] = v_leaf[keep[i]];
    lev_o[i] = v_lev[keep[i]];
  }
  DataFrame res = DataFrame::create(
    Named("root") = r_o,
    Named("leaf") = l_o,
    Named("level") = lev_o
  );
  res.attr("class") = CharacterVector::create("sdc_hierarchy", "data.table", "data.frame");
  return res;
}

// [[Rcpp::export]]
List rcpp_info(DataFrame tree, CharacterVector leaf) {
  List info = rcpp_leafinfo(tree, leaf);
  return List::create(
    Named("name") = leaf,
    Named("is_rootnode") = info["is_rootnode"],
    Named("level") = info["level"],
    Named("is_leaf") = info["is_leaf"],
    Named("siblings") = info["siblings"],
    Named("contributing_codes") = rcpp_contributing_leaves(tree, leaf),
    Named("children") = info["children"],
    Named("parent") = info["parent"],
    Named("is_bogus") = info["is_bogus"],
    Named("parent_bogus") = info["is_bogus_parent"]
  );
}

// [[Rcpp::export]]
IntegerVector rcpp_get_sort_order(DataFrame tree) {
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  std::string rn = as<std::string>(rcpp_rootnode(tree)[0]);

  // Map: Parent -> list of row-indices of leafs
  std::unordered_map<std::string, std::vector<int>> adj_idx;
  int root_row_idx = -1;

  // leafs are added as they appear
  for (int i = 0; i < v_root.size(); ++i) {
    std::string r = as<std::string>(v_root[i]);
    std::string l = as<std::string>(v_leaf[i]);
    if (r == l) {
      root_row_idx = i;
    } else {
      adj_idx[r].push_back(i);
    }
  }

  std::vector<int> sorted_indices;
  if (root_row_idx != -1) {
    std::vector<int> stack;
    stack.push_back(root_row_idx);

    while (!stack.empty()) {
      int curr_idx = stack.back();
      stack.pop_back();
      sorted_indices.push_back(curr_idx + 1);

      std::string curr_leaf = as<std::string>(v_leaf[curr_idx]);
      if (adj_idx.count(curr_leaf)) {
        const auto& children = adj_idx[curr_leaf];
        // a stack (LIFO) is used, therefore we need to push
        // leafs in reversed order
        for (auto it = children.rbegin(); it != children.rend(); ++it) {
          stack.push_back(*it);
        }
      }
    }
  }
  return wrap(sorted_indices);
}

// [[Rcpp::export]]
CharacterMatrix rcpp_tree_to_matrix(DataFrame tree) {
  // get sorted indices
  IntegerVector sort_idx = rcpp_get_sort_order(tree);
  int n = sort_idx.size();

  // max. depth (number of columns)
  IntegerVector levels = tree["level"];
  int max_lev = 0;
  for(int i = 0; i < levels.size(); ++i) {
    if(levels[i] > max_lev) max_lev = levels[i];
  }

  // initialize matrix (using NAs as defaults)
  CharacterMatrix mat(n, max_lev);
  for(int i = 0; i < n * max_lev; ++i) mat[i] = NA_STRING;

  // path-map for fast access
  CharacterVector v_leaf = tree["leaf"];
  auto p_map = build_parent_map(tree["root"], tree["leaf"]);
  std::string rn = as<std::string>(rcpp_rootnode(tree)[0]);

  // fill matrix row-by-row
  for (int i = 0; i < n; ++i) {
    int current_row_in_tree = sort_idx[i] - 1;
    std::string target = as<std::string>(v_leaf[current_row_in_tree]);

    // construct path backwards
    std::vector<std::string> path;
    std::string curr = target;
    path.push_back(curr);
    while (curr != rn && p_map.count(curr)) {
      curr = p_map[curr];
      path.push_back(curr);
    }

    // write values to matrix
    int path_len = path.size();
    for (int j = 0; j < path_len; ++j) {
      mat(i, path_len - 1 - j) = path[j];
    }
  }
  return mat;
}

// Hilfsfunktion: Findet alle Blätter unterhalb eines Knotens
std::vector<std::string> get_leaves_recursive(const std::string& node,
                                              const std::unordered_map<std::string, std::vector<std::string>>& adj) {
  std::vector<std::string> leaves;
  if (adj.find(node) == adj.end()) {
    leaves.push_back(node);
    return leaves;
  }

  std::deque<std::string> q;
  q.push_back(node);
  while (!q.empty()) {
    std::string curr = q.front();
    q.pop_front();
    if (adj.find(curr) != adj.end()) {
      for (const auto& child : adj.at(curr)) {
        q.push_back(child);
      }
    } else {
      leaves.push_back(curr);
    }
  }
  return leaves;
}

// [[Rcpp::export]]
List rcpp_get_leaves_list(DataFrame tree) {
  CharacterVector v_root = tree["root"], v_leaf = tree["leaf"];
  auto adj = build_adj_map(v_root, v_leaf);
  CharacterVector all_nodes = rcpp_all_nodes(tree);

  List res(all_nodes.size());
  for (int i = 0; i < all_nodes.size(); ++i) {
    std::string node = as<std::string>(all_nodes[i]);
    res[i] = wrap(get_leaves_recursive(node, adj));
  }
  res.names() = all_nodes;
  return res;
}
