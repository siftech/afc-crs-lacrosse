// Automatically generated file
#ifndef __FIXEDPOINT_PARAMS_HPP_
#define __FIXEDPOINT_PARAMS_HPP_
#include"params.h"
#include"gparams.h"
struct fixedpoint_params {
  params_ref const & p;
  params_ref g;
  fixedpoint_params(params_ref const & _p = params_ref::get_empty()):
     p(_p), g(gparams::get_module("fixedpoint")) {}
  static void collect_param_descrs(param_descrs & d) {
    d.insert("timeout", CPK_UINT, "set timeout", "4294967295","fixedpoint");
    d.insert("engine", CPK_SYMBOL, "Select: auto-config, datalog, pdr, bmc", "auto-config","fixedpoint");
    d.insert("default_table", CPK_SYMBOL, "default table implementation: sparse, hashtable, bitvector, interval", "sparse","fixedpoint");
    d.insert("default_relation", CPK_SYMBOL, "default relation implementation: external_relation, pentagon", "pentagon","fixedpoint");
    d.insert("generate_explanations", CPK_BOOL, "(DATALOG) produce explanations for produced facts when using the datalog engine", "false","fixedpoint");
    d.insert("use_map_names", CPK_BOOL, "(DATALOG) use names from map files when displaying tuples", "true","fixedpoint");
    d.insert("bit_blast", CPK_BOOL, "(PDR) bit-blast bit-vectors", "false","fixedpoint");
    d.insert("explanations_on_relation_level", CPK_BOOL, "(DATALOG) if true, explanations are generated as history of each relation, rather than per fact (generate_explanations must be set to true for this option to have any effect)", "false","fixedpoint");
    d.insert("magic_sets_for_queries", CPK_BOOL, "(DATALOG) magic set transformation will be used for queries", "false","fixedpoint");
    d.insert("magic", CPK_BOOL, "perform symbolic magic set transformation", "false","fixedpoint");
    d.insert("scale", CPK_BOOL, "add scaling variable to linear real arithemtic clauses", "false","fixedpoint");
    d.insert("unbound_compressor", CPK_BOOL, "auxiliary relations will be introduced to avoid unbound variables in rule heads", "true","fixedpoint");
    d.insert("similarity_compressor", CPK_BOOL, "(DATALOG) rules that differ only in values of constants will be merged into a single rule", "true","fixedpoint");
    d.insert("similarity_compressor_threshold", CPK_UINT, "(DATALOG) if similarity_compressor is on, this value determines how many similar rules there must be in order for them to be merged", "11","fixedpoint");
    d.insert("all_or_nothing_deltas", CPK_BOOL, "(DATALOG) compile rules so that it is enough for the delta relation in union and widening operations to determine only whether the updated relation was modified or not", "false","fixedpoint");
    d.insert("compile_with_widening", CPK_BOOL, "(DATALOG) widening will be used to compile recursive rules", "false","fixedpoint");
    d.insert("eager_emptiness_checking", CPK_BOOL, "(DATALOG) emptiness of affected relations will be checked after each instruction, so that we may ommit unnecessary instructions", "true","fixedpoint");
    d.insert("default_table_checked", CPK_BOOL, "if true, the detault table will be default_table inside a wrapper that checks that its results are the same as of default_table_checker table", "false","fixedpoint");
    d.insert("default_table_checker", CPK_SYMBOL, "see default_table_checked", "null","fixedpoint");
    d.insert("initial_restart_timeout", CPK_UINT, "length of saturation run before the first restart (in ms), zero means no restarts", "0","fixedpoint");
    d.insert("output_profile", CPK_BOOL, "determines whether profile informations should be output when outputting Datalog rules or instructions", "false","fixedpoint");
    d.insert("inline_linear", CPK_BOOL, "try linear inlining method", "true","fixedpoint");
    d.insert("inline_eager", CPK_BOOL, "try eager inlining of rules", "true","fixedpoint");
    d.insert("inline_linear_branch", CPK_BOOL, "try linear inlining method with potential expansion", "false","fixedpoint");
    d.insert("fix_unbound_vars", CPK_BOOL, "fix unbound variables in tail", "false","fixedpoint");
    d.insert("output_tuples", CPK_BOOL, "determines whether tuples for output predicates should be output", "true","fixedpoint");
    d.insert("print_with_fixedpoint_extensions", CPK_BOOL, "use SMT-LIB2 fixedpoint extensions, instead of pure SMT2, when printing rules", "true","fixedpoint");
    d.insert("print_low_level_smt2", CPK_BOOL, "use (faster) low-level SMT2 printer (the printer is scalable but the result may not be as readable)", "false","fixedpoint");
    d.insert("print_with_variable_declarations", CPK_BOOL, "use variable declarations when displaying rules (instead of attempting to use original names)", "true","fixedpoint");
    d.insert("bfs_model_search", CPK_BOOL, "PDR: use BFS strategy for expanding model search", "true","fixedpoint");
    d.insert("use_farkas", CPK_BOOL, "PDR: use lemma generator based on Farkas (for linear real arithmetic)", "true","fixedpoint");
    d.insert("generate_proof_trace", CPK_BOOL, "PDR: trace for 'sat' answer as proof object", "false","fixedpoint");
    d.insert("flexible_trace", CPK_BOOL, "PDR: allow PDR generate long counter-examples by extending candidate trace within search area", "false","fixedpoint");
    d.insert("unfold_rules", CPK_UINT, "PDR: unfold rules statically using iterative squarring", "0","fixedpoint");
    d.insert("use_model_generalizer", CPK_BOOL, "PDR: use model for backwards propagation (instead of symbolic simulation)", "false","fixedpoint");
    d.insert("validate_result", CPK_BOOL, "PDR validate result (by proof checking or model checking)", "false","fixedpoint");
    d.insert("simplify_formulas_pre", CPK_BOOL, "PDR: simplify derived formulas before inductive propagation", "false","fixedpoint");
    d.insert("simplify_formulas_post", CPK_BOOL, "PDR: simplify derived formulas after inductive propagation", "false","fixedpoint");
    d.insert("slice", CPK_BOOL, "PDR: simplify clause set using slicing", "true","fixedpoint");
    d.insert("karr", CPK_BOOL, "Add linear invariants to clauses using Karr's method", "false","fixedpoint");
    d.insert("quantify_arrays", CPK_BOOL, "create quantified Horn clauses from clauses with arrays", "false","fixedpoint");
    d.insert("instantiate_quantifiers", CPK_BOOL, "instantiate quantified Horn clauses using E-matching heuristic", "false","fixedpoint");
    d.insert("coalesce_rules", CPK_BOOL, "BMC: coalesce rules", "false","fixedpoint");
    d.insert("use_multicore_generalizer", CPK_BOOL, "PDR: extract multiple cores for blocking states", "false","fixedpoint");
    d.insert("use_inductive_generalizer", CPK_BOOL, "PDR: generalize lemmas using induction strengthening", "true","fixedpoint");
    d.insert("use_arith_inductive_generalizer", CPK_BOOL, "PDR: generalize lemmas using arithmetic heuristics for induction strengthening", "false","fixedpoint");
    d.insert("use_convex_closure_generalizer", CPK_BOOL, "PDR: generalize using convex closures of lemmas", "false","fixedpoint");
    d.insert("use_convex_interior_generalizer", CPK_BOOL, "PDR: generalize using convex interiors of lemmas", "false","fixedpoint");
    d.insert("cache_mode", CPK_UINT, "PDR: use no (0), symbolic (1) or explicit cache (2) for model search", "0","fixedpoint");
    d.insert("inductive_reachability_check", CPK_BOOL, "PDR: assume negation of the cube on the previous level when checking for reachability (not only during cube weakening)", "false","fixedpoint");
    d.insert("max_num_contexts", CPK_UINT, "PDR: maximal number of contexts to create", "500","fixedpoint");
    d.insert("try_minimize_core", CPK_BOOL, "PDR: try to reduce core size (before inductive minimization)", "false","fixedpoint");
    d.insert("profile_timeout_milliseconds", CPK_UINT, "instructions and rules that took less than the threshold will not be printed when printed the instruction/rule list", "0","fixedpoint");
    d.insert("dbg_fpr_nonempty_relation_signature", CPK_BOOL, "if true, finite_product_relation will attempt to avoid creating inner relation with empty signature by putting in half of the table columns, if it would have been empty otherwise", "false","fixedpoint");
    d.insert("print_answer", CPK_BOOL, "print answer instance(s) to query", "false","fixedpoint");
    d.insert("print_certificate", CPK_BOOL, "print certificate for reachability or non-reachability", "false","fixedpoint");
    d.insert("print_boogie_certificate", CPK_BOOL, "print certificate for reachability or non-reachability using a format understood by Boogie", "false","fixedpoint");
    d.insert("print_statistics", CPK_BOOL, "print statistics", "false","fixedpoint");
    d.insert("use_utvpi", CPK_BOOL, "PDR: Enable UTVPI strategy", "true","fixedpoint");
    d.insert("tab_selection", CPK_SYMBOL, "selection method for tabular strategy: weight (default), first, var-use", "weight","fixedpoint");
    d.insert("full_expand", CPK_BOOL, "DUALITY: Fully expand derivation trees", "false","fixedpoint");
    d.insert("no_conj", CPK_BOOL, "DUALITY: No forced covering (conjectures)", "false","fixedpoint");
    d.insert("feasible_edges", CPK_BOOL, "DUALITY: Don't expand definitley infeasible edges", "true","fixedpoint");
    d.insert("use_underapprox", CPK_BOOL, "DUALITY: Use underapproximations", "false","fixedpoint");
    d.insert("stratified_inlining", CPK_BOOL, "DUALITY: Use stratified inlining", "false","fixedpoint");
    d.insert("recursion_bound", CPK_UINT, "DUALITY: Recursion bound for stratified inlining", "4294967295","fixedpoint");
    d.insert("profile", CPK_BOOL, "DUALITY: profile run time", "false","fixedpoint");
    d.insert("mbqi", CPK_BOOL, "DUALITY: use model-based quantifier instantion", "true","fixedpoint");
    d.insert("batch_expand", CPK_BOOL, "DUALITY: use batch expansion", "false","fixedpoint");
    d.insert("dump_aig", CPK_SYMBOL, "Dump clauses in AIG text format (AAG) to the given file name", "","fixedpoint");
    d.insert("conjecture_file", CPK_STRING, "DUALITY: save conjectures to file", "","fixedpoint");
    d.insert("enable_restarts", CPK_BOOL, "DUALITY: enable restarts", "false","fixedpoint");
  }
  /*
     REG_MODULE_PARAMS('fixedpoint', 'fixedpoint_params::collect_param_descrs')
     REG_MODULE_DESCRIPTION('fixedpoint', 'fixedpoint parameters')
  */
  unsigned timeout() const { return p.get_uint("timeout", g, 4294967295u); }
  symbol engine() const { return p.get_sym("engine", g, symbol("auto-config")); }
  symbol default_table() const { return p.get_sym("default_table", g, symbol("sparse")); }
  symbol default_relation() const { return p.get_sym("default_relation", g, symbol("pentagon")); }
  bool generate_explanations() const { return p.get_bool("generate_explanations", g, false); }
  bool use_map_names() const { return p.get_bool("use_map_names", g, true); }
  bool bit_blast() const { return p.get_bool("bit_blast", g, false); }
  bool explanations_on_relation_level() const { return p.get_bool("explanations_on_relation_level", g, false); }
  bool magic_sets_for_queries() const { return p.get_bool("magic_sets_for_queries", g, false); }
  bool magic() const { return p.get_bool("magic", g, false); }
  bool scale() const { return p.get_bool("scale", g, false); }
  bool unbound_compressor() const { return p.get_bool("unbound_compressor", g, true); }
  bool similarity_compressor() const { return p.get_bool("similarity_compressor", g, true); }
  unsigned similarity_compressor_threshold() const { return p.get_uint("similarity_compressor_threshold", g, 11u); }
  bool all_or_nothing_deltas() const { return p.get_bool("all_or_nothing_deltas", g, false); }
  bool compile_with_widening() const { return p.get_bool("compile_with_widening", g, false); }
  bool eager_emptiness_checking() const { return p.get_bool("eager_emptiness_checking", g, true); }
  bool default_table_checked() const { return p.get_bool("default_table_checked", g, false); }
  symbol default_table_checker() const { return p.get_sym("default_table_checker", g, symbol("null")); }
  unsigned initial_restart_timeout() const { return p.get_uint("initial_restart_timeout", g, 0u); }
  bool output_profile() const { return p.get_bool("output_profile", g, false); }
  bool inline_linear() const { return p.get_bool("inline_linear", g, true); }
  bool inline_eager() const { return p.get_bool("inline_eager", g, true); }
  bool inline_linear_branch() const { return p.get_bool("inline_linear_branch", g, false); }
  bool fix_unbound_vars() const { return p.get_bool("fix_unbound_vars", g, false); }
  bool output_tuples() const { return p.get_bool("output_tuples", g, true); }
  bool print_with_fixedpoint_extensions() const { return p.get_bool("print_with_fixedpoint_extensions", g, true); }
  bool print_low_level_smt2() const { return p.get_bool("print_low_level_smt2", g, false); }
  bool print_with_variable_declarations() const { return p.get_bool("print_with_variable_declarations", g, true); }
  bool bfs_model_search() const { return p.get_bool("bfs_model_search", g, true); }
  bool use_farkas() const { return p.get_bool("use_farkas", g, true); }
  bool generate_proof_trace() const { return p.get_bool("generate_proof_trace", g, false); }
  bool flexible_trace() const { return p.get_bool("flexible_trace", g, false); }
  unsigned unfold_rules() const { return p.get_uint("unfold_rules", g, 0u); }
  bool use_model_generalizer() const { return p.get_bool("use_model_generalizer", g, false); }
  bool validate_result() const { return p.get_bool("validate_result", g, false); }
  bool simplify_formulas_pre() const { return p.get_bool("simplify_formulas_pre", g, false); }
  bool simplify_formulas_post() const { return p.get_bool("simplify_formulas_post", g, false); }
  bool slice() const { return p.get_bool("slice", g, true); }
  bool karr() const { return p.get_bool("karr", g, false); }
  bool quantify_arrays() const { return p.get_bool("quantify_arrays", g, false); }
  bool instantiate_quantifiers() const { return p.get_bool("instantiate_quantifiers", g, false); }
  bool coalesce_rules() const { return p.get_bool("coalesce_rules", g, false); }
  bool use_multicore_generalizer() const { return p.get_bool("use_multicore_generalizer", g, false); }
  bool use_inductive_generalizer() const { return p.get_bool("use_inductive_generalizer", g, true); }
  bool use_arith_inductive_generalizer() const { return p.get_bool("use_arith_inductive_generalizer", g, false); }
  bool use_convex_closure_generalizer() const { return p.get_bool("use_convex_closure_generalizer", g, false); }
  bool use_convex_interior_generalizer() const { return p.get_bool("use_convex_interior_generalizer", g, false); }
  unsigned cache_mode() const { return p.get_uint("cache_mode", g, 0u); }
  bool inductive_reachability_check() const { return p.get_bool("inductive_reachability_check", g, false); }
  unsigned max_num_contexts() const { return p.get_uint("max_num_contexts", g, 500u); }
  bool try_minimize_core() const { return p.get_bool("try_minimize_core", g, false); }
  unsigned profile_timeout_milliseconds() const { return p.get_uint("profile_timeout_milliseconds", g, 0u); }
  bool dbg_fpr_nonempty_relation_signature() const { return p.get_bool("dbg_fpr_nonempty_relation_signature", g, false); }
  bool print_answer() const { return p.get_bool("print_answer", g, false); }
  bool print_certificate() const { return p.get_bool("print_certificate", g, false); }
  bool print_boogie_certificate() const { return p.get_bool("print_boogie_certificate", g, false); }
  bool print_statistics() const { return p.get_bool("print_statistics", g, false); }
  bool use_utvpi() const { return p.get_bool("use_utvpi", g, true); }
  symbol tab_selection() const { return p.get_sym("tab_selection", g, symbol("weight")); }
  bool full_expand() const { return p.get_bool("full_expand", g, false); }
  bool no_conj() const { return p.get_bool("no_conj", g, false); }
  bool feasible_edges() const { return p.get_bool("feasible_edges", g, true); }
  bool use_underapprox() const { return p.get_bool("use_underapprox", g, false); }
  bool stratified_inlining() const { return p.get_bool("stratified_inlining", g, false); }
  unsigned recursion_bound() const { return p.get_uint("recursion_bound", g, 4294967295u); }
  bool profile() const { return p.get_bool("profile", g, false); }
  bool mbqi() const { return p.get_bool("mbqi", g, true); }
  bool batch_expand() const { return p.get_bool("batch_expand", g, false); }
  symbol dump_aig() const { return p.get_sym("dump_aig", g, symbol("")); }
  char const * conjecture_file() const { return p.get_str("conjecture_file", g, ""); }
  bool enable_restarts() const { return p.get_bool("enable_restarts", g, false); }
};
#endif
