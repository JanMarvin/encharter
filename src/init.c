#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* Declare the functions (must match the names in your .cpp) */
extern SEXP pugi_find_first(SEXP, SEXP);
extern SEXP pugi_find_all(SEXP, SEXP);
extern SEXP pugi_add_child(SEXP, SEXP, SEXP);
extern SEXP pugi_remove(SEXP);
extern SEXP pugi_children(SEXP);
extern SEXP pugi_node_length(SEXP);
extern SEXP pugi_set_attr(SEXP, SEXP, SEXP);
extern SEXP pugi_get_attr(SEXP, SEXP);
extern SEXP pugi_set_text(SEXP, SEXP);
extern SEXP pugi_node_name(SEXP);
extern SEXP pugi_serialize_node(SEXP);
extern SEXP pugi_node_type(SEXP);
extern SEXP pugi_has_attr(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"pugi_find_first",    (DL_FUNC) &pugi_find_first,    2},
  {"pugi_find_all",      (DL_FUNC) &pugi_find_all,      2},
  {"pugi_add_child",     (DL_FUNC) &pugi_add_child,     3},
  {"pugi_remove",        (DL_FUNC) &pugi_remove,        1},
  {"pugi_children",      (DL_FUNC) &pugi_children,      1},
  {"pugi_node_length",   (DL_FUNC) &pugi_node_length,   1},
  {"pugi_set_attr",      (DL_FUNC) &pugi_set_attr,      3},
  {"pugi_get_attr",      (DL_FUNC) &pugi_get_attr,      2},
  {"pugi_set_text",      (DL_FUNC) &pugi_set_text,      2},
  {"pugi_node_name",     (DL_FUNC) &pugi_node_name,     1},
  {"pugi_serialize_node",(DL_FUNC) &pugi_serialize_node,1},
  {"pugi_node_type",     (DL_FUNC) &pugi_node_type,     1},
  {"pugi_has_attr",      (DL_FUNC) &pugi_has_attr,      2},
  {NULL, NULL, 0}
};

void R_init_encharter(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
