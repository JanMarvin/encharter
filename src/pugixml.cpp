#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <pugixml/pugixml.hpp>
#include <sstream>
#include <memory>
#include <vector>

extern "C" {

  // --- Finalizer ---
  // This is called by R's Garbage Collector to prevent memory leaks
  void pugi_node_finalizer(SEXP ext_ptr) {
    pugi::xml_node* ptr = (pugi::xml_node*)R_ExternalPtrAddr(ext_ptr);
    if (ptr) delete ptr;
  }

  // --- Internal Helper ---
  SEXP wrap_node_raw(pugi::xml_node node, bool is_doc = false) {
    // Allocate handle on heap
    pugi::xml_node* ptr = new pugi::xml_node(node);

    // Create External Pointer
    SEXP ext_ptr = PROTECT(R_MakeExternalPtr(ptr, Rf_install("pugi_node"), R_NilValue));
    R_RegisterCFinalizerEx(ext_ptr, pugi_node_finalizer, TRUE);

    // Set S3 Classes
    SEXP cls;
    if (is_doc) {
      cls = PROTECT(Rf_allocVector(STRSXP, 3));
      SET_STRING_ELT(cls, 0, Rf_mkChar("pugi_xml"));
      SET_STRING_ELT(cls, 1, Rf_mkChar("pugi_node"));
      SET_STRING_ELT(cls, 2, Rf_mkChar("externalptr"));
    } else {
      cls = PROTECT(Rf_allocVector(STRSXP, 2));
      SET_STRING_ELT(cls, 0, Rf_mkChar("pugi_node"));
      SET_STRING_ELT(cls, 1, Rf_mkChar("externalptr"));
    }
    Rf_classgets(ext_ptr, cls);

    UNPROTECT(2);
    return ext_ptr;
  }

  // --- Exported Functions ---

  SEXP pugi_find_first(SEXP node_ptr, SEXP xpath_str) {
    if (node_ptr == R_NilValue) return R_NilValue;
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* xpath = CHAR(STRING_ELT(xpath_str, 0));

    pugi::xpath_node target = node->select_node(xpath);
    return wrap_node_raw(target.node());
  }

  SEXP pugi_find_all(SEXP node_ptr, SEXP xpath_str) {
    if (node_ptr == R_NilValue) return Rf_allocVector(VECSXP, 0);
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* xpath = CHAR(STRING_ELT(xpath_str, 0));

    pugi::xpath_node_set nodes = node->select_nodes(xpath);
    int n = nodes.size();

    SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
    for (int i = 0; i < n; i++) {
      SET_VECTOR_ELT(out, i, wrap_node_raw(nodes[i].node()));
    }

    UNPROTECT(1);
    return out;
  }

  SEXP pugi_add_child(SEXP node_ptr, SEXP name_str, SEXP where_int) {
    if (node_ptr == R_NilValue) return R_NilValue;
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* name = CHAR(STRING_ELT(name_str, 0));
    int where = Rf_asInteger(where_int);

    pugi::xml_node new_node;
    if (where == 0) new_node = node->prepend_child(name);
    else new_node = node->append_child(name);

    return wrap_node_raw(new_node);
  }

  SEXP pugi_remove(SEXP node_ptr) {
    if (node_ptr == R_NilValue) return R_NilValue;
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    if (node->parent()) node->parent().remove_child(*node);
    return R_NilValue;
  }

  SEXP pugi_children(SEXP node_ptr) {
    if (node_ptr == R_NilValue) return Rf_allocVector(VECSXP, 0);
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    pugi::xml_node target = *node;

    if (target.type() == pugi::node_document) {
      for (pugi::xml_node child : target.children()) {
        if (child.type() == pugi::node_element) {
          target = child;
          break;
        }
      }
    }

    std::vector<pugi::xml_node> children;
    for (pugi::xml_node child : target.children()) {
      if (child.type() == pugi::node_element) children.push_back(child);
    }

    SEXP out = PROTECT(Rf_allocVector(VECSXP, children.size()));
    for (size_t i = 0; i < children.size(); i++) {
      SET_VECTOR_ELT(out, i, wrap_node_raw(children[i]));
    }

    SEXP cls = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(cls, 0, Rf_mkChar("pugi_nodeset"));
    SET_STRING_ELT(cls, 1, Rf_mkChar("list"));
    Rf_classgets(out, cls);

    UNPROTECT(2);
    return out;
  }

  SEXP pugi_node_length(SEXP node_ptr) {
    if (node_ptr == R_NilValue) return Rf_ScalarInteger(0);
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    pugi::xml_node target = *node;

    if (target.type() == pugi::node_document) {
      for (pugi::xml_node child : target.children()) {
        if (child.type() == pugi::node_element) {
          target = child;
          break;
        }
      }
    }

    int count = 0;
    for (pugi::xml_node child : target.children()) {
      if (child.type() == pugi::node_element) count++;
    }
    return Rf_ScalarInteger(count);
  }

  SEXP pugi_set_attr(SEXP node_ptr, SEXP name_str, SEXP val_str) {
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* name = CHAR(STRING_ELT(name_str, 0));
    const char* val = CHAR(STRING_ELT(val_str, 0));

    pugi::xml_attribute attr = node->attribute(name);
    if (attr) attr.set_value(val);
    else node->append_attribute(name).set_value(val);

    return R_NilValue;
  }

  SEXP pugi_get_attr(SEXP node_ptr, SEXP name_str) {
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* name = CHAR(STRING_ELT(name_str, 0));
    return Rf_mkString(node->attribute(name).value());
  }

  SEXP pugi_set_text(SEXP node_ptr, SEXP text_str) {
    if (Rf_isNull(node_ptr)) return R_NilValue;
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* text = CHAR(STRING_ELT(text_str, 0));
    node->text().set(text);
    return R_NilValue;
  }

  SEXP pugi_node_name(SEXP node_ptr) {
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    return Rf_mkString(node->name());
  }

  SEXP pugi_serialize_node(SEXP node_ptr) {
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    if (!node || !(*node)) return Rf_mkString("");

    std::stringstream ss;
    node->print(ss, "", pugi::format_raw | pugi::format_no_declaration);
    return Rf_mkString(ss.str().c_str());
  }

  SEXP pugi_node_type(SEXP node_ptr) {
    if (node_ptr == R_NilValue) return Rf_mkString("missing");
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    if (node->type() == pugi::node_document) return Rf_mkString("document");
    if (node->type() == pugi::node_element) return Rf_mkString("element");
    return Rf_mkString("other");
  }

  SEXP pugi_has_attr(SEXP node_ptr, SEXP name_str) {
    pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    const char* name = CHAR(STRING_ELT(name_str, 0));
    return Rf_ScalarLogical(!node->attribute(name).empty());
  }

} // extern "C"
