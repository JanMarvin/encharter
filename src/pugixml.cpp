#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <pugixml/pugixml.hpp>
#include <sstream>
#include <memory>
#include <vector>
#include <cstdio>

// --- Safety Macros ---

#define CHECK_NODE(ptr)                                                     \
if (ptr == R_NilValue || TYPEOF(ptr) != EXTPTRSXP)                          \
  Rf_error("Invalid pugi_node handle: Object is not an external pointer."); \
pugi::xml_node* node = (pugi::xml_node*)R_ExternalPtrAddr(ptr);             \
if (!node) Rf_error("Invalid pugi_node handle: Pointer is NULL.");

#define CHECK_STRING(s, name)            \
if (!Rf_isString(s) || Rf_length(s) < 1) \
  Rf_error("Argument '%s' must be a non-empty character string.", name);

extern "C" {

  // --- Finalizer ---
  void pugi_node_finalizer(SEXP ext_ptr) {
    pugi::xml_node* ptr = (pugi::xml_node*)R_ExternalPtrAddr(ext_ptr);
    if (ptr) {
      delete ptr;
      R_SetExternalPtrAddr(ext_ptr, NULL);
    }
  }

  // --- Internal Helper ---
  SEXP wrap_node_raw(pugi::xml_node node, SEXP prot, bool is_doc = false) {
    SEXP ext_ptr = PROTECT(R_MakeExternalPtr(NULL, Rf_install("pugi_node"), prot));
    R_RegisterCFinalizerEx(ext_ptr, pugi_node_finalizer, TRUE);

    pugi::xml_node* ptr = new pugi::xml_node(node);
    R_SetExternalPtrAddr(ext_ptr, ptr);

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

  // --- Search & Navigation ---

  static bool try_select_first(pugi::xml_node* node, const char* xpath,
                               pugi::xml_node& out, char* errbuf, size_t errbuf_sz) {
    try {
      out = node->select_node(xpath).node();
      return true;
    } catch (const pugi::xpath_exception& e) {
      snprintf(errbuf, errbuf_sz, "XPath error: %s", e.what());
    } catch (const std::exception& e) {
      snprintf(errbuf, errbuf_sz, "error: %s", e.what());
    } catch (...) {
      snprintf(errbuf, errbuf_sz, "An unknown error occurred during XPath selection.");
    }
    return false;
  }

  SEXP pugi_find_first(SEXP node_ptr, SEXP xpath_str) {
    CHECK_NODE(node_ptr)
    CHECK_STRING(xpath_str, "xpath")
    const char* xpath = CHAR(STRING_ELT(xpath_str, 0));

    pugi::xml_node found;
    char errbuf[512];
    errbuf[0] = '\0';
    if (!try_select_first(node, xpath, found, errbuf, sizeof(errbuf))) {
      Rf_error("%s", errbuf);
    }
    return wrap_node_raw(found, node_ptr);
  }

  static bool try_select_all(pugi::xml_node* node, const char* xpath,
                             std::vector<pugi::xml_node>& out, char* errbuf, size_t errbuf_sz) {
    try {
      pugi::xpath_node_set nodes = node->select_nodes(xpath);
      out.reserve(nodes.size());
      for (size_t i = 0; i < nodes.size(); i++) {
        out.push_back(nodes[i].node());
      }
      return true;
    } catch (const pugi::xpath_exception& e) {
      snprintf(errbuf, errbuf_sz, "XPath error: %s", e.what());
    } catch (const std::exception& e) {
      snprintf(errbuf, errbuf_sz, "error: %s", e.what());
    } catch (...) {
      snprintf(errbuf, errbuf_sz, "An unknown C++ exception occurred in pugi_find_all.");
    }
    return false;
  }

  SEXP pugi_find_all(SEXP node_ptr, SEXP xpath_str) {
    CHECK_NODE(node_ptr)
    CHECK_STRING(xpath_str, "xpath")
    const char* xpath = CHAR(STRING_ELT(xpath_str, 0));

    std::vector<pugi::xml_node> found;
    char errbuf[512];
    errbuf[0] = '\0';
    if (!try_select_all(node, xpath, found, errbuf, sizeof(errbuf))) {
      Rf_error("%s", errbuf);
    }

    R_xlen_t n = (R_xlen_t)found.size();
    SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
    for (R_xlen_t i = 0; i < n; i++) {
      SET_VECTOR_ELT(out, i, wrap_node_raw(found[(size_t)i], node_ptr));
    }

    SEXP cls = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(cls, 0, Rf_mkChar("pugi_nodeset"));
    SET_STRING_ELT(cls, 1, Rf_mkChar("list"));
    Rf_classgets(out, cls);

    UNPROTECT(2);
    return out;
  }

  SEXP pugi_children(SEXP node_ptr) {
    CHECK_NODE(node_ptr)
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
      SET_VECTOR_ELT(out, i, wrap_node_raw(children[i], node_ptr));
    }

    SEXP cls = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(cls, 0, Rf_mkChar("pugi_nodeset"));
    SET_STRING_ELT(cls, 1, Rf_mkChar("list"));
    Rf_classgets(out, cls);

    UNPROTECT(2);
    return out;
  }

  SEXP pugi_node_length(SEXP node_ptr) {
    CHECK_NODE(node_ptr)
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

  // --- Modification ---

  SEXP pugi_add_child(SEXP node_ptr, SEXP input, SEXP where_int) {
    CHECK_NODE(node_ptr)
    int where = Rf_asInteger(where_int);
    pugi::xml_node last_added;

    if (TYPEOF(input) == VECSXP) {
      int n = Rf_length(input);
      for (int i = 0; i < n; i++) {
        SEXP item = VECTOR_ELT(input, i);
        if (TYPEOF(item) == EXTPTRSXP) {
          pugi::xml_node* input_node = (pugi::xml_node*)R_ExternalPtrAddr(item);
          if (input_node && *input_node) {
            last_added = (where == 0) ? node->prepend_copy(*input_node) : node->append_copy(*input_node);
          }
        }
      }
      return last_added ? wrap_node_raw(last_added, node_ptr) : node_ptr;
    }

    if (TYPEOF(input) == EXTPTRSXP) {
      pugi::xml_node* input_node = (pugi::xml_node*)R_ExternalPtrAddr(input);
      if (!input_node || !(*input_node)) return R_NilValue;
      last_added = (where == 0) ? node->prepend_copy(*input_node) : node->append_copy(*input_node);
      return wrap_node_raw(last_added, node_ptr);
    }

    if (Rf_isString(input) && Rf_length(input) > 0) {
      const char* name = CHAR(STRING_ELT(input, 0));
      last_added = (where == 0) ? node->prepend_child(name) : node->append_child(name);
      return wrap_node_raw(last_added, node_ptr);
    }

    return R_NilValue;
  }

  SEXP pugi_remove(SEXP node_ptr) {
    CHECK_NODE(node_ptr)
    if (node->parent()) node->parent().remove_child(*node);
    return R_NilValue;
  }

  // --- Attributes & Text ---

  SEXP pugi_set_attr(SEXP node_ptr, SEXP name_str, SEXP val_str) {
    CHECK_NODE(node_ptr)
    CHECK_STRING(name_str, "name")
    CHECK_STRING(val_str, "value")
    const char* name = CHAR(STRING_ELT(name_str, 0));
    const char* val = CHAR(STRING_ELT(val_str, 0));

    pugi::xml_attribute attr = node->attribute(name);
    if (attr) attr.set_value(val);
    else node->append_attribute(name).set_value(val);
    return R_NilValue;
  }

  SEXP pugi_get_attr(SEXP node_ptr, SEXP name_str) {
    CHECK_NODE(node_ptr)
    CHECK_STRING(name_str, "name")
    const char* name = CHAR(STRING_ELT(name_str, 0));
    return Rf_mkString(node->attribute(name).value());
  }

  SEXP pugi_has_attr(SEXP node_ptr, SEXP name_str) {
    CHECK_NODE(node_ptr)
    CHECK_STRING(name_str, "name")
    const char* name = CHAR(STRING_ELT(name_str, 0));
    return Rf_ScalarLogical(!node->attribute(name).empty());
  }

  SEXP pugi_set_text(SEXP node_ptr, SEXP text_str) {
    CHECK_NODE(node_ptr)
    CHECK_STRING(text_str, "text")
    const char* text = CHAR(STRING_ELT(text_str, 0));
    node->text().set(text);
    return R_NilValue;
  }

  // --- Metadata & Serialization ---

  SEXP pugi_node_name(SEXP node_ptr) {
    CHECK_NODE(node_ptr)
    return Rf_mkString(node->name());
  }

  SEXP pugi_serialize_node(SEXP node_ptr) {
    pugi::xml_node* ptr = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    if (!ptr || !(*ptr)) return Rf_mkString("");

    std::stringstream ss;
    ptr->print(ss, "", pugi::format_raw | pugi::format_no_declaration);
    return Rf_mkString(ss.str().c_str());
  }

  SEXP pugi_node_type(SEXP node_ptr) {
    if (node_ptr == R_NilValue || TYPEOF(node_ptr) != EXTPTRSXP) return Rf_mkString("missing");
    pugi::xml_node* ptr = (pugi::xml_node*)R_ExternalPtrAddr(node_ptr);
    if (!ptr || !(*ptr)) return Rf_mkString("missing");

    if (ptr->type() == pugi::node_document) return Rf_mkString("document");
    if (ptr->type() == pugi::node_element) return Rf_mkString("element");
    return Rf_mkString("other");
  }

} // extern "C"
