#' Add a child node to an XML target
#'
#' @param .x A pugi_node, pugi_xml, or a list containing one.
#' @param .name The name of the new tag to create.
#' @param ... Named arguments for attributes, unnamed for text content.
#' @param .where Integer; 0 to prepend, -1 to append.
#' @param .value Optional character string to set as text content.
#' @return The newly created pugi_node.
#' @keywords internal
xml_add_child <- function(.x, .name, ..., .where = -1, .value = NULL) {
  target <- if (is.list(.x)) .x[[1]] else .x

  if (inherits(target, "pugi_xml") || .Call(C_pugi_node_type, target) == "document") {
    kids <- xml_find_all(target, "/*")
    if (length(kids) > 0) target <- kids[[1]]
  }

  new_node <- .Call(C_pugi_add_child, target, .name, as.integer(.where))

  args <- list(...)
  if (length(args) > 0) {
    arg_names <- names(args)
    for (i in seq_along(args)) {
      val <- as.character(args[[i]])
      if (is.null(arg_names) || arg_names[i] == "") {
        .Call(C_pugi_set_text, new_node, val)
      } else {
        .Call(C_pugi_set_attr, new_node, arg_names[i], val)
      }
    }
  }

  if (!is.null(.value)) {
    .Call(C_pugi_set_text, new_node, as.character(.value))
  }
  new_node
}

#' Find first match via XPath
#'
#' @param x A pugi_node or list of nodes.
#' @param xpath Character string containing XPath expression.
#' @return A pugi_node or list of nodes.
#' @keywords internal
xml_find_first <- function(x, xpath) {
  if (is.list(x)) return(lapply(x, xml_find_first, xpath = xpath))
  if (!grepl("^\\.|^/", xpath)) xpath <- paste0(".//", xpath)
  .Call(C_pugi_find_first, x, as.character(xpath))
}

#' Find all matches via XPath
#'
#' @param x A pugi_node or list of nodes.
#' @param xpath Character string containing XPath expression.
#' @return A pugi_nodeset (list of pugi_nodes).
#' @keywords internal
xml_find_all <- function(x, xpath) {
  if (is.list(x)) {
    res <- unlist(lapply(x, xml_find_all, xpath = xpath), recursive = FALSE)
    class(res) <- c("pugi_nodeset", "list")
    return(res)
  }
  if (!grepl("^\\.|^/", xpath)) xpath <- paste0(".//", xpath)
  .Call(C_pugi_find_all, x, as.character(xpath))
}

#' Get element children
#'
#' @param x A pugi_node or list of nodes.
#' @return A pugi_nodeset of child elements.
#' @keywords internal
xml_children <- function(x) {
  if (is.list(x)) {
    res <- unlist(lapply(x, function(node) .Call(C_pugi_children, node)), recursive = FALSE)
    class(res) <- c("pugi_nodeset", "list")
    return(res)
  }
  .Call(C_pugi_children, x)
}

#' Get node names
#'
#' @param x A pugi_node or list of nodes.
#' @return A character vector of tag names.
#' @keywords internal
xml_name <- function(x) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call(C_pugi_node_name, node))))
  # Unwrap document node to its root element, matching xml2 behaviour
  if (.Call(C_pugi_node_type, x) == "document") {
    kids <- xml_find_all(x, "/*")
    if (length(kids) > 0) return(.Call(C_pugi_node_name, kids[[1]]))
    return("")
  }
  .Call(C_pugi_node_name, x)
}

#' Get node types
#'
#' @param x A pugi_node or list of nodes.
#' @return A character vector (e.g., "element", "document").
#' @keywords internal
xml_type <- function(x) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call(C_pugi_node_type, node))))
  .Call(C_pugi_node_type, x)
}

#' Get attribute value
#'
#' @param x A pugi_node or list of nodes.
#' @param attr Character string of the attribute name.
#' @return A character vector of attribute values.
#' @keywords internal
xml_attr <- function(x, attr) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call(C_pugi_get_attr, node, attr))))
  .Call(C_pugi_get_attr, x, as.character(attr))
}

#' Set attribute value
#'
#' @param x A pugi_node or list of nodes.
#' @param attr Character string of the attribute name.
#' @param value The value to set (coerced to character).
#' @keywords internal
xml_set_attr <- function(x, attr, value) {
  if (is.list(x)) {
    invisible(lapply(x, function(node) .Call(C_pugi_set_attr, node, attr, as.character(value))))
  } else {
    .Call(C_pugi_set_attr, x, as.character(attr), as.character(value))
  }
}

#' Check for attribute existence
#'
#' @param x A pugi_node or list of nodes.
#' @param attr Character string of the attribute name.
#' @return A logical vector.
#' @keywords internal
xml_has_attr <- function(x, attr) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call(C_pugi_has_attr, node, attr))))
  .Call(C_pugi_has_attr, x, as.character(attr))
}

#' Get count of child elements
#'
#' @param x A pugi_node or list of nodes.
#' @return An integer vector of child counts.
#' @keywords internal
xml_length <- function(x) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call(C_pugi_node_length, node))))
  .Call(C_pugi_node_length, x)
}

#' Remove nodes from the tree
#'
#' @param x A pugi_node or list of nodes.
#' @keywords internal
xml_remove <- function(x) {
  if (is.list(x)) invisible(lapply(x, function(node) .Call(C_pugi_remove, node)))
  else if (!is.null(x)) .Call(C_pugi_remove, x)
}

#' @method as.character pugi_node
#' @export
as.character.pugi_node <- function(x, ...) .Call(C_pugi_serialize_node, x)

#' @method print pugi_node
#' @export
print.pugi_node <- function(x, ...) cat(as.character(x), "\n")

#' @method print pugi_nodeset
#' @export
print.pugi_nodeset <- function(x, ...) {
  n <- length(x)
  cat(sprintf("{pugi_nodeset (%d)}\n", n))
  if (n > 0) {
    for (i in seq_len(min(n, 20))) {
      cat(sprintf("[%d] %s\n", i, as.character(x[[i]])))
    }
    if (n > 20) cat("...\n")
  }
  invisible(x)
}
