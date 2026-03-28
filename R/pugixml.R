xml_add_child <- function(.x, .name, ..., .where = -1, .value = NULL) {
  target <- if (is.list(.x)) .x[[1]] else .x

  if (inherits(target, "pugi_xml") || .Call("pugi_node_type", target) == "document") {
    kids <- xml_find_all(target, "/*")
    if (length(kids) > 0) target <- kids[[1]]
  }

  new_node <- .Call("pugi_add_child", target, .name, as.integer(.where))

  args <- list(...)
  if (length(args) > 0) {
    arg_names <- names(args)
    for (i in seq_along(args)) {
      val <- as.character(args[[i]])
      if (is.null(arg_names) || arg_names[i] == "") {
        .Call("pugi_set_text", new_node, val)
      } else {
        .Call("pugi_set_attr", new_node, arg_names[i], val)
      }
    }
  }

  if (!is.null(.value)) {
    .Call("pugi_set_text", new_node, as.character(.value))
  }
  new_node
}

xml_find_first <- function(x, xpath) {
  if (is.list(x)) return(lapply(x, xml_find_first, xpath = xpath))
  if (!grepl("^\\.|^/", xpath)) xpath <- paste0(".//", xpath)
  .Call("pugi_find_first", x, xpath)
}

xml_find_all <- function(x, xpath) {
  if (is.list(x)) {
    res <- unlist(lapply(x, xml_find_all, xpath = xpath), recursive = FALSE)
    class(res) <- c("pugi_nodeset", "list")
    return(res)
  }
  if (!grepl("^\\.|^/", xpath)) xpath <- paste0(".//", xpath)
  .Call("pugi_find_all", x, xpath)
}

xml_children <- function(x) {
  if (is.list(x)) {
    res <- unlist(lapply(x, function(node) .Call("pugi_children", node)), recursive = FALSE)
    class(res) <- c("pugi_nodeset", "list")
    return(res)
  }
  .Call("pugi_children", x)
}

xml_name <- function(x) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call("pugi_node_name", node))))
  .Call("pugi_node_name", x)
}

xml_type <- function(x) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call("pugi_node_type", node))))
  .Call("pugi_node_type", x)
}

xml_attr <- function(x, attr) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call("pugi_get_attr", node, attr))))
  .Call("pugi_get_attr", x, attr)
}

xml_set_attr <- function(x, attr, value) {
  if (is.list(x)) {
    invisible(lapply(x, function(node) .Call("pugi_set_attr", node, attr, as.character(value))))
  } else {
    .Call("pugi_set_attr", x, attr, as.character(value))
  }
}

xml_has_attr <- function(x, attr) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call("pugi_has_attr", node, attr))))
  .Call("pugi_has_attr", x, attr)
}

xml_length <- function(x) {
  if (is.list(x)) return(unname(sapply(x, function(node) .Call("pugi_node_length", node))))
  .Call("pugi_node_length", x)
}

xml_remove <- function(x) {
  if (is.list(x)) invisible(lapply(x, function(node) .Call("pugi_remove", node)))
  else if (!is.null(x)) .Call("pugi_remove", x)
}

#' @export
as.character.pugi_node <- function(x, ...) .Call("pugi_serialize_node", x)

#' @export
as.character.pugi_xml <- function(x, ...) .Call("pugi_serialize_node", x)

#' @export
print.pugi_node <- function(x, ...) cat(as.character(x), "\n")

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
