test_that("Node creation handles dots (...) as attributes correctly", {
  doc <- openxlsx2::read_xml('<root xmlns:a="http://example.com/a"/>')

  # Test multiple attributes of different types
  node <- xml_add_child(doc, "a:testNode",
                        val = "100",
                        name = "series_alpha",
                        visible = "true")

  output <- as.character(doc)

  expect_match(output, 'val="100"')
  expect_match(output, 'name="series_alpha"')
  expect_match(output, 'visible="true"')
  expect_match(output, "<a:testNode")
})

test_that("Vectorized xml_remove handles empty results gracefully", {
  doc <- openxlsx2::read_xml('<root><item id="1"/><item id="2"/></root>')

  # Case 1: Remove nothing (non-existent XPath)
  nothing <- xml_find_all(doc, "//nonexistent")
  expect_length(nothing, 0)
  expect_error(xml_remove(nothing), NA) # Should not error

  # Case 2: Remove all items via list
  items <- xml_find_all(doc, "//item")
  expect_length(items, 2)
  xml_remove(items)

  remaining <- xml_find_all(doc, "//item")
  expect_length(remaining, 0)
})

test_that("xml_add_child correctly distinguishes .value from attributes", {
  doc <- openxlsx2::read_xml("<root/>")

  # mix of attribute (val) and node text (.value)
  node <- xml_add_child(doc, "c:v", val = "hidden", .value = "42.5")

  output <- as.character(doc)

  # Should look like: <c:v val="hidden">42.5</c:v>
  expect_match(output, 'val="hidden"')
  expect_match(output, ">42.5</c:v>")
})

test_that("xml_children returns a list of external pointers", {
  doc <- openxlsx2::read_xml("<root><a/><b/><c/></root>")

  kids <- xml_children(doc)

  expect_type(kids, "list")
  expect_length(kids, 3)
})

test_that("Namespace prefixes are preserved during serialization", {
  # Vital for OpenXML/encharter
  raw_xml <- '<c:chartSpace xmlns:c="http://schemas.openxmlformats.org/drawingml/2006/chart"><c:chart/></c:chartSpace>'
  doc <- openxlsx2::read_xml(raw_xml)

  ser <- as.character(doc)

  # Pugi usually formats empty tags as <tag/> or <tag></tag>
  # We just care the prefix and tag name survive
  expect_match(ser, "<c:chart")
  expect_match(ser, "xmlns:c=")
})

test_that("Full Hierarchy Traversal (The encharter Workflow)", {
  xml_str <- "
  <c:chartSpace xmlns:c=\"http://schemas.openxmlformats.org/drawingml/2006/chart\">
    <c:chart>
      <c:plotArea>
        <c:lineChart>
          <c:ser><c:idx val=\"0\"/></c:ser>
          <c:ser><c:idx val=\"1\"/></c:ser>
        </c:lineChart>
      </c:plotArea>
    </c:chart>
  </c:chartSpace>"

  doc <- openxlsx2::read_xml(xml_str)

  # 1. Test xml_children dive (should get <c:chart>)
  kids <- xml_children(doc)
  expect_length(kids, 1)
  expect_equal(openxlsx2::xml_node_name(as.character(kids[[1]])), "c:chart")

  # 2. Test Deep XPath
  series <- xml_find_all(doc, "//c:ser")
  expect_length(series, 2)

  # 3. Test Vectorized Removal
  xml_remove(series)

  # 4. Verify removal via serialization
  res <- as.character(doc)
  expect_false(grepl("<c:ser", res))
})

test_that("Attribute handling parity", {
  doc <- openxlsx2::read_xml("<node/>")

  # Test our '...' logic again
  xml_add_child(doc, "child", id = "test_1", val = "99")

  res <- as.character(doc)
  expect_match(res, "id=\"test_1\"")
  expect_match(res, "val=\"99\"")
})

XML_SIMPLE  <- "<root><child id=\"1\">Hello</child><child id=\"2\">World</child></root>"
XML_ATTRS   <- "<item a=\"1\" b=\"two\" c=\"true\"/>"
XML_NESTED  <- "<a><b><c val=\"deep\"/></b><b><c val=\"other\"/></b></a>"
XML_EMPTY   <- "<root/>"
XML_UNICODE <- "<root><node>\u00e9\u00e0\u00fc</node></root>"

# ---- xml_name ---------------------------------------------------------------

test_that("xml_name returns root element name from document node", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  expect_equal(xml_name(doc), "root")
})

test_that("xml_name returns element name from element node", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  child <- xml_find_first(doc, ".//child")
  expect_equal(xml_name(child), "child")
})

test_that("xml_name works on a list of nodes", {
  doc      <- openxlsx2::read_xml(XML_SIMPLE)
  children <- xml_find_all(doc, ".//child")
  expect_equal(xml_name(children), c("child", "child"))
})

test_that("xml_name on empty root returns correct name", {
  doc <- openxlsx2::read_xml(XML_EMPTY)
  expect_equal(xml_name(doc), "root")
})

# ---- xml_type ---------------------------------------------------------------

test_that("xml_type returns 'document' for document node", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  expect_equal(xml_type(doc), "document")
})

test_that("xml_type returns 'element' for element node", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  child <- xml_find_first(doc, ".//child")
  expect_equal(xml_type(child), "element")
})

test_that("xml_type returns 'missing' for NULL", {
  expect_equal(xml_type(NULL), "missing")
})

# ---- xml_find_first ---------------------------------------------------------

test_that("xml_find_first finds nested element from document", {
  doc  <- openxlsx2::read_xml(XML_NESTED)
  node <- xml_find_first(doc, ".//c")
  expect_equal(xml_name(node), "c")
  expect_equal(xml_attr(node, "val"), "deep")
})

test_that("xml_find_first returns empty node for no match", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_find_first(doc, ".//nonexistent")
  # Should not error; name of an empty/null node is ""
  expect_equal(xml_name(node), "")
})

test_that("xml_find_first auto-prefixes bare tag names", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  # "child" without .// prefix should still work
  node <- xml_find_first(doc, "child")
  expect_equal(xml_name(node), "child")
})

test_that("xml_find_first works on element node, not just document", {
  doc   <- openxlsx2::read_xml(XML_NESTED)
  b     <- xml_find_first(doc, ".//b")
  c_node <- xml_find_first(b, ".//c")
  expect_equal(xml_attr(c_node, "val"), "deep")
})

# ---- xml_find_all -----------------------------------------------------------

test_that("xml_find_all returns all matching nodes", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//child")
  expect_length(nodes, 2)
  expect_s3_class(nodes, "pugi_nodeset")
})

test_that("xml_find_all returns empty list for no match", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//nothing")
  expect_length(nodes, 0)
})

test_that("xml_find_all on a list of nodes unions results", {
  doc  <- openxlsx2::read_xml(XML_NESTED)
  bs   <- xml_find_all(doc, ".//b")
  cs   <- xml_find_all(bs, ".//c")
  expect_length(cs, 2)
  expect_equal(xml_attr(cs, "val"), c("deep", "other"))
})

# ---- xml_children -----------------------------------------------------------

test_that("xml_children returns direct element children from document", {
  doc      <- openxlsx2::read_xml(XML_SIMPLE)
  children <- xml_children(doc)
  # Document unwraps to <root>, whose children are the two <child> nodes
  expect_length(children, 2)
  expect_s3_class(children, "pugi_nodeset")
  expect_equal(xml_name(children), c("child", "child"))
})

test_that("xml_children returns empty nodeset for leaf element", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  child <- xml_find_first(doc, ".//child")
  # <child> contains text only, no element children
  kids  <- xml_children(child)
  expect_length(kids, 0)
})

test_that("xml_children works on a list of nodes", {
  doc  <- openxlsx2::read_xml(XML_NESTED)
  bs   <- xml_find_all(doc, ".//b")
  kids <- xml_children(bs)
  # Each <b> has one <c> child → 2 total
  expect_length(kids, 2)
})

# ---- xml_length -------------------------------------------------------------

test_that("xml_length counts direct element children from document", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  expect_equal(xml_length(doc), 2L)
})

test_that("xml_length is 0 for leaf node", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  child <- xml_find_first(doc, ".//child")
  expect_equal(xml_length(child), 0L)
})

test_that("xml_length works on a list", {
  doc  <- openxlsx2::read_xml(XML_NESTED)
  bs   <- xml_find_all(doc, ".//b")
  lens <- xml_length(bs)
  expect_equal(lens, c(1L, 1L))
})

# ---- xml_attr / xml_set_attr / xml_has_attr ---------------------------------

test_that("xml_attr retrieves existing attribute", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//child")
  expect_equal(xml_attr(nodes[[1]], "id"), "1")
  expect_equal(xml_attr(nodes[[2]], "id"), "2")
})

test_that("xml_attr returns empty string for missing attribute", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_find_first(doc, ".//child")
  expect_equal(xml_attr(node, "nonexistent"), "")
})

test_that("xml_attr on a list returns character vector", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//child")
  expect_equal(xml_attr(nodes, "id"), c("1", "2"))
})

test_that("xml_set_attr creates new attribute", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_find_first(doc, ".//child")
  xml_set_attr(node, "class", "highlight")
  expect_equal(xml_attr(node, "class"), "highlight")
})

test_that("xml_set_attr updates existing attribute", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_find_first(doc, ".//child")
  xml_set_attr(node, "id", "99")
  expect_equal(xml_attr(node, "id"), "99")
})

test_that("xml_set_attr coerces numeric to character", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_find_first(doc, ".//child")
  xml_set_attr(node, "count", 42L)
  expect_equal(xml_attr(node, "count"), "42")
})

test_that("xml_set_attr on a list sets attribute on all nodes", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//child")
  xml_set_attr(nodes, "class", "item")
  expect_equal(xml_attr(nodes, "class"), c("item", "item"))
})

test_that("xml_has_attr returns TRUE for present attribute", {
  doc  <- openxlsx2::read_xml(XML_ATTRS)
  node <- xml_find_first(doc, ".//item")
  expect_true(xml_has_attr(node, "a"))
})

test_that("xml_has_attr returns FALSE for absent attribute", {
  doc  <- openxlsx2::read_xml(XML_ATTRS)
  node <- xml_find_first(doc, ".//item")
  expect_false(xml_has_attr(node, "z"))
})

test_that("xml_has_attr on a list returns logical vector", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//child")
  # Both have 'id', neither has 'foo'
  expect_equal(xml_has_attr(nodes, "id"),  c(TRUE, TRUE))
  expect_equal(xml_has_attr(nodes, "foo"), c(FALSE, FALSE))
})

# ---- xml_add_child ----------------------------------------------------------

test_that("xml_add_child appends new element by name", {
  doc     <- openxlsx2::read_xml(XML_EMPTY)
  new_kid <- xml_add_child(doc, "item")
  expect_equal(xml_name(new_kid), "item")
  expect_equal(xml_length(doc), 1L)
})

test_that("xml_add_child sets attributes from named ...", {
  doc  <- openxlsx2::read_xml(XML_EMPTY)
  node <- xml_add_child(doc, "item", id = "5", type = "x")
  expect_equal(xml_attr(node, "id"),   "5")
  expect_equal(xml_attr(node, "type"), "x")
})

test_that("xml_add_child sets text from unnamed ...", {
  doc  <- openxlsx2::read_xml(XML_EMPTY)
  node <- xml_add_child(doc, "cat", "Hello")
  xml_str <- as.character(doc)
  expect_match(xml_str, "Hello")
})

test_that("xml_add_child appends by default (where = -1)", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  xml_add_child(doc, "last")
  children <- xml_children(doc)
  expect_equal(xml_name(children[[length(children)]]), "last")
})

test_that("xml_add_child prepends when where = 0", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  xml_add_child(doc, "first", .where = 0L)
  children <- xml_children(doc)
  expect_equal(xml_name(children[[1]]), "first")
})

test_that("xml_add_child works on document node (unwraps to root)", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_add_child(doc, "appended")
  expect_equal(xml_name(node), "appended")
  # Child count of root should now be 3
  expect_equal(xml_length(doc), 3L)
})

test_that("xml_add_child copies an external node", {
  doc1 <- openxlsx2::read_xml(XML_SIMPLE)
  doc2 <- openxlsx2::read_xml(XML_EMPTY)
  src  <- xml_find_first(doc1, ".//child")
  xml_add_child(doc2, src)
  expect_equal(xml_length(doc2), 1L)
  expect_equal(xml_name(xml_children(doc2)[[1]]), "child")
})

# ---- xml_remove -------------------------------------------------------------

test_that("xml_remove detaches a node from the tree", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  expect_equal(xml_length(doc), 2L)
  node <- xml_find_first(doc, ".//child")
  xml_remove(node)
  expect_equal(xml_length(doc), 1L)
})

test_that("xml_remove on a list removes all matched nodes", {
  doc   <- openxlsx2::read_xml(XML_SIMPLE)
  nodes <- xml_find_all(doc, ".//child")
  xml_remove(nodes)
  expect_equal(xml_length(doc), 0L)
})

test_that("xml_remove on NULL is a no-op", {
  expect_no_error(xml_remove(NULL))
})

# ---- as.character / serialization -------------------------------------------

test_that("as.character produces valid XML string from element node", {
  doc  <- openxlsx2::read_xml(XML_SIMPLE)
  node <- xml_find_first(doc, ".//child")
  s    <- as.character(node)
  expect_type(s, "character")
  expect_match(s, "<child")
  expect_match(s, "Hello")
})

test_that("as.character on document node produces full XML", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  s   <- as.character(doc)
  expect_match(s, "<root>")
  expect_match(s, "</root>")
})

test_that("as.character round-trips: parse → serialize → parse", {
  doc1 <- openxlsx2::read_xml(XML_NESTED)
  s    <- as.character(doc1)
  doc2 <- openxlsx2::read_xml(s)
  # expect_equal(xml_name(doc2), "a")
  expect_equal(xml_length(doc2), 2L)
})

# ---- unicode ----------------------------------------------------------------

test_that("unicode content is preserved through round-trip", {
  skip_if_not(l10n_info()$`UTF-8`, "UTF-8 is not supported in this locale")
  doc  <- openxlsx2::read_xml(XML_UNICODE)
  node <- xml_find_first(doc, ".//node")
  s    <- as.character(node)
  expect_match(s, "\u00e9")
})

# ---- error handling ---------------------------------------------------------

test_that("xml_name errors on invalid handle", {
  expect_error(xml_name(42L), regexp = "external pointer|handle")
})

test_that("xml_attr errors on invalid handle", {
  expect_error(xml_attr(42L, "id"), regexp = "external pointer|handle")
})

test_that("xml_find_first errors on non-string xpath", {
  doc <- openxlsx2::read_xml(XML_SIMPLE)
  expect_error(xml_find_first(doc, 123L), regexp = "XPath error: Unrecognized node test")
})

test_that("xml_add_child errors on invalid node handle", {
  expect_error(xml_add_child(42L, "child"), regexp = "external pointer|handle")
})
