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
