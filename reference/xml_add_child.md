# Add a child node to an XML target

Add a child node to an XML target

## Usage

``` r
xml_add_child(.x, .name, ..., .where = -1, .value = NULL, .escape = TRUE)
```

## Arguments

- .x:

  A pugi_node, pugi_xml, or a list containing one.

- .name:

  The name of the new tag to create.

- ...:

  Named arguments for attributes, unnamed for text content.

- .where:

  Integer; 0 to prepend, -1 to append.

- .value:

  Optional character string to set as text content.

- .escape:

  Logical; `FALSE` skips the escaping of attribute values and text, for
  values that are known to need none, e.g. numbers.

## Value

The newly created pugi_node.
