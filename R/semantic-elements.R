#' Predifined resource identifiers from widely-spread ontologies
#'
#' @family semantic elements
#' @name semantic_elements
NULL


#' XSD Date Type
#' @name xsd_date
#' @export
#' @family semantic elements
xsd_date = identifier(
  id = "date",
  prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
)

#' XSD String Type
#' @name xsd_string
#' @export
#' @family semantic elements
 xsd_string = identifier(
   id = "string",
   prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
 )

 #' XSD Integer Type
 #' @export
 #' @family semantic elements
 xsd_integer = identifier(
   id = "integer",
   prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
 )

#' XSD URI Type
#' @name xsd_uri
#' @export
#' @family semantic elements
xsd_uri = identifier(
  id = "anyURI",
  prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
)


#' Has Label Property
#' @name xsd_string
#' @export
#' @family semantic elements
 rdfs_label = identifier(
   id = "label",
   prefix = c(rdfs = "http://www.w3.org/2000/01/rdf-schema#")
)

#' RDF Type
#' @export
#' @family semantic elements
rdf_type = identifier(
  id = "type",
  prefix = c(rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
)

#' A blank node
#' @export
#' @family semantic elements
 blank_node = identifier(
   id = "_blank000"
 )

 blank_node$qname = ""
