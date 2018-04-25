#' XSD Date Type
#' @name xsd_date
#' @export
xsd_string = identifier(
  id = "date",
  prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
)


#' XSD String Type
#' @name xsd_string
#' @export
 xsd_string = identifier(
   id = "string",
   prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
 )

 #' XSD Integer Type
 #' @export
 xsd_integer = identifier(
   id = "integer",
   prefix = c(xsd = "http://www.w3.org/2001/XMLSchema#")
 )


#' Has Label Property
#' @name xsd_string
#' @export
 rdfs_label = identifier(
   id = "label",
   prefix = c(rdfs = "http://www.w3.org/2000/01/rdf-schema#")
)

#' RDF Type
#' @export
rdf_type = identifier(
  id = "type",
  prefix = c(rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
)


 blank_node = identifier(
   id = "_blank000"
 )

 blank_node$qname = ""
