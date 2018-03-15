#' Serializes a Prefix Vector
#'
#' If \code{reqd} is left missing, all prefixes will be returned.
#' The individual prefixes may or may not end with ":". If they don't,
#' it will be added during function execution.
#'
#' Note the base prefix should be denoted as "_base". Its seriazliation is then
#'
#' \code{PREFIX : partial_uri}
#'
#' @param prefixes named character. Contains prefixes. Names are the prefixes.
#' @param reqd a character vector of needed prefixes, can be missing, then take all.
#' @param lang character. The serialization language. One of \code{"SPARQL"} or
#'   \code{"Turtle"}. Default is \code{"SPARQL"}
#'
#' @return prefix serialization.
#'
#' @examples
#' prefixes = c(
#'   rdfs = "http://www.w3.org/2000/01/rdf-schema#",
#'   foaf = "http://xmlns.com/foaf/0.1/",
#'   openbiodiv = "http://openbiodiv.net/"
#'  )
#' prefix_serializer(prefixes, reqd = c("rdfs", "openbiodiv"))
#' prefix_serializer(prefixes, reqd = c("rdfs", "openbiodiv"), lang = "Turtle")
#'
#' @export
prefix_serializer = function(prefixes, reqd = names(prefixes), lang = "SPARQL")
{
  # sub-function to format a single prefix line as SPARQL
  prefix_sparql_line = function(prefix, partial_uri) {
    if ( ! prefix == "_base" ) {
      paste0( "PREFIX ", prefix, ": ", partial_uri, " \n" )
    }
    # base prefix case:
    else {
      paste0( "PREFIX ", ": ", partial_uri, " \n" )
    }
  }
  # sub-function to format a sing prefix line as Turtle
  prefix_turtle_line = function(prefix, partial_uri) {
    if ( ! prefix == "_base" ) {
      paste0( "@prefix ", prefix, ": ", partial_uri, " .\n" )
    }
    # base prefix case:
    else {
      paste0( "@prefix ", ": ", partial_uri, " .\n" )
    }
  }
  #choose language
  if (lang == "Turtle") {
    line_function = prefix_turtle_line
  }
  else {
    line_function = prefix_sparql_line
  }

  # subset the prefixes by only the required
  prefixes = prefixes[match(reqd, names(prefixes))]
  # process what happens if prefixes is empty?
  #browser()
  serialization = sapply(prefixes, function (p)
  {
    i = which(prefixes == p)
    line_function(names(prefixes)[i], p)
  })

  return(serialization)
}
