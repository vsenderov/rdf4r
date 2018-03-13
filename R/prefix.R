#' Serializes the prefix database in SPARQL
#'
#' If `reqd_prefixes` is left missing, all prefixes will be returned.
#' The individual prefixes may or may not end with ":". If they don't,
#' it will be added during function execution.
#'
#' Note the base prefix should be denoted as "_base". Its seriazliation is then
#'
#' \code{PREFIX : partial_uri}
#'
#' @param prefixes named character. Contains prefixes. Names are the prefixes.
#' @param reqd a character vector of needed prefixes, can be missing, then take all.
#'
#' @examples
#' prefixes = c(rdfs = "http://www.w3.org/2000/01/rdf-schema#", foaf = "http://xmlns.com/foaf/0.1/", openbiodiv = "http://openbiodiv.net/")
#' prefix_serializer_sparql(prefixes, reqd = c("rdfs", "openbiodiv"))
#'
#' @export
prefix_serializer_sparql = function (prefixes, reqd = names(prefixes))
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
  # subset the prefixes by only the required
  prefixes = prefixes[match(reqd, names(prefixes))]
  # process what happens if prefixes is empty?
  #browser()
  serialization = sapply(prefixes, function (p)
  {
    i = which(prefixes == p)
    prefix_sparql_line(names(prefixes)[i], p)
  })

  return(serialization)
}



prefix_serializer_turtle = function() {
  # sub-function to format a sing prefix line as Turtle
  prefix_turtle_line = function( prefix, uri ) {
    if ( ! prefix == "_base" ) {
      paste0( "@prefix ", prefix, ": ", uri, " .\n" )
    }
    # base prefix case:
    else {
      paste0( "@prefix ", ": ", uri, " .\n" )
    }
  }

  paste0( serialization, prefix_turtle_line( p, all_prefixes[[p]] ) )
}


