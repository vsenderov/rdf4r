#' Query Function Factory
#'
#' The function `query_factory`'s purpose is to manufacture a function executing a specified SPARQL query against a specified endpoint.
#'
#' @param p_query character. A parameterized SPARQL query. Parameters are given with percent sign in front
#' @param access_options triplestore_access_options.
#' @param prefix named character
#'
#' @return the query function (closure)
#'
#' @examples
#' p_query = "SELECT DISTINCT ?id WHERE {
#'    ?id rdfs:label '%label'
#'  }"
#'
#'  p_query2 = "SELECT * WHERE {
#'    ?s ?p ?o
#'  } LIMIT 100"
#'
#'  drop_query = "DROP GRAPH %subgraph"
#'
#' simple_lookup = query_factory(p_query, access_options = graphdb)
#' simplest_f = query_factory(p_query2, access_options = graphdb)
#' drop_g = query_factory(drop_query, access_options = graphdb)
#'
#' @export
query_factory = function(p_query, access_options, prefixes = NA)
{
  # add prefixes to beginning of the query
  p_query = paste(prefix_serializer_sparql(prefixes), p_query, sep = "\n\n")

  # find what are the parameters (i.e. % strings)), remove %
  params = gsub("^%", "", unlist(regmatches(
    p_query,
    gregexpr("%[[:alnum:]_]+", p_query)
  )))

  # define the returning function; signature will be added later
  f = function(...){
    replacement = as.list(environment())[names(formals())] # takes argument list
    names(replacement) = pasteif('%', names(replacement), cond = length(replacement) > 0)
    # replace the parameters in the p_query with values of its arguments and
    #  connect to the database and submit the query
    submit_sparql(
      gsubfn::gsubfn(pattern = "%[[:alnum:]_]+",
                     replacement = replacement,
                     x = p_query),
      access_options = access_options
    )
  }

  # manipulate function signature
  signature = rep(alist(x = ), length(params))
  names(signature) = params
  formals(f) = signature

  f
}

