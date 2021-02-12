#' Convert a parameterized SPARQL query to an R function
#'
#' The function `query_factory`'s purpose is to manufacture a function
#' executing a specified SPARQL query against a specified endpoint.
#'
#' @param p_query character. A parameterized SPARQL query. Parameters are
#'   given with percent sign in front
#' @param submit_function function. The function that should be used to submit the
#'   query - i.e. whether to use the READ endpoint or the UPDATE endpoint.
#'   One of \code{submit_sparql}, or \code{submit_sparql_update}. The
#'   detault is \code{submit_sparql} - i.e. use the READ endpoint.
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
#' drop_query = "DROP GRAPH %subgraph"
#'
#' simple_lookup = query_factory(p_query, access_options = graphdb)
#' simplest_f = query_factory(p_query2, access_options = graphdb)
#' drop_g = query_factory(drop_query, submit_function = submit_sparql_update, access_options = graphdb_secret)
#'
#' @export
query_factory = function(p_query, submit_function = submit_sparql, access_options, prefixes = NA, ...)
{
  # add prefixes to beginning of the query and flatten
  p_query = c(prefix_serializer(prefixes), p_query, sep = "\n\n")
  p_query = do.call(paste, p_query)

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
    submit_function(
      gsubfn::gsubfn(pattern = "%[[:alnum:]_]+",
                     replacement = replacement,
                     x = p_query),
      access_options = access_options, ...
    )
  }

  # manipulate function signature
  signature = rep(alist(x = ), length(params))
  names(signature) = params
  formals(f) = signature

  f
}









#' Create a function that submits serialized RDF to a specific endpoint
#'
#' TODO : use the serializer from the RDF object and no need for the prefix argument
#'
#' Wraps \code{add_data} to simply submit a Turtle/Trig file to a triplestore
#'
#' @param access_options
#' @param prefixes
#'
#' @return a function with one parameter, \code{rdf_data}
#' @export
#'
#'
#'
#' @examples
#' \dontrun{
#' prefixes = c(rdfs = "<http://www.w3.org/2000/01/rdf-schema#>", foaf = "<http://xmlns.com/foaf/0.1/>", openbiodiv = "<http://openbiodiv.net/>")
#' ttl = "openbiodiv:test_context {
#' openbiodiv:test1 rdf:label 'sample lab'@en .
#' }"
#' add_data_to_graphdb = add_data_factory(access_options = graphdb, prefixes = prefixes)
#'
#' add_data_to_graphdb(rdf_data = ttl)
#' }
#'
#'
#'
add_data_factory = function(access_options, prefixes)
{
  function(rdf_data) {
    # prefix manipulation
    rdf_data = c(prefix_serializer(prefixes, lang = "Turtle"), rdf_data, sep = "\n\n")
    # want to flatten the data
    add_data(do.call(paste, as.list(rdf_data)), access_options = access_options)
  }
}

#'
#' The function `add_trig_file_to_graphdb` retrieves the rdf data from a trig file and inserts each line into an element of a list.
#' Then, this list is imported to graphdb triplestore using rdfr4::add_factory() functtion.
#'
#' @param graphdb_access_options graphdb triplestore access options.
#' @param prefixes named character
#' @param trig_file file name of the rdf format containing context (named graph).
#'
#' @export
add_trig_file_to_graphdb = function(graphdb_access_options,prefixes,trig_file){
  if(missing(trig_file)){
    return ("File missing")
  } else if(!file.exists(trig_file)) {
    return (paste0("File ",trig_file," not found."))
  } else {
    rdf_data <- scan(trig_file, what = "", sep = "\n")
  }
  add_data_to_graphdb = add_data_factory(access_options = graphdb_access_options, prefixes = prefixes)
  out <- add_data_to_graphdb(rdf_data = rdf_data)
  return(out)
}
