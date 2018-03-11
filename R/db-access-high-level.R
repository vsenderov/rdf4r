#' Query Function Factory
#'
#' @param p_query character. A parameterized SPARQL query. Parameters are given with % in front (e.g. %label)
#' @param access_options triplestore_access_options.
#' @param prefix named character
#'
#' @return the query function (closure)
#'
#' @examples
#' p_query = "SELECT *
#'         WHERE {
#' ?id2 rdfs:label %label. ?id2 rdfs:label %label2.
#' }"
#'f = query_function_constructor(p_query, access_options = graphdb)
#'
#' @export
query_function_constructor = function(p_query, access_options, prefixes = NA)
{
  p_query = paste(prefix_serializer_sparql(prefixes), p_query, sep = "\n\n")  # add prefixes to beginning

  params = (regmatches(
    p_query,
    gregexpr("%[[:alnum:]_]+", p_query)
  )) # 1. find what are the parameters (i.e. % strings))

  f = function(replacement){
    browser()
    # 3. The function itself will replace the parameters in the p_query with values of its arguments
    gsubfn(pattern = "%[[:alnum:]_]+", replacement = replacement, x = p_query)
    # for this I can look at the implementation of lookup_id in ropenbio
    # 4. The function will connect to the database and submit the query
  }
}

