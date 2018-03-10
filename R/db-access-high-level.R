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
#' ?id rdfs:label %label
#' }"
#' grapdhb_lookup_id = query_function_constructor(p_query, access_options = graphdb)
#'
#' @export
query_function_constructor = function(p_query, access_options, prefixes = NA)
{
  # add prefixes to beginning
  p_query = paste(prefix_serializer_sparql(prefixes))
  params = list()   # 1. find what are the parameters (i.e. % strings)

  f = function(params)   # 2. This list will be the function arguments
  {
    # 3. The function itself will replace the parameters in the p_query with values of its arguments
    # for this I can look at the implementation of lookup_id in ropenbio
    # 4. The function will connect to the database and submit the query
  }





}



openbiodiv_lookup_id = query_function_constructor(s, lookup_pquery, prefix_list)

openbiodiv_lookup_id(label = "Teodor Georgiev")


```
"SELECT *
WHERE {
?id rdfs:label "Teodor Georgiev"
}
