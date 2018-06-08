#' Basic Triplestore Access
#'
#' Create an object with the access options for a triple-store. If successful it also outputs to the screen the protocol version. If unsuccessful it raises an error. Uses basic authentication. If user and password are not supplied, the repository is asumed to be open.
#'
#' @seealso \code{api_triplestore_access}
#'
#' @param server_url character. The URL of the triplestore.
#' @param repository character. The ID of the repository to which you want to connect to.
#' @param user character. If authentication is needed, the username.
#' @param password character. If authentication is needed, the password.
#'
#' @return list. Contains the server access options.
#'
#' @examples
#' graphdb = basic_triplestore_access(server_url = "http://graph.openbiodiv.net:7777/", repository = "obkms_i7", user = "dbuser", password = "public-access")
#' graphdb2 = basic_triplestore_access(server_url = "http://graph.openbiodiv.net:7777/", repository = "obkms_i6")
#'
#' @export
basic_triplestore_access = function(server_url, repository = NA, user = NA, password = NA)
{
  authenticate = function(user, password) {
    if (is.na(user) || is.na(password)) {
      return (NULL)
    }
    else {
      httr::authenticate(user = user, password = password, type = "basic")
    }
  }
  server_access_options = list(
    server_url = strip_trailing_symbol(server_url),
    repository = repository,
    authentication = authenticate(user, password),
    status = NA
  )

  server_access_options$status = get_protocol_version(server_access_options)
  class(server_access_options) = append(class(server_access_options), "triplestore_access_options")
  return(server_access_options)
}









#' API Triplestore Access
#'
#' Create an object with the access options for a triple-store. If successful it also outputs to the screen the protocol version. If unsuccessful it raises an error. Uses API authentication.
#'
#' @seealso \code{api_triplestore_access}
#'
#' @param server_url character. The URL of the triplestore.
#' @param repository character. The ID of the repository to which you want to connect to.
#' @param api_key a string, the API key used for API-style authentication.
#' @param secret a string, the secret string corresponding to the API key
#'   needed for API-style authentication.
#'
#' @return list. Contains the server access options.
#'
#' @examples
#' graphdb3 = api_triplestore_access(server_url = "https://rdf.ontotext.com/4135593934/openbiodiv", repository = "test", api_key = "s4bb1d43uc52", api_secret = "d7h7eg4e263ghss")
#'
#' @export
api_triplestore_access = function(server_url, repository = NA, api_key = "", api_secret = "")
{
  parsed_url = unlist(strsplit(server_url, "//"))
  basic_triplestore_access(
    paste0(parsed_url[1], "//", api_key, ":", api_secret, "@", parsed_url[2]), repository
    )
}









#' Get Protocol Version
#'
#' Test connectivity to the graph database and get the communication protocol
#' version.
#'
#' This function tests the connectivity to the graph database. If there is connectivity it will return the protocol version as an integer. If there is no connectivity, an error will be raised.
#'
#' @param access_options list containing the graph database connectivity options,                returned by the helper function \code{basic_triplestore_access} or \code{api_triplestore_access}
#'
#' @return integer containing the protocol version if connectivity is OK.
#'
#' @seealso \code{basic_triplestore_access} in order to see how to create the connection options for a triple store with basic HTTP authentication and \code{api_access_options} for API-based authentication to a triplestore
#'
#' @examples
#' get_protocol_version(graphdb)
#'
#' @export
get_protocol_version = function(access_options)
{
  response_text = httr::content(
    httr::GET(url = paste(access_options$server_url, "/protocol", sep = ""),
              config = access_options$authentication)
  )
  if (is.na(as.integer(response_text))) {
    stop(response_text)
  }
  else {
    return(as.integer(response_text))
  }
}







#' List Repositories
#'
#' @param access_options object returned from \code{basic_triplestore_access} or
#'   \code{api_triplestore_access}
#'
#' @return data.frame. Contains repository information for that access point.
#'
#' @examples
#' list_repositories(graphdb)
#'
#' @export
list_repositories = function(access_options)
{
  result = xml2::read_xml(httr::GET(url = paste0(access_options$server_url, "/repositories"),
                          httr::add_headers(Accept = "application/sparql-results+xml, */*;q=0.5"),
                          access_options$authentication))
  uri = xml2::xml_text(xml2::xml_find_all(result, xpath = "//d1:results/d1:result/d1:binding[@name = 'uri']/d1:uri"))
  id = xml2::xml_text(xml2::xml_find_all(result, xpath = "//d1:results/d1:result/d1:binding[@name = 'id']/d1:literal"))
  #title = xml2::xml_text(xml2::xml_find_all(result, xpath = "//d1:results/d1:result/d1:binding[@name = 'title']/d1:literal"))
  readable = xml2::xml_text(xml2::xml_find_all(result, xpath = "//d1:results/d1:result/d1:binding[@name = 'readable']/d1:literal"))
  writable = xml2::xml_text(xml2::xml_find_all(result, xpath = "//d1:results/d1:result/d1:binding[@name = 'writable']/d1:literal"))
  data.frame(uri, id, title, readable, writable, stringsAsFactors = FALSE)
}









#' Submit a SPARQL Query to a Triplestore (READ)
#'
#'
#' In case of error, no execution-aborting condition is set! Instead an NA
#' is returned and a warning may be issued.
#'
#' This submits to the \code{/repositories/} endpoint, which means that only
#' certain types (READ) of SPARQL queries are allowed. Namely SELECT,
#' CONSTRUCT, DESCRIBE, and ASK. For UPDATE-type operations use the
#' \code{submit_sparql_update} function.
#'
#' Supported operations:
#'
#' SELECT
#' CONSTRUCT
#' DESCRIBE
#' ASK
#'
#'@seealso \code{submit_sparql_update}
#'
#'@param query character. Properly formatted SPARQL query to be submitted
#'  to an endpoint.
#'@param access_options object returned by \code{basic_triplestore_access}
#'  or \code{api_triplestore_access}.
#'@param as_dataframe logical. TRUE by default. If TRUE, the results are
#'  returned as a data.frame.
#'
#'@return data.frame or object returned by the triplestore. NA if nothing
#'  found or query could not be executed.
#'
#' @examples
#' query = "SELECT * WHERE {
#'   ?s ?p ?o .
#' } LIMIT 100"
#'
#' submit_sparql(query = query, access_options = graphdb)
#'
#'@export
submit_sparql = function(query, access_options, as_dataframe = TRUE)
{

  result = httr::POST(
    url = paste0(access_options$server_url, "/repositories/", access_options$repository),
    access_options$authentication,
    httr::add_headers(Accept = "text/csv, */*;q=0.5"),
    httr::add_headers('Content-Type' = "application/x-www-form-urlencoded; charset=utf-8"),
    body = list(query = query),
    encode = "form"
  )
  if(as_dataframe) {
    return (read.csv(textConnection(httr::content(result, as = "text")), stringsAsFactors = FALSE))
  }
  else {
    return (result)
  }
}









#' Submit a SPARQL Query to a Triplestore (UPDATE)
#'
#' This submits an UPDATE (in the sense of the CRUD methodology) to a
#' SPARQL endpoint. For READ queries see \code{submit_sparql}.
#'
#' Here are the SPARQL operations that are supported:
#'
#' LOAD
#' CLEAR
#' DROP
#' ADD
#' MOVE
#' COPY
#' CREATE
#' INSERT
#' DELETE
#'
#' @seealso \code{submit_sparql}
#'
#' @param query \code{character} the SPARQL UPDATE query. See Details.
#' @param access_options \code{triplestore_access_options} object.
#'
#' @return
#'
#' @examples
#' drop_query = "DROP GRAPH <http::/openbiodiv.net/test123>"
#'
#' submit_sparql_update(drop_query, access_options = graphdb)
#'
#' @export
submit_sparql_update = function(query, access_options) {
  query = do.call(paste, as.list(query))
  # Undocumented BUG in GraphDB needs us to have two slashes `//`
  endpoint = paste(access_options$server_url, "//repositories/",
                   access_options$repository, "/statements", sep = "")
  httr::content(httr::POST(
    url = endpoint,
    #httr::content_type("application/x-www-form-urlencoded"),
    access_options$authentication,
    body = list(update = query)
  ), as = 'text')
}







#' Add Data to a Repository
#'
#' @param rdf_data character. RDF data containing the triples to be submitted.
#' @param access_options output of \code{basic_triplestore_access} or \code{api_triplestore_access}
#' @param data_format currently only `application/x-trig` is supported. A variant of Turtle.
#'
#' @return the response of the `httr::POST` against the endpoint
#'
#' @examples
#' rdf_data = c("<http://test.net/mytest1> {
#'   <http://test.net/mysubject2> <http://test.net/has-value> '5' .
#' }")
#' add_data(rdf_data, access_options = graphdb)
#'
#' @export
add_data = function(rdf_data, access_options, data_format = "application/x-trig")
{
  # Undocumented BUG in GraphDB needs us to have two slashes `//`
  endpoint = paste(access_options$server_url, "//repositories/",
                   access_options$repository, "/statements", sep = "")
  httr::content(httr::POST(url = endpoint, access_options$authentication, httr::content_type(data_format), body = rdf_data), as = 'text')
}


