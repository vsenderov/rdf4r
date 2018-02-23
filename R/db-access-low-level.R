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
#'
#' TODO TEST NO AUTHENTICATION
#'
#' @export
basic_triplestore_access = function(server_url, repository, user = NA, password = NA)
{
  server_access_options = list(
    server_url = server_url,
    repository = repository,
    user = user,
    password = password,
    status = NA
  )

  server_access_options$status = get_protocol_version(server_access_options)

  return(server_access_options)
}









#' API Triplestore Access
#'
#' TODO: UNFINISHED
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
#'
#' @export
api_triplestore_access = function(server_url, repository, user = as.character(NA), password = as.character(NA))
{
  server_access_options = list(
      server_url = paste( protocol, api_key, ":", secret, "@", server_add, sep = ""),
      authentication = "api",
      api_key = api_key,
      secret = secret,
      repository = repository
    )

  server_access_options$status = get_protocol_version(server_access_options)

  return(server_access_options)
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
              config = httr::authenticate(access_options$user, access_options$password, "basic"))
  )
  if (is.na(as.integer(response_text))) {
    stop(response_text)
  }
  else {
    return(as.integer(response_text))
  }
}
