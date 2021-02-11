#' RDF4R: R Library for Working with RDF
#'
#' RDF4R provides the following facilities.
#'
#' @section Connection to a triple store:
#'
#' \itemize{
#'   \item \code{\link{basic_triplestore_access}}: connect to a triple store without a password or with a username and a password
#'   \item \code{\link{api_triplestore_access}}: connect to a triple store with an API key
#'   \item \code{\link{get_protocol_version}}:  Get Protocol Version
#'   \item \code{\link{list_repositories}}: list the repositories at an endpoint
#' }
#'
#' @section Working with repositories on a triple store:
#'
#' \itemize{
#'   \item \code{\link{submit_sparql}}: Submit a SPARQL Query to a Triplestore (READ)
#'   \item \code{\link{submit_sparql_update}}: Submit a SPARQL Query to a Triplestore (UPDATE)
#'   \item \code{\link{add_data}}: Add Data to a Repository
#' }
#'
#' @section Function factories to convert SPARQL queries, or data endpoints to R functions:
#'
#' \itemize{
#'   \item \code{\link{query_factory}}: Convert a parameterized SPARQL query to an R function
#'   \item \code{\link{add_data_factory}}: Create a function that submits serialized RDF to a specific endpoint
#' }
#'
#' @section Working with literals and identifiers:
#'
#' \itemize{
#'   \item \code{\link{literal}}: Family of functions for creating literals
#'   \item \code{\link{identifier}}: Family of functions for creating resource identifiers
#'   \item \code{\link{fidentifier}}: Identifier Constructor via a List of Lookup Functions
#'   \item \code{\link{identifier_factory}}: Manufacturing Identifier Constructors
#' }
#'
#' @section Prefix management:
#'
#' Prefixes are managed autmatically by RDF objects, so you probably wouldn't need to call the functions in this section manually.
#' \itemize{
#'   \item \code{\link{prefix_serializer}}: Serializes a Prefix Vector
#' }
#'
#' @section Creation and serialization of RDF:
#'
#' \itemize{
#'   \item \code{\link{ResourceDescriptionFramework}}: Mutable RDF Object
#'   \item \code{\link{AnonRDF}}: Create a list of RDF statements that all share the same blank subject node
#' }
#'
#' @section A basic vocabulary of semantic elements:
#'
#' \itemize{
#'   \item \code{\link{semantic_elements}}: Predifined resource identifiers from widely-spread ontologies
#' }


#' @section Function factories to convert SPARQL queries, or data endpoints to R functions:
#'
#' \itemize{
#'   \item \code{\link{add_trig_file_to_graphdb}}: Retrieves rdf data from a file and imports it to graphdb
#' }
#'

#'
#' @docType package
#' @name rdf4r
NULL
