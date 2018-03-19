#' Identifier Consturction
#'
#' An identifier in the semantic web is something that uniquely identifies
#' a resource. Identifiers can be represented as URI's (e.g.
#' \code{<http://example.com/id>}), or as QNAME's (e.g. \code{example:id}).
#'
#' RDF4R stores identifiers as lists with the following fields:
#'
#' \code{
#' sample_id = list(
#'   id  = "57d68e07-8315-4b30-9a8e-57226fd815d7",
#'   uri = "<http://openbiodiv.net/57d68e07-8315-4b30-9a8e-57226fd815d7>",
#'   qname = "openbiodiv:id",
#'   prefix = c(openbiodiv = "http://openbiodiv.net")
#' )
#' }
#'
#' @param id character(1). The part of identifier after the prefix. Ror
#'   example, a UUID.
#' @param prefix named character. Note if multiple prefixes are supplied,
#'   the URI will only use the first one. If there is no prefix supplied
#'   QNAME and URI will essentially be the same.
#'
#' @return identifier object (a type of list).
#' @export
#'
#' @examples
#' sample_id = identifier("57d68e07-8315-4b30-9a8e-57226fd815d7",
#'   prefix = c(openbiodiv = "http://openbiodiv.net/",
#'   test = "http://test.com"))
#'
#' sample_id2 = identifier("http://www.example.com/1")
identifier = function(id, prefix = NULL)
{
  stopifnot(length(id) == 1)
  id = id
  uri = strip_angle(paste0(prefix[1], id), reverse = TRUE)
  qname = pasteif(names(prefix)[1], id, sep = ":", cond = !is.null(prefix), return_value = uri)
  prefix = prefix[1]

  ll = list(id = id, uri = uri, qname = qname, prefix = prefix)
  class(ll) = "identifier"

  ll
}









#' @describeIn identifier Construct Identifier via Lookup Function
#'
#' @param label character(1). Parameter that will be passed to the lookup
#'   functions. See \code{...} for details.
#' @param prefixes named character. Contains the prefixes.
#' @param def_prefix the prefix to be used if lookup fails.
#' @param FUN list of lookup functions to be tried. this can be omitted and
#'   instead the functions specified as additional arguments.
#' @param ... (lookup) functions that will be executed in order to
#'   obtain the identifier. The functions should have one argument to which
#'   \code{label} will be assigned during the call. As soon as we have a
#'   unique match the function execution halts. If there is no match, a
#'   URI with the base prefix (the one indiciated by "_base") and a UUID
#'   will be generated.
#'
#' @examples
#'
#' prefixes = c(
#'   rdfs = "http://www.w3.org/2000/01/rdf-schema#",
#'   foaf = "http://xmlns.com/foaf/0.1/",
#'   openbiodiv = "http://openbiodiv.net/"
#' )
#'
#' sample_id3 = fidentifier("Teodor Georgiev",
#'   prefixes = prefixes,
#'   def_prefix = c(openbiodiv = "http://openbiodiv.net/"),
#'   simple_lookup, simple_lookup)
#'
#' @export
fidentifier = function(label, ...,  FUN = list(...), prefixes, def_prefix )
{
  # sanity
  stopifnot(is.character(label) && length(label) == 1 && is.character(prefixes))
  fi = 1
  partial_uri = character()
  while (fi <= length(FUN)) {
    partial_uri = FUN[[fi]](label)[[1]]
    if (length(partial_uri) == 1) {
      # found a unique solution
      # try to find if we have a prefix match
      pi = sapply(prefixes, function(p) {
        grepl(paste0("^", p), partial_uri) # does the partial_uri begin with p
      })
      prefix = prefixes[pi] # we could have multiple matches or no match, but this is fine as the identifier constructor acocmodates both
      # if we did have at least one match we need to properly form the id
      id = gsub(paste0("^", prefix[1]), "" , partial_uri)
      # now call normal constructor
      return(identifier(id, prefix = prefix))
    }
    fi = fi + 1
  }
  # if we are here, no unique solution has been found
  return(identifier(uuid::UUIDgenerate(), prefix = def_prefix))
}









#' Manufacturing Identifier Constructors
#'
#' @inheritParams fidentifier
#'
#' @return an identifier constructor function with one parameter
#'   (\code{label})
#'
#' @examples
#'
#' openbiodiv_id = identifier_factory(simple_lookup,
#'   prefixes = prefixes,
#'   def_prefix = c(openbiodiv = "http://openbiodiv.net/"))
#'
#' openbiodiv_id("Teodor Georgiev")
#' openbiodiv_id("Pavel Stoev")
identifier_factory = function(...,  FUN = list(...), prefixes, def_prefix )
{
  function(label) {
    fidentifier(label, FUN = FUN, prefixes = prefixes, def_prefix = def_prefix, ...)
  }
}
