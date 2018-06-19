#' Identifier Consturction
#'
#' This is the constructor function for objects of the \code{identifier}
#' class.
#'
#' An identifier in the semantic web is something that uniquely identifies
#' a resource. Identifiers can be represented as URI's (e.g.
#' \code{<http://example.com/id>}), or as QNAME's (e.g. \code{example:id}).
#'
#' The Semantic Web model also allows for resources to be anonymous, via
#' so-called blank nodes. We use identifiers whose QNAME prefix is an
#' underscore (e.g. \code{_:alice}).
#'
#' RDF4R stores identifiers as lists with the following fields:
#'
#' \code{sample_id = list(
#'   id  = "57d68e07-8315-4b30-9a8e-57226fd815d7",
#'   uri = "<http://openbiodiv.net/57d68e07-8315-4b30-9a8e-57226fd815d7>",
#'   qname = "openbiodiv:id",
#'   prefix = c(openbiodiv = "http://openbiodiv.net")
#' )}
#'
#' @param id \code{character}. Local ID, for example a UUID. The part of
#'   identifier after the prefix.
#'
#' @param prefix named \code{character}. The name corresponds to the
#'   prefix and the proper part to the namespace. Only the first element
#'   of the vector will be honored. If you don't supply a prefix, the ID
#'   will be treated as a URI and the QNAME and URI will be the same.
#'
#' @param blank optional \code{logical}. If you want to create a blank node.
#'
#' @return \code{identifier} object (a type of list).
#'
#' @examples
#'
#' a = identifier(
#'   id = "57d68e07-8315-4b30-9a8e-57226fd815d7",
#'   prefix = c(openbiodiv = "http://openbiodiv.net")
#' )
#'
#' b = identifier(
#'   id = "alice",
#'   blank = TRUE
#' )
#'
#' a
#' b
#'
#' @export
#' @family identifier functions
identifier = function(id, prefix = NA, blank = FALSE)
{
  if (blank == TRUE) {
    prefix = c("_" = "_")
  }

  if (length(id) != 1 || length(prefix) != 1|| length(names(prefix)) != 1) {
    warning("Arguments to `identifier` not of length 1 or missing names!
            Using first positions.")
  }

  id = strip_angle(id[1])
  if (!has_meaningful_value(id)) {return (NULL)}
  prefix = strip_angle(prefix[1])
  uri = strip_angle(
    pasteif(prefix[1], id, cond = (!is.na(prefix)), return_value = id),
    reverse = TRUE
  )
  qname =
    pasteif(names(prefix)[1], id, sep = ":", cond = !is.na(prefix), return_value = uri)

  if (blank == TRUE) {
    uri = qname
  }

  ll = list(id = id, uri = uri, qname = qname, prefix = prefix)
  class(ll) = "identifier"

  ll
}


#' Outputs an identifier in a default way
#'
#' @param id \code{identifier}
#'
#' @return \code{character} default representation.
#' @export
#' @family identifier functions
print.identifier = function(id)
{
  print(id$qname)
}


#' Outputs an identifier in a default way (not print)
#'
#' @param id \code{identifier}
#'
#' @return \code{character} default representation.
#' @export
#' @family identifier functions
#' @family representables
represent.identifier = function(id)
{
  id$uri
}







#' Identifier Constructor via a List of Lookup Functions
#'
#' @param fun \code{list} of lookup functions to be tried. The functions
#' should have the same arguments (see \code{...} below). As soon as we have
#' a unique match the function execution halts. If there is no match, a
#' URI with the base prefix (the one indiciated by "_base") and a UUID
#' will be generated.
#' @param ... arguments to be passed to every lookup function. The arguments
#'   must be "representable", i.e. either a literal or a identifier
#' @param prefixes Named \code{character} that contains the prefixes to be
#' used for identifier construction after each lookup.
#'
#' @export
fidentifier = function(fun, ...,  prefixes)
{
  # sanity
  stopifnot(is.character(prefixes))
  fi = 1
  partial_uri = character()
  while (fi <= length(fun)) {
    partial_uri = do.call(fun[[fi]], lapply(list(...), represent))[[1]]
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
  return(NA)
}









#' Manufacturing Identifier Constructors
#'
#' @inheritParams fidentifier
#'
#' @param def_prefix to use if all else fails
#'
#' @return an indetifier constructor function.
#'   The identifier constructor has two arguments. "label" is a list
#'   of argument lists to 'fidentifier', "generate" is a boolean of
#'   whether to generate
#'
#' @export
identifier_factory = function(fun, prefixes, def_prefix)
{
  function(label, generate = TRUE)
  {
    stopifnot(is.list(label))
    if(length(label) == 0) {
      return(NA)
    } else {
      for (l in label) {
        # is l an argument list or a single argument?
        if ((is.literal(l) || is.identifier(l))) {
          ii = do.call(fidentifier, list(fun = fun, l, prefixes = prefixes))
        }
        else {
          # l is an argument list, we just need to modify it
          l$fun = fun
          l$prefixes = prefixes
          ii = do.call(fidentifier, l)
        }
        if (!is.na(ii)) {
          return(ii)
        }
      }
    }
    if (is.na(ii) && generate == TRUE) {
      return(identifier(id = uuid::UUIDgenerate(), prefix = def_prefix))
    }
    return(NA)
  }
}








#' Is the object an identifier?
#'
#' @param x object to check
#'
#' @return logical
#'
#' @export
#' @family identifier functions
is.identifier = function(x)
{
  if ("identifier" %in% class(x)) TRUE
  else FALSE
}
