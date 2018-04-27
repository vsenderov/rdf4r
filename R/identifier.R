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
#' @param ... arguments to be passed to every lookup function.
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
    partial_uri = fun[[fi]](...)[[1]]
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
#' @return a function. The first argument of the function is a list. Each list
#'   element is a list of arguments to be passed to the lookup function.
#'
#' @export
#' @examples
#'
#' openbiodiv_id = identifier_factory(simple_lookup,
#'   prefixes = prefixes,
#'   def_prefix = c(openbiodiv = "http://openbiodiv.net/"))
#'
#' openbiodiv_id("Teodor Georgiev")
#' openbiodiv_id("Pavel Stoev")
identifier_factory = function(fun, prefixes, def_prefix)
{
  function(label, generate = TRUE)
  {
    stopifnot(is.list(label))
    if(length(label) == 0) {
      ii = NA
    } else {
      for (l in label) {
        ii = do.call(fidentifier, list(fun = fun, l, prefixes = prefixes))
        if (!is.na(ii)) {
          return(ii)
        }
      }
    }
    if (is.na(ii) && generate == TRUE) {
      identifier(id = uuid::UUIDgenerate(), prefix = def_prefix)
    }
  }
}








#' Is the object an identifier?
#'
#' @param x object to check
#'
#' @return logical
#'
#' @export
is.identifier = function(x)
{
  if ("identifier" %in% class(x)) TRUE
  else FALSE
}
