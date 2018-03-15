#' Construct Identifier from ID
#'
#' @param id character(1). The part of identifier after the prefix.
#' @param prefix named character. Note if multiple prefixes are supplied,
#'   the URI will only use the first one. If there is no prefix supplied
#'   QNAME and URI will essentially be the same.
#'
#' @return identifier object (a type of list).
#' @export
#'
#' @examples
#' sample_id = identifier("57d68e07-8315-4b30-9a8e-57226fd815d7",
#'   prefix = c(openbiodiv = "http://openbiodiv.net/", test = "http://test.com"))
#'
#' sample_id2 = identifier("http://www.example.com/1")
identifier = function(id, prefix = NULL)
{
  id = id
  uri = strip_angle(paste0(prefix[1], id), reverse = TRUE)
  qname = pasteif(names(prefix)[1], id, sep = ":", cond = !is.null(prefix), return_value = uri)
  prefix = prefix[1]

  ll = list(id = id, uri = uri, qname = qname, prefix = prefix)
  class(ll) = "identifier"

  ll
}









#' Construct Identifier via Lookup
#'
#' @param label character(1). Parameter that will be passed to the lookup
#'   functions. See \code{...} for details.
#' @param prefixes named character. Contains the prefixes.
#' @param FUN list of \code{...}
#' @param ... (lookup) functions that will be executed in order to
#'   obtain the identifier. The functions should have one argument to which
#'   \code{label} will be assigned during the call. As soon as we have a
#'   unique match the function execution halts. If there is no match, a
#'   URI with the base prefix (the one indiciated by "_base") and a UUID
#'   will be generated.
#'
#' @return identifier object (a type of list)
#' @export
#'
#' @examples
lookup_identifier = function(label, prefixes, FUN = list(...), ...)
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
      browser()
      identifier(id, prefix = prefix)
    }
    fi = fi + 1
  }
  if (length(id) == 0) { #no unique solution has been found, generate
    id = uuid::UUIDgenerate()
  }
}
