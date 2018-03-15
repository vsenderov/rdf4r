#' Construct Identifier from ID
#'
#' @param id character(1). The part of identifier after the prefix.
#' @param prefix named character(1). The prefix. See Examples.
#'
#' @return identifier object (a type of list).
#' @export
#'
#' @examples
#' sample_id = identifier("57d68e07-8315-4b30-9a8e-57226fd815d7",
#'   prefix = c(openbiodiv = "http://openbiodiv.net/"))
identifier = function(id, prefix)
{
  # sanity
  stopifnot(is.character(id) && is.character(prefix) && length(id) == 1 && length(prefix) == 1 && !is.null(names(prefix)[1]))
  list(
    id = id,
    uri = strip_angle(paste0(prefix, id), reverse = TRUE),
    qname = paste(names(prefix)[1], id, sep = ":"),
    prefix = prefix
  )
}
