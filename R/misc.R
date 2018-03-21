#' Check whether supplied text argument has a meaningful value
#'
#' Meaningful value means:
#'
#' \itemize{
#'   \item{is not NULL}
#'   \item{is not an atomic type}
#'   \item{is not of length 0}
#'   \item{is not NA}
#'   \item{is not an empty string or just spaces}
#' }
#'
#' @param text_value object to check for a meaningful value
#'
#' @return FALSE, if the value is not meaningful; TRUE, otherwise
#'
#' @examples
#' has_meaningful_value("Hohn")
#' has_meaningful_value(1)
#' has_meaningful_value(list())
#' has_meaningful_value(list(1, "one"))
#' has_meaningful_value(c(1, "one"))
#' has_meaningful_value("              ")
#' has_meaningful_value("")
#' @export
has_meaningful_value = function (text_value)
{
  if (is.null(text_value)) return (FALSE)
  if (!is.atomic(text_value)) return (FALSE)
  if (length(text_value) == 0) return (FALSE)
  if (is.na(text_value)) return(FALSE)
  text_value = gsub("[ ]+", "", as.character(text_value))
  if (text_value == "") return  (FALSE)
  return(TRUE)
}
