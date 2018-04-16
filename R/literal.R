#' Literal Construction
#'
#' \code{literal} constructs a \code{literal} object.
#'
#' If for some reason the supplied text value is meaningless (empty string,
#' just spaces, invalid type, etc.), the returned object is the equivalent
#' of NA
#'
#' @param text_value \code{character} The textual value of literal.
#' @param xsd_type \code{identifier} The XSD type.
#' @param lang \code{character} The language code.
#'
#' @return a \code{literal} object, which is a \code{list} with the
#'   following fields:\itemize{
#'   \item{\code{text_value} \code{character} The textual value of literal.}
#'   \item{\code{xsd_type} \code{identifier} The XSD type.}
#'   \item{\code{lang} \code{character} The language code.}
#'   \item{\code{squote} \code{character} The value with added quotes and
#'    types,  e.g. "'John'^^xsd:string"}}
#' @export
#'
#' @examples
#'
#' lking_lear = literal(text_value = "King Lear", lang = "en")
#' lshakespeare = literal(text_value = "Shakespeare")
#' l1599 = literal(text_value = "1599", xsd_type = xsd_integer)
#'
#' lking_lear
#' lshakespeare
#' l1599
#'
#' # see vignette
literal = function(text_value, xsd_type, lang)
{
  if (!has_meaningful_value(text_value)) {
    text_value = as.character(NA)
  }

  text_value = text_value[1]

  if (!missing(lang) && has_meaningful_value(lang)) {
    xsd_type = xsd_string
    postfix = paste0("@", lang)
  }
  else if (!missing(xsd_type) && !is.null(xsd_type)) {
    lang = ""
    postfix = paste0("^^", xsd_type$qname)
  }
  else {
    xsd_type = xsd_string
    lang = ""
    postfix = ""
  }

  ll = list(
    text_value = text_value,
    xsd_type = xsd_type,
    lang = lang,
    squote = paste0("\"", text_value, "\"", postfix)
  )

  class(ll) = "literal"

  return(ll)
}


#' Outputs a literal in a default way
#'
#' @param ll \code{literal}
#'
#' @return \code{character} default representation.
#' @export
print.literal = function(ll)
{
  print(ll$squote)
}

#' Outputs a literal in a default way (not print!)
#'
#' @param ll \code{literal}
#'
#' @return \code{character} default representation.
#' @export
represent.literal = function(ll)
{
  ll$squote
}



#' Is the object a literal?
#'
#' @param x object to check
#'
#' @return logical
#'
#' @export
is.literal = function(x)
{
  if ("literal" %in% class(x)) TRUE
  else FALSE
}
