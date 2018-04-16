#' Last Token of Character Vector
#'
#' Calls `strsplit` and then returns the last token.
#'
#' @param x a character vector to extract last token from
#' @param split string or regex on which to do the split
#' @param ... further arguments to strsplit
#'
#' @return last token of the character vector
#'
#' @examples
#' last_token("http://tb.plazi.org/GgServer/xml/uno", "/")
#' last_token(c("http://tb.plazi.org/GgServer/xml/dos", "http://tb.plazi.org/GgServer/xml/tres"), "/")
#'
#' @export
last_token = function(x, split, ...) {
  intermediate = strsplit(x, split)
  sapply(intermediate, last)
}

#' Last Element of Vector
#'
#' @param x a vector (not a list)
#'
#' @return the last element of the vector
#'
#' @examples
#' x = c(1, 2, 3)
#' y = c("a", "b", "c")
#' last(x)
#' last(y)
#'
#' @export
last = function(x) {
  return(x[length(x)])
}


#' Paste Constructor
#' @param sep the separator that you want
#' @return a pasting function
#' @export
pasteconstr = function(sep) {
  function(...) {
    paste(sep = sep, ...)
  }
}





#' Paste If
#'
#' Pastes something only if the condition is met.
#'
#' @param ... one or more R objects, to be passed to paste0 if cond is TRUE.
#' @param sep default "" - equiv. to paste0
#' @param cond boolean, condition to be true.
#' @param return_value
#'
#' @return string. If condition is false returns \code{return_value}. default is NULL
#' @examples
#' pasteif("1", "st", cond = (3 < 2))
#'
#' @export
pasteif =
function(..., sep = "", cond, return_value = NULL) {
  if (cond) {
    paste(..., sep = sep)
  }
  else {
    return(return_value)
  }
}




#' Strip Extension From a Filename
#'
#' @param filename filename
#' @return the filename without the extension
#' @export
strip_filename_extension = function(filename) {
  gsub("\\.[^.]+$", "", filename)
}


#' Gets the filenmae extension
#'
#' @param filename
#'
#' @return extension
#'
#' @examples
#' get_filename_extension("https://zenodo.org/record/1163869/files/figure.png")
#'
#' @export
get_filename_extension = function(filename)
{
  m = regexec("\\.[^.]+$", filename)
  unlist(regmatches(filename, m))
}


#' Strip Trailing Slash or Colon If Present
#'
#' Symbols that are stripped if present: /, :
#'
#' Works only on UNIX.
#'
#' @param x character vector.
#'
#' @return x without the trailing slash, if present
#'
#' @examples
#' strip_trailing_symbol("/media/obkms/plazi-corpus-xml/")
#' strip_trailing_symbol("rdfs:")
#' strip_trailing_symbol("rdfs")
#'
#'
#' @export
strip_trailing_symbol =
function(x) {
  sub("(/|:)$", "", x)
}





#' Strips (or Adds) Angular Brackets from a URI if Present
#'
#' @param partial_uri the URI to strip angular brackets from
#' @param reverse FALSE, if true will put angular brakcets instead
#'
#' @return URI with stripped angular brackets around it
#'
#' @example
#' partial_uri = "http://example.com"
#' strip_angle(partial_uri)
#' strip_angle(partial_uri, reverse = TRUE)
#' uri = "<http://test.gov>"
#' strip_angle(uri)
#' strip_angle(uri, reverse = TRUE)
#'
#' @export
strip_angle = function (partial_uri , reverse = FALSE) {
  # check if angular brackets are present
  if (grepl("^<.*>$", partial_uri)) {
    uri = partial_uri # with brackets
    partial_uri = (substr(uri, 2, nchar(uri) - 1)) # without brackets
  }
  else {
    uri = gsub( "^(.*)$", "<\\1>", partial_uri ) # with brackets
  }
  # by now partial_uri has no angulars  and uri has angulars
  if (reverse) { # i.e. add brackets
    return (uri)
  }
  else {
    return(partial_uri)
  }
}


#' Defualt representation
#'
#' @param x
#'
#' @export
represent = function(x)
{
    UseMethod("represent", x)
}

