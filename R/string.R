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
#' @x a vector (not a list)
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
#' @retun a pasting function
#' @export
pasteconstr = function(sep) {
  function(...) {
    paste(sep = sep, ...)
  }
}





#' Paste If
#'
#' Pastes something only if the condition is met. Uses paste0.
#'
#' @param ... one or more R objects, to be passed to paste0 if cond is TRUE.
#' @param cond boolean, condition to be true.
#'
#' @return string
#' @example
#' pasteif("1", "st", cond = (3 < 2))
#'
#' @export
pasteif =
function(..., cond) {
  if (cond) {
    paste0(...)
  }
  else {
    return("")
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


#' Strip Trailing Slash If Present
#'
#' Works only on UNIX.
#'
#' @param x character vector.
#'
#' @return x without the trailing slash, if present
#'
#' @examples
#' strip_trailing_slash("/media/obkms/plazi-corpus-xml/")
#'
#'
#' @export
strip_trailing_slash =
function(x) {
  sub("/$", "", x)
}

