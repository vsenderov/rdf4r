#' An Amortized Vector
#'
#' This object stores a preallocated list. You can add objects to the list
#' with \code{add} and add lists to the list with \code{add_list}. When the
#' objects to be added exceed the dimensions of the list, the list is re-
#' allocated with double the size. This amortizes the runtime if you keep
#' growing the dynamic vector.
#'
#' Use this object if you need an object with a mutable states that you
#' keep growing. An example with be a list of RDF triples.
#'
#' @method add x adds x
#'
#'
#'
#' @examples
#'
#' v = DynVector$new(3)
#' v$add("test")
#'

DynVector = R6::R6Class(
  classname = "dynamic_vector",

  private = list(
    dynamic_vector = NULL,
    last_item = 0
  ),

  public = list(

    initialize = function(size)
    {
      private$dynamic_vector = vector(mode = "list", length = size)
    },

    add = function(x)
    {
      current_item = private$last_item + 1
      if (current_item > length(private$dynamic_vector)) {
        # need to reallocate
        b = DynVector$new(length(private$dynamic_vector)*2)
        b$add_list(self$get())
        b$add(x)
        # is it possible to just replace self with b?
        private$dynamic_vector = b$get()
        private$last_item = length(private$dynamic_vector)
      }
      else {
        private$dynamic_vector[[current_item]] = x
        private$last_item = private$last_item + 1
      }
    },

    add_list = function(l)
    {
      end_list = private$last_item + length(l)
      if (end_list > length(private$dynamic_vector)) {
        #reallocate
        b = DynVector$new(length(private$dynamic_vector)*2)
        b$add_list(self$get())
        b$add_list(l)
        # is it possible to just replace self with b?
        private$dynamic_vector = b$get()
        private$last_item = length(private$dynamic_vector)
      }
      else {
        private$dynamic_vector[(private$last_item + 1):end_list] = l
        private$last_item = end_list
      }
    },

    get = function()
    {
      private$dynamic_vector[1:private$last_item]
    }
  )
)








#' @export
RDF = R6::R6Class(
  classname = "rdf",
  inherit = DynVector,
  public = list(
    add_triple = function(subject, object, predicate)
    {

    }
  )
)



