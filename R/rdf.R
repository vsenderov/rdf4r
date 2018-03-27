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
#' @export

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
      if (private$last_item == 0) {
        return (list())
      }
      private$dynamic_vector[1:private$last_item]
    }
  )
)








#' @export
RDF = R6::R6Class(
  classname = "rdf",
  inherit = DynVector,

  private = list(
    serialization = NULL,
    write_couplet = function(subject)
    {
      {
        private$turtle$add(c(paste(subject, " ")))
        # subset the triples with only this subject
        triples = lapply(private$dynamic_vector, function (t) {
          if (!is.null(t) && t[[1]]$qname == subject )
            return (t)
        })
        triples = triples[!sapply(triples,is.null)]
        # find the unique predicates

      }

    }
  ),

  public = list(
    initialize = function(size = 12)
    {
      super$initialize(size = size)
      private$serialization = DynVector$new(size) # will store the serialization
    },

    add_triple = function(subject, predicate, object)
    {
      if (!is.identifier(subject) || !is.identifier(predicate) || !is.list(object)) {
        return (FALSE);
      }
      else {
        self$add(list(subject = subject, predicate = predicate, object = object))
        return(TRUE)
      }
    },

    serialize = function(context) {
       private$turtle$add(c(paste(context, "{\n")))
       # qnames of subjects and kick out NULL
       subjects = sapply(
         private$dynamic_vector, function (t) {
          t[[1]]$qname
         }
        )
       subjects = subjects[!sapply(subjects,is.null)]
       for (s in unique(subjects)) {
         private$write_couplet(subject = s)
         private$turtle$add(". \n")
       }
       private$turtle$add(". }")
       return (private$turtle)
     }
  )
)



