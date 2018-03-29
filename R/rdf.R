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
ResourceDescriptionFramework = R6::R6Class(
  classname = "ResourceDescriptionFramework",
  inherit = DynVector,

  private = list(
    write_couplet = function(subject, triples)
    {
      local_serialization = DynVector$new(10)
      local_serialization$add(c(paste(subject, " ")))
      # subset the triples with only this subject
      triples = lapply(triples, function(t)
      {
        if (t$subject$qname == subject) return(t)
      })
      triples = triples[!sapply(triples,is.null)]
      # find the unique predicates
      predicates = (sapply(triples, function (t)
      {
        t$predicate$qname
      }))

      next_object = FALSE
      for (p in unique( predicates )) {
        predicate_stanza = private$write_predicate_stanza(p, triples)
        if (next_object == FALSE) {
          local_serialization$add_list(predicate_stanza$get())
          next_object = TRUE
        }
        else{
          local_serialization$add(c(";\n\t"))
          local_serialization$add_list(predicate_stanza$get())
        }
      }

      return(local_serialization)
    },

    write_predicate_stanza = function(predicate, triples)
    {
      local_serialization = DynVector$new(10)
      local_serialization$add(c(predicate, " "))
      # subset only for this predicate
      triples = lapply(triples, function (t) {
        if (t$predicate$qname == predicate) return (t)
      })
      triples = triples[!sapply(triples,is.null)]
      # We fucking do care about uniqueness of objects!!!!!!!
      objects = lapply(triples, function (t) {
        t$object
      })
      next_object = FALSE
      for (o in unique(objects) ) {
        end_stanza = private$write_end_stanza( o, triples )
        if (next_object == FALSE) {
          local_serialization$add_list(end_stanza$get())
          next_object = TRUE
        }
        else {
          local_serialization$add(c(", "))
          local_serialization$add_list(end_stanza$get())
        }
      }
      return (local_serialization)
    },

    write_end_stanza = function (object, triples)
    {
      local_serialization = DynVector$new(10)
      if (is.literal(object)) {
        local_serialization$add(object$squote)
      }
      else if (is.identifier(object)) {
        local_serialization$add(object$qname)
      }
      else {
        # object is RDF with blank nodes --> recursion
        local_serialization$add(c(" [ "))
        local_serialization$add_list(private$write_couplet(subject = blank_node, triples = object))
        local_serialization$add(c(" ] "))
      }
      return(local_serialization)
    }

  ),

  public = list(
    initialize = function(size = 12)
    {
      super$initialize(size = size)
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

    add_triples = function(ll)
    {
      if(!is.ResourceDescriptionFramework(ll)) return (FALSE)
      else {
        self$add_list(ll$get())
      }
    },

    serialize = function(context)
    {
      serialization = DynVector$new(length(private$dynamic_vector))
      serialization$add(c(paste(context, "{\n")))
      # qnames of subjects and kick out NULL
      subjects = sapply(self$get(), function(t)
      {
        t$subject$qname
      })

      next_object = FALSE
      for (s in unique(subjects)) {
        couplet = private$write_couplet(subject = s, triples = self$get())
        if (next_object == FALSE) {
          serialization$add_list(couplet$get())
          next_object = TRUE
        }
        else{
          serialization$add(c(". \n"))
          serialization$add_list(couplet$get())
        }
      }
      serialization$add(". }")
      return (unlist(serialization$get()))
    }
  )
)



AnonRDF = R6::R6Class(
  classname = "anonymous_rdf",
  inherit = ResourceDescriptionFramework,

  public = list(

    add_triple = function(predicate, object)
    {
      if (!is.identifier(predicate) || !is.list(object)) {
        return (FALSE);
      }
      else {
        super$add(list(subject = blank_node, predicate = predicate, object = object))
        return(TRUE)
      }
    },

    add_triples = function(ll)
    {
      if(!is.AnonRDF(ll)) return (FALSE)
      else {
        self$add_list(ll$get())
      }
    },

    serialize = function(context)
    {
      # you cannot serialize anonymous RDF
      return (FALSE)
    }
  )
)


#' Is the object an Triples List (RDF)?
#'
#' @param x object to check
#'
#' @return logical
#'
#' @export
is.ResourceDescriptionFramework = function(x)
{
  if ("ResourceDescriptionFramework" %in% class(x)) TRUE
  else FALSE
}

#' Is the object an Anonymous Triples List (RDF)?
#' @export
is.AnonRDF = function(x)
{
  if ("anonymous_rdf" %in% class(x)) TRUE
  else FALSE
}
