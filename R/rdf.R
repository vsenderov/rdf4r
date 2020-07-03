#' Mutable RDF Object
#'
#' This is a mutable object. Adds, stores, and serializes RDF statements
#' efficiently. Uses an amortized vector \code{DynVector} as a internal
#' storage. You can inherit this class and override to create your own
#' implementation and still retain syntax compatibility.
#'
#' @param initialize(size=100) Constructor. Use this to create new objects
#'   it has a default tuning parameter. You may want to set it to the average
#'   number of triples per processed document.
#'
#' @param set_context(context) Context needs to be an \code{identifier}
#'   object that correspond to the named graph of where the statements
#'   are stored.
#'
#' @param get_prefixes() Returns the prefixes as a deduplicated named
#'   character vector of prefix to namespace mappings.
#'
#' @param add_triple(subject,predicate,object) \itemize{
#'   \item{\code{subject} \code{identifier}}
#'   \item{\code{predicate} \code{identifier}}
#'   \item{\code{object} \code{identifier} or \code{literal} or \code{AnonymousRDF}.
#'   Use the anonymous RDF if you hop through a blank node.}
#' } Returns the success status (\code{logical})
#'
#' @param add_triples(ll) ll needs to be a \code{ResourceDescriptionFramework}
#'   object. The information is merged.
#'
#' @param serialize() Returns the Turtle serialization.
#'
#' @param prefix_list \code{dynamic_vector} Prefixes of all the stored
#'   identifiers as an uncollapsed list. For most cases you would want
#'   to use \code{get_prefixes}.
#'
#' @param context \code{identifier}. The named graph of where the statements
#'   are stored.
#'
#'
#' @export
#' @family rdf
ResourceDescriptionFramework = R6::R6Class(
  classname = "ResourceDescriptionFramework",

  public = list(

    prefix_list = NULL, # DynVector
    context = NULL, # identifer

    initialize = function(size = 1000000)
    {
      private$triples = DynVector$new(size = size)
      self$prefix_list = DynVector$new(size = size)
    },

    set_context = function(context)
    {
      stopifnot(is.identifier(context))
      self$context = context
    },

    get_prefixes = function()
    {
      un = unlist(self$prefix_list$get())
      un[!duplicated(un)]
    },

    add_triple = function(subject, predicate, object)
    {
      #put this here bc some predicate prefixes are not added otherwise
      self$prefix_list$add(predicate$prefix)

      if (!is.identifier(subject) || !is.identifier(predicate) || !(is.literal(object) || is.identifier(object) || is.ResourceDescriptionFramework(object))) {
        return (FALSE)
      }
      else {
        self$prefix_list$add(subject$prefix)
        #self$prefix_list$add(predicate$prefix)
        if (is.identifier(object)) {
          self$prefix_list$add(object$prefix)
        }
        private$triples$add(list(subject = subject, predicate = predicate, object = object))
        return(TRUE)
      }
    },

    add_triples = function(ll)
    {

      if(!is.ResourceDescriptionFramework(ll)) return (FALSE)
      if(length(ll$get_list()) == 0) return (FALSE)
      else {
        self$prefix_list$add_list(ll$prefix_list$get())
        private$triples$add_list(ll$get_list())
      }
    },

    get_list = function()
    {
      private$triples$get()
    },

    serialize = function()
    {
      if (is.null(self$context)) {
        error("context not set. cannot serialize")
      }
      if (length(self$get_list()) == 0) {
        return("")
      }
      # TODO prepend the prefiexes
      serialization = DynVector$new(10)

      serialization$add(
        prefix_serializer(self$get_prefixes(), lang = "Turtle")
      )

      serialization$add(c(paste(self$context$qname, "{\n")))
      # qnames of subjects and kick out NULL
      subjects = sapply(private$triples$get(), function(t)
      {
        t$subject$qname
      })

      next_object = FALSE
      for (s in unique(subjects)) {
        couplet = private$write_couplet(subject = s, triples = private$triples$get())
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
  ),









  private = list(

    triples = NULL, # DynVector

    # --- Serialization Functions ---
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

  )
)




#' Is the object an Triples List (RDF)?
#'
#' @param x object to check
#'
#' @return logical
#'
#' @export
#' @family rdf
is.ResourceDescriptionFramework = function(x)
{
  if ("ResourceDescriptionFramework" %in% class(x)) TRUE
  else FALSE
}





#' Create a list of RDF statements that all share the same blank subject node
#' @export
#' @family anonymous rdf
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



#' Is the object an Anonymous Triples List (RDF)?
#' @export
#' @family anonymous rdf
is.AnonRDF = function(x)
{
  if ("anonymous_rdf" %in% class(x)) TRUE
  else FALSE
}







#' RDF Class with \code{rdflib} backend
#'
#' @inheritParams ResourceDescriptionFramework
#'
#' @export
RdfLibBackend = R6::R6Class(
  inherit = ResourceDescriptionFramework,
  classname = "RdfLibBackend",

  public = list(

    initialize = function(context)
    {
      super$initialize()
      super$set_context(context)
    },

    nquad = function(subject, predicate, object, context)
    {
      paste(represent(subject), represent(predicate), represent(object), represent(self$context), ".")
    }

  )
)
