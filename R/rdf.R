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
#' @param add_triples_extended(data, subject_column_label = "", subject_column_name = "", subject_rdf_prefix = "", predicate, object_column_label = "", object_column_name = "", object_rdf_type = NULL, object_rdf_prefix = "", progress_bar = TRUE) file_name needs to be characters, and
#' progress_bar needs to be boolean.
#'
#' @param ntriples() returns number of triples inserted by add_riples_extended function.
#'
#' @param set_list(triple_vector) triple_vector needs to be a \code{DynVector}
#'   object. The information is merged.
#'
#' @param serialize() Returns the Turtle serialization.
#'
#' @param serialize_to_file(file_name,progress_bar=TRUE) file_name needs to be characters, and
#' progress_bar needs to be boolean.
#'
#' @param prefix_list \code{DynVector} Prefixes of all the stored
#'   identifiers as an uncollapsed list. For most cases you would want
#'   to use \code{get_prefixes}.
#'
#' @param context \code{identifier}. The named graph of where the statements
#'   are stored.
#'
#' @export
#' @family rdf
ResourceDescriptionFramework = R6::R6Class(
  classname = "ResourceDescriptionFramework",

  public = list(

    triples_list = NULL, # list of DynVector

    prefix_list = NULL, # DynVector
    context = NULL, # identifer

    initialize = function(size = 1000000)
    {
      private$triples = DynVector$new(size = size)
      self$prefix_list = DynVector$new(size = size)
      self$triples_list <- list()
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
      if (!is.identifier(subject) || !is.identifier(predicate) || !(is.literal(object) || is.identifier(object) || is.ResourceDescriptionFramework(object))) {
        return (FALSE)
      }
      else {
        self$prefix_list$add(subject$prefix)
        self$prefix_list$add(predicate$prefix)
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

    set_list = function(triple_vector)
    {
      private$triples = triple_vector
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
    },
    serialize_to_file = function(file_name, progress_bar=TRUE){
      # write prefixes
      cat(paste0(prefix_serializer(self$get_prefixes(), lang = "Turtle")), file = file_name)

      # write context
      cat(paste0(self$context$qname," {"),file = file_name, append = TRUE, sep="\n")
      m_triples <- 0

      for(triple_list in self$triples_list){
        m_triples <- m_triples + length(triple_list$get())
      }

      if(isTRUE(progress_bar)){
        pb <- txtProgressBar(1, m_triples, style = 3)
      }

      n <- 0
      for(triple_list in self$triples_list){
        n_triples <- length(triple_list)
        for(triple in triple_list$get()) {
          n <- n + 1
          if(isTRUE(progress_bar)){
            setTxtProgressBar(pb, n)
          }
          if (is.literal(triple$object)) {
            the_object <- triple$object$squote
          }
          else if (is.identifier(triple$object)) {
            the_object <- triple$object$qname
          }
          # writ triple to file
          this_triple <- paste0(" ", triple$subject$qname, " ", triple$predicate$qname, " ", the_object," .\n")
          cat(this_triple, file = file_name, append = TRUE)
        }
      }
      # write }
      cat(" }", file = file_name, append = TRUE)
    },
    add_triples_extended = function(data, subject_column_label = "", subject_column_name = "", subject_rdf_prefix = "", predicate, object_column_label = "", object_column_name = "", object_rdf_type = NULL, object_rdf_prefix = "", progress_bar = TRUE){
      # define resource identifiers for subjects and objects:
      n_rows = nrow(data)
      if(isTRUE(progress_bar)) {
        pb <- txtProgressBar(1, n_rows, style = 3)
      }
      phathe_triples <- DynVector$new(size = n_rows)

      if(subject_rdf_prefix != "") self$prefix_list$add(subject_rdf_prefix)
      if(object_rdf_prefix != "") self$prefix_list$add(object_rdf_prefix)

      for (i in 1:n_rows) {

        the_subject <- if(subject_column_name == "")
          literal(text_value = subject_column_label)
        else
          identifier(paste0(subject_column_label,data[i,subject_column_name]), prefix = subject_rdf_prefix)

        the_object <- if(object_column_name == ""){
          literal(text_value = object_column_label)
        } else if(object_rdf_prefix == "" && !is_null(object_rdf_type) ) {
          literal(text_value = paste0(object_column_label,data[i,object_column_name]), xsd_type = object_rdf_type)
        } else {
            the_object <- identifier(paste0(object_column_label,data[i,object_column_name]), prefix = object_rdf_prefix)
        }

        # build triples:
        phathe_triples$add(list(subject = the_subject, predicate = predicate, object = the_object))
        if(isTRUE(progress_bar)) {
          setTxtProgressBar(pb, i)
        }
      }
      n <- length(self$triples_list)
      self$triples_list[[n+1]] <- phathe_triples
    },
    ntriples = function(){
      n <- 0
      for(triple_list in self$triples_list){
        n <- n + length(triple_list$get())
      }
      return(n)
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
