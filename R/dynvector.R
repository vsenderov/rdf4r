DynVector = R6Class(
  classname = "dynamic_vector",
  public = list(
    dynamic_vector = NULL,
    last_item = 0,

    initialize = function(size)
    {
      self$dynamic_vector = vector(mode = "list", length = size)
    },

    add = function(x)
    {
      current_item = self$last_item + 1
      if (current_item > length(self$dynamic_vector)) {
        # need to reallocate
        # new dynamic vector with double the size
        # add the contents of the previous vector ot it
        # change the self to point to the new vector
        browser()
      }
      else {
        self$dynamic_vector[[current_item]] = x
        self$last_item = self$last_item + 1
      }
    },

    add_list = function(l)
    {
      end_list = self$last_item + length(l)
      if (end_list > length(self$dynamic_vector)) {
        # need to reallocate
        browser()
      }
      else {
        self$dynamic_vector[(self$last_item + 1):end_list] = l
      }
    }

  )
)
