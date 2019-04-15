if(!isGeneric("get_M")) setGeneric("get_M", function(object, ...)standardGeneric("get_M"))
if(!isGeneric("get_M_by_element")) setGeneric("get_M_by_element", function(object, ...)standardGeneric("get_M_by_element"))
if(!isGeneric("get_exp_minus_M_by_element")) setGeneric("get_exp_minus_M_by_element", function(object, ...)standardGeneric("get_exp_minus_M_by_element"))

setGeneric("Natural_mortality", function(object, ...)standardGeneric("Natural_mortality"))

setClass("Natural_mortality",representation(M="numeric", ogive="Ogive_or_NULL",M_by_element="vector",exp_minus_M_by_element="vector"))
setMethod("initialize", signature(.Object="Natural_mortality"),
          function(.Object,ogive,M,min_age,max_age){
              .Object@ogive=ogive
              .Object@M=M
              if(is.null(ogive))
                  .Object@M_by_element=rep(M,max_age-min_age+1)
              else
                  .Object@M_by_element=M*get_value(ogive,min_age:max_age)
              .Object@exp_minus_M_by_element=exp(-.Object@M_by_element)
              return (.Object)
          }
)
setMethod("Natural_mortality", signature(object="missing"),
	function(object,M,ogive,min_age,max_age) {
		return(new("Natural_mortality",ogive=ogive,M=M,min_age=min_age,max_age=max_age))
	}
)
setMethod("get_M", signature(object="Natural_mortality"),
	function(object){
          return (object@M)
      }
)
setMethod("get_M_by_element", signature(object="Natural_mortality"),
	function(object){
          return (object@M_by_element)
      }
)
setMethod("get_exp_minus_M_by_element", signature(object="Natural_mortality"),
	function(object){
          return (object@exp_minus_M_by_element)
      }
)
