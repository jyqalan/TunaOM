if(!isGeneric("get_mature_proportions")) setGeneric("get_mature_proportions", function(object, ...)standardGeneric("get_mature_proportions"))

setGeneric("Maturity", function(object, ...)standardGeneric("Maturity"))

setClass("Maturity",representation(ogive="Ogive",mature_proportions="vector"))
setMethod("initialize", signature(.Object="Maturity"),
          function(.Object,ogive,min_age,max_age){
              .Object@ogive=ogive
              .Object@mature_proportions=get_value(ogive,min_age:max_age)
              return (.Object)
          }
)
setMethod("Maturity", signature(object="missing"),
	function(object,ogive,min_age,max_age) {
		return(new("Maturity",ogive=ogive,min_age=min_age,max_age=max_age))
	}
)
setMethod("get_mature_proportions", signature(object="Maturity"),
	function(object){
          return (object@mature_proportions)
      }
)
