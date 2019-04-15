if(!isGeneric("get_selectivities")) setGeneric("get_selectivities", function(object, ...)standardGeneric("get_selectivities"))


setGeneric("Selectivity", function(object, ...)standardGeneric("Selectivity"))

setClass("Selectivity",representation(ogive="Ogive",selectivities="vector"))
setMethod("initialize", signature(.Object="Selectivity"),
          function(.Object,ogive,min_age,max_age){
              if(ogive@low != min_age) stop("ogive@low != min_age")
              if(ogive@high != max_age) stop("ogive@high != max_age")
              .Object@ogive=ogive
              .Object@selectivities=get_value(ogive,min_age:max_age)
              return (.Object)
          }
)
setMethod("Selectivity", signature(object="missing"),
	function(object,ogive,min_age,max_age) {
		return(new("Selectivity",ogive=ogive,min_age=min_age,max_age=max_age))
	}
)
setMethod("get_selectivities", signature(object="Selectivity"),
	function(object){
          return (object@selectivities)
      }
)
