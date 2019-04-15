if(!isGeneric("mean_size")) setGeneric("mean_size", function(object, ...)standardGeneric("mean_size"))
if(!isGeneric("get_cv")) setGeneric("get_cv", function(object, ...)standardGeneric("get_cv"))

setClass("Growth_curve")
setMethod("mean_size", signature(object="Growth_curve"),
	function(object,age) {
 	}
)   
   
setMethod("get_cv", signature(object="Growth_curve"),
	function(object,age) {
 	}
)   
   
