if(!isGeneric("get_value")) setGeneric("get_value", function(object, ...)standardGeneric("get_value"))

setClass("Ogive",representation(low="numeric",high="numeric"))

setClassUnion("Ogive_or_NULL",c("Ogive","NULL"))

setMethod("get_value", signature(object="Ogive"),
	function(object,x){
      }
)   
