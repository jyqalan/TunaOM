if(!isGeneric("mean_weight_at_age")) setGeneric("mean_weight_at_age", function(object, ...)standardGeneric("mean_weight_at_age"))   

setGeneric("Age_based_mean_weight", function(object, ...)standardGeneric("Age_based_mean_weight"))

setClass("Age_based_mean_weight",representation("Mean_weight",a="numeric",b="numeric",size_at_age="Size_at_age"))
setMethod("initialize", signature(.Object="Age_based_mean_weight"),
          function(.Object, a,b,size_at_age,min_age,max_age){
              .Object@a=a
              .Object@b=b
              .Object@size_at_age = size_at_age
              .Object@mean_weight=mean_weight_at_age(.Object,min_age:max_age)
              return (.Object)
          }
)   
setMethod("Age_based_mean_weight", signature(object="missing"),
	function(object,a,b,size_at_age,min_age,max_age) {
		return(new("Age_based_mean_weight",a=a,b=b,size_at_age=size_at_age,min_age=min_age,max_age=max_age))
	}
)
setMethod("mean_weight_at_age", signature(object="Age_based_mean_weight"),
	function(object,age) {
            cv <- cv_at_age(object@size_at_age,age)
            result =  object@a*pow(mean_size_at_age(object@size_at_age,age),object@b)*(1+cv^2)^(object@b*(object@b-1)/2)
            return(result)
 	}
)    

