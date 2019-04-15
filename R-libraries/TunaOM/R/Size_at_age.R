if(!isGeneric("get_mean_size")) setGeneric("get_mean_size", function(object, ...)standardGeneric("get_mean_size"))   
if(!isGeneric("mean_size_at_age")) setGeneric("mean_size_at_age", function(object, ...)standardGeneric("mean_size_at_age"))

if(!isGeneric("get_cvs")) setGeneric("get_cvs", function(object, ...)standardGeneric("get_cvs"))   
if(!isGeneric("cv_at_age")) setGeneric("cv_at_age", function(object, ...)standardGeneric("cv_at_age"))

if(!isGeneric("get_size_dist")) setGeneric("get_size_dist", function(object, ...)standardGeneric("get_size_dist"))   
if(!isGeneric("size_dist_at_age")) setGeneric("size_dist_at_age", function(object, ...)standardGeneric("size_dist_at_age"))   


setGeneric("Size_at_age", function(object, ...)standardGeneric("Size_at_age"))

setClass("Size_at_age",representation(growth_curve="Growth_curve",mean_size="vector",cvs="vector",min_age="numeric",max_age="numeric",size_dist="matrix"))
setMethod("initialize", signature(.Object="Size_at_age"),
          function(.Object, growth_curve,min_age,max_age,plus_group,class_mins){
              .Object@growth_curve = growth_curve
              .Object@mean_size = mean_size_at_age(.Object,min_age:max_age)
              .Object@cvs = cv_at_age(.Object,min_age:max_age)
              .Object@min_age =min_age
              .Object@max_age =max_age
              .Object@size_dist = size_dist_at_age(.Object,plus_group,class_mins)
              return (.Object)
          }
)   
setMethod("Size_at_age", signature(object="missing"),
	function(object,growth_curve,min_age,max_age,plus_group,class_mins) {
		return(new("Size_at_age",growth_curve=growth_curve,min_age=min_age,max_age=max_age,plus_group=plus_group,class_mins=class_mins))
	}
)
setMethod("mean_size_at_age", signature(object="Size_at_age"),
	function(object,age) {
            result=mean_size(object@growth_curve,age);
            return (result);
 	}
)
setMethod("get_mean_size", signature(object="Size_at_age"),
	function(object) {
            return (object@mean_size);
 	}
)      
   
setMethod("cv_at_age", signature(object="Size_at_age"),
	function(object,age) {
            result=get_cv(object@growth_curve,age);
            return (result);
 	}
)
setMethod("get_cvs", signature(object="Size_at_age"),
	function(object) {
            return (object@cvs);
 	}
)      


setMethod("get_size_dist", signature(object="Size_at_age"),
          function(object,plus_group=T,class_mins=NULL) {
            if(is.null(class_mins))
              return (object@size_dist)
            else
              return (size_dist_at_age(object,plus_group,class_mins))
          }
)


setMethod("size_dist_at_age", signature(object="Size_at_age"),
  function(object,plus_group,class_mins) {
    n_classes = ifelse(plus_group,length(class_mins), length(class_mins) - 1);
    n_ages = object@max_age - object@min_age + 1
    result = matrix(0,n_classes,n_ages);
    for (j in 1:n_ages) {
       result[,j] = distribution(class_mins, plus_group, "normal", object@mean_size[j], object@mean_size[j]*object@cvs[j]);
    }
     return (result);
  }
)
    
