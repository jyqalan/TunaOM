if(!isGeneric("mean_size")) setGeneric("mean_size", function(object, ...)standardGeneric("mean_size"))
if(!isGeneric("get_cv")) setGeneric("get_cv", function(object, ...)standardGeneric("get_cv"))

setGeneric("von_Bert", function(object, ...)standardGeneric("von_Bert"))


setClass("von_Bert",representation("Growth_curve",k="numeric",t0="numeric",Linf="numeric",cv="numeric"))
setMethod("initialize", signature(.Object="von_Bert"),
          function(.Object, k, t0, Linf,cv){
              if (k<0) stop(" The k parameter should never be negative.");
              if (Linf<0) stop(" The Linf parameter should never be negative.");
              if (cv<0) stop(" The cv parameter should never be negative.");
              .Object@k=k
              .Object@t0=t0
              .Object@Linf=Linf
              .Object@cv=cv
              return (.Object)
          }
)   
setMethod("von_Bert", signature(object="missing"),
	function(object,k, t0, Linf,cv) {
		return(new("von_Bert",k=k,t0=t0,Linf=Linf,cv=cv))
	}
)
setMethod("mean_size", signature(object="von_Bert"),
	function(object,age) {
            if (any(-object@k*(age-object@t0) > 10))
                stop("Fatal error: exp(-k*(age-t0)) is enormous. Your k or t0 parameters are probably wrong.")
            result = object@Linf*(1 - exp(-object@k*(age - object@t0)));
            result[result<0] = 0.0
            return (result);
 	}
)   

setMethod("get_cv", signature(object="Growth_curve"),
	function(object,age) {
            result = rep(object@cv,length(age))
            return(result)
 	}
)   
   
