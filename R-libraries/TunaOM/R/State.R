if(!isGeneric("print")) setGeneric("print", useAsDefault = print)
if(!isGeneric("show")) setGeneric("show", useAsDefault = show)
if(!isGeneric("initialize")) setGeneric("initialize", useAsDefault = initialize)
if(!isGeneric("abundance")) setGeneric("abundance", function(object, ...)standardGeneric("abundance"))
if(!isGeneric("numbers_at")) setGeneric("numbers_at", function(object, ...)standardGeneric("numbers_at"))
if(!isGeneric("mature_abundance")) setGeneric("mature_abundance", function(object, ...)standardGeneric("mature_abundance"))
if(!isGeneric("mature_numbers_at")) setGeneric("mature_numbers_at", function(object, ...)standardGeneric("mature_numbers_at"))
if(!isGeneric("numbers_at_size_in_agebased")) setGeneric("numbers_at_size_in_agebased", function(object, ...)standardGeneric("numbers_at_size_in_agebased"))
if(!isGeneric("mature_numbers_at_size_in_agebased")) setGeneric("mature_numbers_at_size_in_agebased", function(object, ...)standardGeneric("mature_numbers_at_size_in_agebased"))

if(!isGeneric("recruitment_process")) setGeneric("recruitment_process", function(object, ...)standardGeneric("recruitment_process"))
if(!isGeneric("M_process")) setGeneric("M_process", function(object, ...)standardGeneric("M_process"))
if(!isGeneric("aging_process")) setGeneric("aging_process", function(object, ...)standardGeneric("aging_process"))
if(!isGeneric("spawning_process")) setGeneric("spawning_process", function(object, ...)standardGeneric("spawning_process"))
if(!isGeneric("catch_process")) setGeneric("catch_process", function(object, ...)standardGeneric("catch_process"))
if(!isGeneric("F_process")) setGeneric("F_process", function(object, ...)standardGeneric("F_process"))
if(!isGeneric("anuual_cycle")) setGeneric("annual_cycle", function(object, ...)standardGeneric("annual_cycle"))



if(!isGeneric("set_initial_state")) setGeneric("set_initial_state", function(object, ...)standardGeneric("set_initial_state"))
if(!isGeneric("set_current_state")) setGeneric("set_current_state", function(object, ...)standardGeneric("set_current_state"))
if(!isGeneric("set_R0")) setGeneric("set_R0", function(object, ...)standardGeneric("set_R0"))
if(!isGeneric("set_B0")) setGeneric("set_B0", function(object, ...)standardGeneric("set_B0"))


setGeneric("State", function(object, ...)standardGeneric("State"))
setClassUnion("function_or_NULL",c("function","NULL"))


# State Class
setClass("State",representation("Partition",
                                min_age="numeric",
                                max_age="numeric",
                                plus_group="logical",
                                maturity="Maturity",
                                natural_mortality="Natural_mortality",
                                fishing_selectivity="Selectivity",
                                size_at_age="Size_at_age",
                                mean_weight="Mean_weight",
                                stock_recruit="Stock_recruit_or_NULL",
                                R0="numeric",
                                B0="numeric",
                                SSB="numeric",
                                recruit="numeric",
                                U_max="numeric",
                                U_obs="numeric",
                                U="numeric",
                                F_max="numeric",
                                F_obs="numeric",
                                F="numeric",
                                catch="numeric",
                                actual_catch="numeric",
                                fishing_pressure_limit_exceeded="logical",
                                annual_cycle="function_or_NULL"))

setMethod("initialize", signature(.Object="State"),
          function(.Object,min_age=1,max_age=1,plus_group=TRUE,maturity,natural_mortality,fishing_selectivity,size_at_age,mean_weight,stock_recruit,U_max,F_max,annual_cycle=NULL){
              if(min_age < 0) stop("min_age < 0")
              if(max_age < 0) stop("max_age < 0")
              if(min_age > max_age) stop("min_age > max_age")
              if(U_max < 0 || U_max >= 1) stop("U_max must be between 0 and 1")
              
              .Object@min_age=min_age
              .Object@max_age=max_age
              .Object@plus_group = plus_group
              .Object@n_cols = max_age - min_age + 1
              .Object@col_min = 1
              .Object@col_max =  max_age - min_age + 1
              .Object@.Data = vector("numeric",length=.Object@n_cols)
              .Object@maturity = maturity
              .Object@natural_mortality = natural_mortality
              .Object@fishing_selectivity = fishing_selectivity
              .Object@size_at_age = size_at_age
              .Object@mean_weight = mean_weight
              .Object@stock_recruit = stock_recruit
              .Object@R0 = 0
              .Object@B0 = 0
              .Object@SSB = 0
              .Object@recruit = 0
              .Object@U_max = U_max
              .Object@F_max = F_max
              .Object@U=0
              .Object@U_obs=0
              .Object@F=0
              .Object@F_obs=0
              .Object@catch=0
              .Object@actual_catch=0
              .Object@fishing_pressure_limit_exceeded=FALSE
              if(is.null(annual_cycle)) .Object@annual_cycle = NULL
              else .Object@annual_cycle = match.fun(annual_cycle)
              return (.Object)
          }
)

setMethod("State", signature(object="missing"),
	function(object,min_age,max_age,plus_group,maturity,natural_mortality,fishing_selectivity,size_at_age,mean_weight,stock_recruit,U_max,F_max,annual_cycle=NULL) {
		return(new("State",min_age=min_age,
                                   max_age=max_age,
                                   plus_group=plus_group,
                                   maturity=maturity,
                                   natural_mortality=natural_mortality,
                                   fishing_selectivity=fishing_selectivity,
                                   size_at_age=size_at_age,
                                   mean_weight=mean_weight,
                                   stock_recruit=stock_recruit,
                                   U_max=U_max,
                                   F_max=F_max,
                                   annual_cycle=annual_cycle))
	}
)

setMethod("show", signature(object="State"),
	function(object){
                cat(object@.Data,"\n")            
		cat("min_age: ", object@min_age, "\n")
		cat("max_age: ", object@max_age, "\n")
		cat("B0: ", object@B0, "\n")
		cat("R0: ", object@R0, "\n")
		cat("SSB: ", object@SSB, "\n")               
	}
)
setMethod("print", signature(x="State"),
	function(x){
            show(x)
	}
)

setMethod("abundance", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
            result <- sum(result * get_mean_weight(object@mean_weight))
            return(result)
	}
)

setMethod("mature_abundance", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)*get_mature_proportions(object@maturity)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
            result <- sum(result * get_mean_weight(object@mean_weight))
            return(result)
	}
)

setMethod("numbers_at", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
           return(result)
	}
)

setMethod("mature_numbers_at", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)*get_mature_proportions(object@maturity)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
            return(result)
	}
)


setMethod("numbers_at_size_in_agebased", signature(object="State"),
          function(object,plus_group=T,class_mins=NULL,selectivity=NULL){
            result <- get_row(object)
            if(!is.null(selectivity)){
              result <- result * get_selectivities(selectivity)
            }
            result = get_size_dist(size_at_age,plus_group,class_mins) %*% result
            return(result)
          }
)


setMethod("mature_numbers_at_size_in_agebased", signature(object="State"),
          function(object,plus_group=T,class_mins=NULL,selectivity=NULL){
            result <- get_row(object)*get_mature_proportions(object@maturity)
            if(!is.null(selectivity)){
              result <- result * get_selectivities(selectivity)
            }
            result = get_size_dist(size_at_age,plus_group,class_mins) %*% result
            return(result)
          }
)


setMethod("annual_cycle",signature(object="State"),
        function(object,...){
            if(is.null(annual_cycle)) stop("annual_cycle not defined!")
            else return(object@annual_cycle(object,...))
        }         
)

setMethod("set_initial_state",signature(object="State"),
        function(object,B0,aging_ahead_recruiment=TRUE,simulation=FALSE){
            if(!simulation) {
                if(!is.null(object@natural_mortality@ogive)){
                    stop("Ogive of M is not null, You need to provide an annual_cycle function to the state object!")
                }    
                M <- get_M(object@natural_mortality)
                R0 <- 1
                object@.Data <- R0*exp(-(1:object@n_cols)*M)
                if(object@plus_group) object@.Data[object@n_cols] <-  object@.Data[object@n_cols]/(1-exp(-M))
                premortality_partition <- R0*exp(-(0:(object@n_cols-1))*M)
                if(object@plus_group) premortality_partition[object@n_cols] <- premortality_partition[object@n_cols]/(1-exp(-M))
                midmortality_partition <- average_partition(object,premortality_partition,0.5)
                SSB <- sum(midmortality_partition*get_mean_weight(object@mean_weight)*get_mature_proportions(object@maturity))
                object@.Data <-  object@.Data*B0/SSB
                object@B0 <- B0
                object@R0 <- B0/SSB            
                object@SSB <- sum(midmortality_partition*get_mean_weight(object@mean_weight)*get_mature_proportions(object@maturity))*B0/SSB
                return (object)          
            } else {
                if(is.null(object@annual_cycle)) stop("annual_cycle not defined!")
                object@R0 <- 1
                object@.Data <- vector("numeric",length=object@n_cols)
                for(i in object@col_min:ifelse(aging_ahead_recruiment,object@col_max,object@col_max-1)) {
                    object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=NULL)
                }
                if (object@plus_group){
                    prechange_partition = get_row(object)
                    object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=NULL)                   
                    if (prechange_partition[object@col_max] > 0){
                        temp = object@.Data[object@col_max] / prechange_partition[object@col_max] - 1;
                        object@.Data = prechange_partition;
                        object@.Data[object@col_max] = object@.Data[object@col_max] * pow(1-temp,-1);
                    } else {
                        object@.Data = prechange_partition;
                    }
                }
                object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=NULL)    
                SSB <- object@SSB
                object@.Data <-  object@.Data*B0/SSB
                object@B0 <- B0
                object@R0 <- B0/SSB            
                object@SSB <- B0
                return (object)          
            }
        }  
)



setMethod("set_current_state",signature(object="State"),
        function(object,status,start=NULL,end=NULL,type="",YCS=1,use_F=TRUE,F_guess=NULL,catch_guess=NULL,DELTA=10e-3){
            # Searching for a constant F or catch that would fish the population down to status of B0 from start to end
            # or until reaching equilibrium if start and end is not defined or  end = Inf 
            if(is.null(object@annual_cycle)) stop("annual_cycle not defined!")
            if(is.null(start) & is.null(end)){
                option="equilibrium"
            } else {
                if(is.null(start) | is.null(end)) stop("start or end is null")
                if(start > end) stop("start > end!")
                if(!is.finite(end)) option <- "equilibrium"
                else option <- ""
            }
            if(option!="equilibrium" & type!="constant"){
                if(length(YCS) != end-start+1) stop("the length of YCS should be equal to end-start+1")
            } else if(option!="equilibrium" & type=="constant") {
                YCS <- rep(1,end-start+1)
            }
            if(!is.null(F_guess) & !is.null(catch_guess)) stop("use either catch _guess or F_guess, but not both!")            
            if(status > 1.00 || status < 0) stop("Current status should not be over 100% B0 or less than 0% B0")

            if(status==1){
                SSB <- object@SSB
                while(TRUE){
                    object <- annual_cycle(object,type="constant",YCS=1,F=0,catch=NULL)
                    if(abs(object@SSB-SSB) < DELTA) break
                    else SSB <- object@SSB
                }  
            } else if(use_F){
                if(is.null(F_guess)) F_guess <- object@F_max
                if(F_guess <= 0) stop("F_guess <=0!")
                pre_object <- object
                F_low <- 0
                F_high <- F_guess
                cat("testing F=")
                while(TRUE) {                    
                    cat(F_high," ")
                    if(option=="equilibrium"){
                        SSB <- object@SSB
                        while(TRUE) {
                            object <- annual_cycle(object,type="constant",YCS=1,F=F_high,catch=NULL)
                            if(abs(object@SSB-SSB) <= DELTA) break
                            else SSB <- object@SSB
                        }
                    } else {
                        for (i in 1:(end-start+1)) {
                            object <- annual_cycle(object,type=type,YCS=YCS[i],F=F_high,catch=NULL)
                         }                    
                    }
                    if((object@SSB > object@B0*status) > DELTA){
                            object <- pre_object
                            F_low <- F_high
                            F_high <- 2*F_high
                            if(F_high*max(get_selectivities(object@fishing_selectivity)) > object@F_max) stop(paste("tesing F=",F_high," exceeded F_max",sep=""))
                    } else break
                }
                if(abs(object@SSB-object@B0*status) > DELTA){
                    object <- pre_object                      
                    while(TRUE){
                        cat((F_high+F_low)/2," ")
                        if(option=="equilibrium"){
                            SSB <- object@SSB
                            while(TRUE){
                                object <- annual_cycle(object,type="constant",YCS=1,F=(F_high+F_low)/2,catch=NULL)
                                if(abs(object@SSB-SSB) <= DELTA) break
                                else SSB <- object@SSB
                            }
                         } else {
                             for (i in 1:(end-start+1)) {
                                 object <- annual_cycle(object,type=type,YCS=YCS[i],F=(F_high+F_low)/2,catch=NULL)
                             }                    
                         }        
                         if(abs(object@SSB-object@B0*status) < DELTA) break
                         else {
                             if(object@SSB > object@B0*status){
                                 F_high <- F_high
                                 F_low <- (F_high+F_low)/2
                             } else {
                                 F_low <- F_low
                                 F_high <- (F_high+F_low)/2
                             }
                             if(abs(object@F-(F_high+F_low)/2) <DELTA) break # Add this to reduce time
                             object <- pre_object
                         }
                    }
                }
               cat("\n")
            } else {
                if(is.null(catch_guess)) catch_guess <-  sum(get_row(object)*get_mean_weight(object@mean_weight)*(1-exp(-object@F_max*get_selectivities(object@fishing_selectivity))))
                if(catch_guess <= 0) stop("catch_guess <=0!")
                pre_object <- object
                catch_low <- 0
                catch_high <- catch_guess
                cat("testing catch=")
                while(TRUE) {                    
                    cat(catch_high," ")
                    if(option=="equilibrium"){
                        SSB <- object@SSB
                        while(TRUE) {
                            object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=catch_high)
                            if(abs(object@SSB-SSB) <= DELTA) break
                            else SSB <- object@SSB
                        }
                    } else {
                        for (i in 1:(start-end+1)) {
                            object <- annual_cycle(object,type=type,YCS=YCS[i],F=NULL,catch=catch_high)
                         }                    
                    }
                    if((object@SSB > object@B0*status) > DELTA){
                            object <- pre_object
                            catch_low <- catch_high
                            catch_high <- 2*catch_high
                            if(max(catch_high/sum(get_row(object)*get_selectivities(object@fishing_selectivity)*get_mean_weight(object@mean_weight))*
                                   get_selectivities(object@fishing_selectivity)) > object@U_max)
                               stop(paste("tesing catch=",catch_high," exceeded U_max",sep=""))
                    } else break
                }
                if(abs(object@SSB-object@B0*status) > DELTA){
                    object <- pre_object                      
                    while(TRUE){
                        cat((catch_high+catch_low)/2," ")
                        if(option=="equilibrium"){
                            SSB <- object@SSB
                            while(TRUE){
                                object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=(catch_high+catch_low)/2)
                                if(abs(object@SSB-SSB) <= DELTA) break
                                else SSB <- object@SSB
                            }
                         } else {
                             for (i in 1:(start-end+1)) {
                                 object <- annual_cycle(object,type=type,YCS=YCS[i],F=NULL,catch=(catch_high+catch_low)/2)
                             }                    
                         }        
                         if(abs(object@SSB-object@B0*status) < DELTA) break
                         else {
                             if(object@SSB > object@B0*status){
                                 catch_high <- catch_high
                                 catch_low <- (catch_high+catch_low)/2
                             } else {
                                 catch_low <- catch_low
                                 catch_high <- (catch_high+catch_low)/2
                             }
                             object <- pre_object
                         }
                    }
                }                
               cat("\n") 
            }
            return(object)
        }
)

setMethod("set_R0",signature(object="State"),
        function(object,R0){
                object
                object@R0 <- R0           
                return (object)          
            
        }  
)

setMethod("set_B0",signature(object="State"),
        function(object,B0){
                object
                object@B0 <- B0           
                return (object)          
            
        }  
)



setMethod("recruitment_process", signature(object="State"),
	function(object,type="",YCS=1){
            if(type=="constant"){
                 object@recruit <- object@R0
            } else {
                 if(is.null(YCS)) stop("YCS is null")
                 #if(YCS <=0.1 || YCS > 10) stop(paste("YCS of ",YCS,", too extreme!",sep=""))
                 object@recruit <- object@R0 * YCS
                 if(!is.null(object@stock_recruit))
                     object@recruit <- object@recruit * SR(object@stock_recruit,object@SSB)
            }
            object@.Data[object@col_min] <- object@recruit
            return(object)
	}
)

setMethod("aging_process", signature(object="State"),
	function(object){
            if(object@plus_group){
                if (object@n_cols > 1){
                    object@.Data[object@col_max] = object@.Data[object@col_max]+object@.Data[object@col_max-1]
                    j=object@col_max-1
                    while(j > object@col_min) {
                        object@.Data[j] = object@.Data[j-1];
                        j=j-1
                    }
                    object@.Data[object@col_min] = 0;
                }
            } else  {
                i=object@col_max
                while(j > object@col_min) {
                    object@.Data[j] = object@.Data[j-1];
                    j=j-1
                }
                object@.Data[object@col_min] = 0;                
            }
          return(object)
	}            
)  

setMethod("M_process", signature(object="State"),
	function(object,fraction=1){
            if(fraction < 0 || fraction > 1) stop("fraction must be between 0 and 1") 
            if(fraction==1)
                object@.Data = object@.Data*get_exp_minus_M_by_element(object@natural_mortality)
            else
                object@.Data = object@.Data*exp(-get_M_by_element(object@natural_mortality)*fraction)
          return(object)
	}            
)

setMethod("spawning_process", signature(object="State"),
	function(object,premortality_partition,proportion=0.5,method="weighted_sum"){
            partmortality_partition <- average_partition(object,premortality_partition,proportion=proportion,method=method)
            object@SSB = sum(partmortality_partition*get_mean_weight(object@mean_weight)*get_mature_proportions(object@maturity))
          return(object)
	}            
)  

setMethod("catch_process", signature(object="State"),
	function(object,catch){
            if(catch < 0) stop("catch < 0")
            U <- catch/sum(get_row(object)*get_selectivities(object@fishing_selectivity)*get_mean_weight(object@mean_weight))
            U_obs <- max(U*get_selectivities(object@fishing_selectivity))
            fishing_pressure_limit_exceeded <- FALSE
            if (U_obs > object@U_max) {
                U <- U*object@U_max/U_obs
                U_obs <- max(U*get_selectivities(object@fishing_selectivity))
                fishing_pressure_limit_exceeded <- TRUE
            } 
            object@U <- U
            object@U_obs <- U_obs
            object@catch <- catch
            object@actual_catch <- sum(U*object@.Data*get_selectivities(object@fishing_selectivity)*get_mean_weight(object@mean_weight))
            object@fishing_pressure_limit_exceeded <- fishing_pressure_limit_exceeded
            # F cannot be determined analytically
            object@.Data <- object@.Data*(1-object@U*get_selectivities(object@fishing_selectivity))
          return(object)
	}            
)  

setMethod("F_process", signature(object="State"),
	function(object,F){
            if(F < 0) stop("F < 0")
            catch <- sum(get_row(object)*get_mean_weight(object@mean_weight)*(1-exp(-F*get_selectivities(object@fishing_selectivity))))
            F_obs <- F*max(get_selectivities(object@fishing_selectivity))
            fishing_pressure_limit_exceeded <- FALSE
            if (F_obs > object@F_max) {
                F <- object@F_max/max(get_selectivities(object@fishing_selectivity))
                F_obs <- F*max(get_selectivities(object@fishing_selectivity))
                fishing_pressure_limit_exceeded <- TRUE
            }
            object@catch <- catch
            object@actual_catch <-  sum(get_row(object)*get_mean_weight(object@mean_weight)*(1-exp(-F*get_selectivities(object@fishing_selectivity))))
            object@F <- F
            object@F_obs <- F_obs
            object@fishing_pressure_limit_exceeded <- fishing_pressure_limit_exceeded
            # U can be calculated, but not for now

            object@.Data <- object@.Data*exp(-object@F*get_selectivities(object@fishing_selectivity))
            object@SSB <- 
          return(object)
	}            
)  








