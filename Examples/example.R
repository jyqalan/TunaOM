setwd("c:\\workspace\\TunaOM\\Examples")

get_lf = function(state,size=10000) {
  x= numbers_at_size_in_agebased(state,class_mins=1:60,plus_group=T)
  x=  as.vector(x) / sum(as.vector(x)  )
  x = sample(1:60,size=size,replace=T,prob=x)
  x = as.vector(table(factor(x,levels=1:60)))
  x=round(x/sum(x),4)

}

############
### Constant
############

min_age <- 1
max_age <- 120
B0 <- 375000 
U_max <- 0.9
F_max <- 2
h=0.75
M=0.05
class_mins=1:60

size_at_age=Size_at_age(growth_curve=von_Bert(k=0.065,t0=-0.5,Linf=37.63,cv=0.2),min_age=min_age,max_age=max_age,plus_group=T,class_mins=class_mins)
maturity=Maturity(ogive=Ogive_logistic(low=1,high=120,a50=28.6,a_to95=7.11),min_age=min_age,max_age=max_age)                  
#fishing_selectivity=Selectivity(ogive=Ogive_logistic(low=1,high=120,a50=28.6,a_to95=7.11),min_age=min_age,max_age=max_age)
fishing_selectivity=Selectivity(ogive=Ogive_constant(low=1,high=120,constant=1),min_age=min_age,max_age=max_age)
mean_weight=Age_based_mean_weight(a=9.21e-8,b=2.71,size_at_age=size_at_age,min_age=min_age,max_age=max_age)
natural_mortality = Natural_mortality(M=M,ogive=NULL,min_age=min_age,max_age=max_age)
stock_recruit=Beverton_Holt(h=h,B0=B0)   


years <- 120

############
### Annual cycle function
############

do_year <- function(state,type="",YCS=1,F=NULL,catch=NULL){
     state <- aging_process(state)
     state <- recruitment_process(state,type=type,YCS=YCS)
     premortality_partition <- get_row(state)
     state <- M_process(state,fraction=0.5)
     if(!is.null(F) & !is.null(catch)) stop("use either catch or F in a year, but not both")
     if(!is.null(F)) state <- F_process(state,F=F)
     if(!is.null(catch)) state <- catch_process(state,catch=catch)
     state <- M_process(state,fraction=0.5)
     state <- spawning_process(state,premortality_partition)
     return(state)   
}
     
    state=State(min_age=min_age,max_age=max_age,plus_group=TRUE,maturity=maturity,natural_mortality=natural_mortality,
                fishing_selectivity=fishing_selectivity,size_at_age=size_at_age,mean_weight=mean_weight,stock_recruit= stock_recruit,
                U_max=U_max, F_max=F_max,annual_cycle=do_year)
    s <- set_initial_state(state,B0=B0)
    x = get_lf(s)
    
    state = s
    for(i in 1:120) state <- annual_cycle(state,type="",YCS=1,F=0.10,catch=NULL)
    y = get_lf(state)
    
    state = s
    for(i in 1:120) state <- annual_cycle(state,type="",YCS=1,F=0.10,catch=NULL)
    z = get_lf(state)   
    plot(1:60,x)
    lines(1:60,y,lty=1)
    lines(1:60,z,lty=2)
    