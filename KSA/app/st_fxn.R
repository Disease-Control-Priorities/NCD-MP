state.transition<-function(b_rates, pop0, code){ 
  
  base_rates<-merge(b_rates, pop0[year<=2030], by=c("year", "location", "sex", "age"), all=TRUE)%>%
    arrange(year, age)%>%
    filter(year>=2019)
  
  if(code!="baseline"){
    
    base_rates<-left_join(base_rates, interv.df%>%filter(cause%in%cse, Code==code))%>%
      mutate(IR = IR*ifelse(is.na(IR_eff),1,IR_eff),
             CF = CF*ifelse(is.na(CF_eff),1,CF_eff))%>%
      select(-IR_eff, -CF_eff, -Code)
  }
  
  else{
    
  }
  
  base_rates[age==0 & year>2019, Nx:=Nx0]
  base_rates[, Nx0:=NULL]
  
  # calculate initial states for the incoming year 2019 and all years for age 0 population
  
  suppressWarnings(base_rates[year==2019 | age==0, sick:=Nx*prev.rate])
  base_rates[year==2019 | age==0, dead:=Nx*dis.mx]
  base_rates[year==2019 | age==0, well:=Nx*(1-(prev.rate+all.mx))]
  base_rates[year==2019 | age==0, newcases:=well*IR]
  
  base_rates[age==0 | year==2019, pop:=Nx]
  base_rates[age==0 | year==2019, all.dead:=Nx*all.mx]
  
  base_rates[CF>0.9, CF:=0.9]
  base_rates[IR>0.9, IR:=0.9]
  
  base_rates<-base_rates%>%arrange(sex, location, cause, age, year)
  #i<-1
  #STATE TRANSITIONS#
  for(i in 1:11){
    
    b2<-base_rates[year<=2019+i & year>=2019+i-1]
    b2[,age2:=age+1]
    
    #new cases
    b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age)]
    #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
    b2[newcases2<0, newcases2:=0] #prevent possible negatives
    
    #sick
    b2[, sick2:=shift(sick)*(1-(CF+bg.mx)) + shift(well)*IR, by=.(sex, location, cause, age)]
    #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
    b2[sick2<0, sick2:=0] #prevent possible negatives
    
    #dead
    b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age)]
    #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
    b2[dead2<0, sick2:=0] #prevent possible negatives
    
    #pop
    b2[,pop2:=shift(pop)-shift(all.dead), by=.(sex, location, cause, age)]
    #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
    b2[pop2<0, pop2:=0] #prevent possible negatives
    
    #all dead
    b2[, all.dead2:=sum(dead2), by=.(sex, location, year, age)]
    b2[, all.dead2:=all.dead2+(pop2*bg.mx.all)]
    b2[all.dead2<0, all.dead2:=0]
    
    #well
    b2[, well2:=pop2-all.dead2-sick2]
    b2[well2<0, well2:=0] #prevent possible negatives
    
    #re-combined into original data.table
    b2<-b2[year==2019+i & age2<96, c("age2", "newcases2", "sick2", "dead2", "well2", "pop2", "all.dead2", "sex", "location", "cause")]
    setnames(b2, "age2", "age")
    base_rates[year==2019+i & age>0, sick:=b2[,sick2]]
    base_rates[year==2019+i & age>0, newcases:=b2[,newcases2]]
    base_rates[year==2019+i & age>0, dead:=b2[,dead2]]
    base_rates[year==2019+i & age>0, well:=b2[,well2]]
    base_rates[year==2019+i & age>0, pop:=b2[,pop2]]
    base_rates[year==2019+i & age>0, all.dead:=b2[,all.dead2]]
    
  }
  
  base_rates%>%select(year, location, sex, age, cause, IR, CF, well, newcases, sick, dead, pop, all.dead)
}
