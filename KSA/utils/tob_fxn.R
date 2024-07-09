#run model
tob.model<-function(b_rates, year1, n, code){
  
  
  if(code=="baseline"){
    
    b_df<-b_rates
    
  }

  else{
    
  int.df<-tob_eff%>%filter(Code==code)%>%
    merge(., data.frame(age=10:65))%>%
    filter(age>=age_start & age<=age_stop)%>%
    select(cessation, initiation, age)
  
  b_df<-b_rates%>%left_join(., int.df)%>%
    mutate(cess = ifelse(year>=2024, cess*(1+ifelse(is.na(cessation),0, cessation)), cess),
                         init = ifelse(year>=2024, init*(1-ifelse(is.na(initiation),0, initiation)), init),
                         cess = ifelse(cess>1,1,cess))%>%
    select(-cessation, -initiation)
  
  }
  
  b_df<-data.table(b_df)
  
  #i<-1
  #STATE TRANSITIONS#
  for(i in 1:n){
    
    b2<-b_df[year<=year1+i & year>=year1+i-1]
    b2[,age2:=age+1]
    
    #sick
    b2[, Current2:=shift(Current)*(1-(cess)) + shift(Never)*init, by=.(Sex, age)]
    #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
    b2[Current2<0, Current2:=0] #prevent possible negatives
    
    #dead
    b2[, Ex2:=shift(Current)*cess + shift(Ex), by=.(Sex, age)]
    #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
    b2[Ex2<0, Ex2:=0] #prevent possible negatives
    
    #well
    b2[, Never2:=shift(Never)*(1-init), by=.(Sex, age)]
    b2[Never2<0, Never2:=0] #prevent possible negatives
    
    #re-combined into original data.table
    b2<-b2[year==year1+i & age2<66, c("age2", "Current2", "Ex2", "Never2", "Sex")]
    setnames(b2, "age2", "age")
    b_df[year==year1+i & age>10, Current:=b2[,Current2]]
    b_df[year==year1+i & age>10, Ex:=b2[,Ex2]]
    b_df[year==year1+i & age>10, Never:=b2[,Never2]]
    
  }
  
  b_df
  
}