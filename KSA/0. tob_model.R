rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(purrr, data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl, zoo)


## data for 2013 & 2023, interpolate
df<-read.csv("data/tob_2023.csv", stringsAsFactors = F)

years<-data.frame(Year = 2013:2023)
ages<-data.frame(Age = unique(df$Age))
sex<-data.frame(Sex = c("Male", "Female"))

df2<-merge(years, ages)%>%
  merge(., sex)

tob<-full_join(df, df2)%>%
  arrange(Year)%>%
  group_by(Age, Sex)%>%
  mutate(current = na.approx(Current.smokers, na.rm = FALSE),
         ex = na.approx(Ex.smokers, na.rm=FALSE),
         never = 100 - current - ex)%>%
  filter(Age!="Total")%>%
  mutate(age = as.numeric(substr(Age, 1,2)))%>%
  ungroup()%>%
  select(age, Sex, Year, current, ex, never)

## assume 100 never smokers for age 10 (simplifying assumption) ##
add<-data.frame(Year = rep(2013:2023, 2),
                age = rep(10, 22),
                Sex = rep(c("Male", "Female"), 11),
                never = rep(100, 22),
                current = rep(0, 22),
                ex = rep(0,22)
                )

tob<-bind_rows(tob, add)%>%
  bind_rows(., tob%>%mutate(age = age+5,
                            never = NA,
                            current = NA,
                            ex = NA))%>%
  filter(age<=65)%>%
  group_by(Sex, Year)%>%
  arrange(age)%>%
  mutate(Never = na.approx(never, na.rm=FALSE),
         Current = na.approx(current, na.rm=FALSE),
         Ex = na.approx(ex, na.rm=FALSE))%>%
  select(age, Year, Sex, Never, Current, Ex)

## solve Markov to get transitions ##
## by 5 year gaps ##
#df<-tob
#year1<-2013
#year2<-2018

get.new.rates<-function(df, year1, year2){
  
  df1<-df%>%filter(Year==year1)%>%ungroup%>%select(-Year)
  
  df2<-df%>%filter(Year==year2)%>%
    rename(Ex2 = Ex, Current2 = Current, Never2 = Never)%>%
    mutate(age = age-5)%>%ungroup%>%select(-Year)
  
  dt<-left_join(df1, df2)%>%
    mutate(c_aarc = log((Current2/100)/(Current/100))/5,
           e_aarc = log((Ex2/100)/(Ex/100))/5,
           n_aarc = log((Never2/100)/(Never/100))/5
           )
  
  
  dt<-data.table(dt)
  dt[c_aarc>1, c_aarc:=0.99]
  dt[e_aarc>1, e_aarc:=0.99]
  dt[n_aarc>1, n_aarc:=0.99]
  
  
  rows<-as.numeric(nrow(dt))
  
  reprow<-function(row){
    floor((row-1)/rows)
  }
  
  DT<-dt[rep(seq(1,nrow(dt)), 5)][, age2:=1+reprow(.I)]
  
  DT[age2==1, current_smoker:=Current]
  DT[age2==1, never_smoker:=Never]
  DT[age2==1, ex_smoker:=Ex]

  for(i in 2:5){
    DT2<-DT[age2<=i &age2>=i-1]
    DT2[, current_smoker2:=shift(current_smoker)*(1+c_aarc), by=.(age, Sex)]
    DT2<-DT2[age2==i, c("age", "Sex", "current_smoker2")]
    DT[age2==i, current_smoker:=DT2[,current_smoker2]]
  }
  
  for(i in 2:5){
    DT2<-DT[age2<=i &age2>=i-1]
    DT2[, never_smoker2:=shift(never_smoker)*(1+n_aarc), by=.(age, Sex)]
    DT2<-DT2[age2==i, c("age", "Sex", "never_smoker2")]
    DT[age2==i, never_smoker:=DT2[,never_smoker2]]
  }
  
  for(i in 2:5){
    DT2<-DT[age2<=i &age2>=i-1]
    DT2[, ex_smoker2:=shift(ex_smoker)*(1+n_aarc), by=.(age, Sex)]
    DT2<-DT2[age2==i, c("age", "Sex", "ex_smoker2")]
    DT[age2==i, ex_smoker:=DT2[,ex_smoker2]]
  }
  
  DT[, init:=(current_smoker-(shift(current_smoker)-ex_smoker))/shift(never_smoker),  by=.(age, Sex)]
  DT[, cess:=ex_smoker/shift(current_smoker),  by=.(age, Sex)]
  #assume no regression from ex to current smokers, simplification
  
  DT[ , avg_init:=mean(na.omit(init)), by=.(age, Sex)]
  DT[ , avg_cess:=mean(na.omit(cess)), by=.(age, Sex)]

  DT_final<-unique(DT[,c("age", "Sex", "avg_init", "avg_cess")])
  DT_final[, year:=year2]
  
  DT_final[avg_init<0 | is.na(avg_init), avg_init:=0]
  DT_final[avg_cess<0 | is.na(avg_cess), avg_cess:=0]
  DT_final[avg_init>1, avg_init:=0.9]
  DT_final[avg_cess>1, avg_cess:=0.9]
  
}

newrates<-get.new.rates(tob, 2013,2018)

for(i in 1:5){
  DT_final<-  get.new.rates(tob, 2013+i, 2018+i)
  newrates<-rbindlist(list(DT_final, newrates), use.names = T)
}


ages<-data.frame(age=10:65)
years<-data.frame(year = 2018:2023)

add<-merge(ages, sex)%>%
  merge(., years)

tps<-full_join(newrates, add)%>%
  group_by(year, Sex)%>%
  arrange(age)%>%
  mutate(init = na.approx(avg_init, na.rm=FALSE),
         cess = na.approx(avg_cess, na.rm=FALSE))%>%
  select(-avg_init, -avg_cess)

## test it ##

age10<-data.frame(age=rep(10,10),
                  Sex = rep(c("Male", "Female"), 5),
                  year = rep(2019:2023, 2))%>%
  mutate(Current = 0,
         Ex = 0,
         Never = 100)


brates<-tob%>%filter(Year==2018)%>%
  rename(year = Year)%>%
  bind_rows(., age10)%>%
  full_join(., tps)%>%
  group_by(Sex, year)%>%
  arrange(age)%>%
  mutate(Never = na.approx(Never, na.rm = FALSE),
         Ex = na.approx(Ex, na.rm=FALSE),
         Current = na.approx(Current, na.rm = FALSE))


#run model
state.transition<-function(b_rates){

  #i<-1
  #STATE TRANSITIONS#
  for(i in 1:5){
    
    b2<-b_rates[year<=2018+i & year>=2018+i-1]
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
    b2<-b2[year==2018+i & age2<66, c("age2", "Current2", "Ex2", "Never2", "Sex")]
    setnames(b2, "age2", "age")
    b_rates[year==2018+i & age>10, Current:=b2[,Current2]]
    b_rates[year==2018+i & age>10, Ex:=b2[,Ex2]]
    b_rates[year==2018+i & age>10, Never:=b2[,Never2]]
    
  }
  
  b_rates
}

test<-state.transition(data.table(brates%>%arrange(age, year)))%>%
  mutate(check = Ex+Never+Current) #yep! 

## plot compared to data ##

plot<-bind_rows(tob%>%mutate(data="original")%>%rename(year = Year), 
                test%>%mutate(data="modelled"))


ggplot(plot%>%filter(year>2018), aes(x=age, y=Current, color=data))+
  geom_point()+
  facet_grid(year~Sex)


ggplot(plot%>%filter(year>2018), aes(x=age, y=Never, color=data))+
  geom_point()+
  facet_grid(year~Sex)

ggplot(plot%>%filter(year>2018), aes(x=age, y=Ex, color=data))+
  geom_point()+
  facet_grid(year~Sex)

####

ggplot(plot%>%filter(age==20), aes(x=year, y=Current, color=data))+
  geom_point()+
  facet_wrap(~Sex)  #current way too low over time, ex way to high

ggplot(plot%>%filter(age==20), aes(x=year, y=Ex, color=data))+
  geom_point()+
  facet_wrap(~Sex)

#cessation too high

ggplot(tps, aes(x=age, y=init, color=year))+
  geom_point()+
  facet_wrap(~Sex)+
  ylab("Initiation rate")

ggplot(tps, aes(x=age, y=init, color=year))+
  geom_point()+
  facet_wrap(~Sex)+
  ylab("Cessation rate")

## Hand tuning ##
##inflection point at 25
## another inflection after 55

test<-state.transition(data.table(brates%>%arrange(age, year)%>%
                                    mutate(init = ifelse(age>25, 0.1*init, 0.35*init),
                                           cess = ifelse(age>25, 0.025*cess, 0.1*cess),
                                           init = ifelse(init>0.9, 0.9, init))))%>%
  mutate(check = Ex+Never+Current) #yep! 




plot<-bind_rows(tob%>%mutate(data="original")%>%rename(year = Year), 
                test%>%mutate(data="modelled"))


ggplot(plot%>%filter(year>2018), aes(x=age, y=Current, color=data))+
  geom_point()+
  facet_grid(year~Sex)

ggplot(plot%>%filter(year>2018), aes(x=age, y=Never, color=data))+
  geom_point()+
  facet_grid(year~Sex)

ggplot(plot%>%filter(year>2018), aes(x=age, y=Ex, color=data))+
  geom_point()+
  facet_grid(year~Sex)


####
ggplot(plot%>%filter(age==25), aes(x=year, y=Current, color=data))+
  geom_point()+
  facet_wrap(~Sex) 

ggplot(plot%>%filter(age==25), aes(x=year, y=Ex, color=data))+
  geom_point()+
  facet_wrap(~Sex)

ggplot(plot%>%filter(age==25), aes(x=year, y=Never, color=data))+
  geom_point()+
  facet_wrap(~Sex)


###

ggplot(plot%>%filter(age==55), aes(x=year, y=Current, color=data))+
  geom_point()+
  facet_wrap(~Sex) 

ggplot(plot%>%filter(age==55), aes(x=year, y=Ex, color=data))+
  geom_point()+
  facet_wrap(~Sex)

ggplot(plot%>%filter(age==55), aes(x=year, y=Never, color=data))+
  geom_point()+
  facet_wrap(~Sex)


#Better!
#Save data and tps

write.csv(test,
          "data/tob_tps.csv",
          row.names = F)




