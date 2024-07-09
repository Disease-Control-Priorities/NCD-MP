rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, readxl)   

##############################################
#Locations to be analyzed
locs<-c("Saudi Arabia")

#Mutually exclusive age groups
ages<-c("<5 years", "5-9 years", "10-14 years", "15-19 years", 
        "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", 
        "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years",
        "70-74 years", "75-79 years", "80-84", "85-89", "90-94", "95+ years")
##############################################
# custom functions
#############################################

gbd_age_start<-function(age){
      suppressWarnings(
            
            ifelse(age=="<5 years", 0, 
                   ifelse(age=="5-9 years", 5, as.numeric(substr(age,1,2))))
      )
}

gbd_age_stop<-function(age){
      suppressWarnings(
            
            ifelse(age=="<5 years", 4,
                   ifelse(age=="5-9 years", 9,
                          ifelse(age=="95+ years", 99, as.numeric(substr(age,4,5)))))
      )
}


'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"
#data permalink
dt<-read.csv("data/gbd_data.csv", stringsAsFactors = F)
dt<-data.table(dt)
unique(dt$age)
unique(dt$year)
unique(dt$cause)
#Get average annual rates of change by cohort
#as a function#
get.new.rates<-function(dt, year1, year2){

prevyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year1)%>%
                select(-c(measure, metric, year))%>%
                  rename(prev14 = val))

prevrtyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(prevrt14 = val))

deathyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(death14 = val))

deathrtyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(deathrt14 = val))

prevyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(prev19 = val))

prevrtyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(prevrt19 = val))

deathyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(death19 = val))

deathrtyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(deathrt19 = val))

mymerge<-function(x,y){merge.data.table(x,y, by=c("location", "age", "sex", "cause"))}

dt14<-Reduce(mymerge, list(prevyear1,prevrtyear1,deathyear1,deathrtyear1))
dt19<-Reduce(mymerge, list(prevyear2,prevrtyear2,deathyear2,deathrtyear2))

dt14[, pop14:=death14/deathrt14*100000]

dt14<-dt14%>%mutate(age2 = ifelse(age=="<5 years",    1, NA),
                    age2 = ifelse(age=="5-9 years",   2, age2),
                    age2 = ifelse(age=="10-14 years", 3, age2),
                    age2 = ifelse(age=="15-19 years", 4, age2),
                    age2 = ifelse(age=="20-24 years", 5, age2),
                    age2 = ifelse(age=="25-29 years", 6, age2),
                    age2 = ifelse(age=="30-34 years", 7, age2),
                    age2 = ifelse(age=="35-39 years", 8, age2),
                    age2 = ifelse(age=="40-44 years", 9, age2),
                    age2 = ifelse(age=="45-49 years", 10, age2),
                    age2 = ifelse(age=="50-54 years", 11, age2),
                    age2 = ifelse(age=="55-59 years", 12, age2),
                    age2 = ifelse(age=="60-64 years", 13, age2),
                    age2 = ifelse(age=="65-69 years", 14, age2),
                    age2 = ifelse(age=="70-74 years", 15, age2),
                    age2 = ifelse(age=="75-79 years", 16, age2),
                    age2 = ifelse(age=="80-84", 17, age2),
                    age2 = ifelse(age=="85-89", 18, age2),
                    age2 = ifelse(age=="90-94", 19, age2),
                    age2 = ifelse(age=="95+ years",   20, age2))

dt19[, pop19:=death19/deathrt19*100000]

dt19<-dt19%>%mutate(age2 = ifelse(age=="<5 years",    0, NA),
                    age2 = ifelse(age=="5-9 years",   1, age2),
                    age2 = ifelse(age=="10-14 years", 2, age2),
                    age2 = ifelse(age=="15-19 years", 3, age2),
                    age2 = ifelse(age=="20-24 years", 4, age2),
                    age2 = ifelse(age=="25-29 years", 5, age2),
                    age2 = ifelse(age=="30-34 years", 6, age2),
                    age2 = ifelse(age=="35-39 years", 7, age2),
                    age2 = ifelse(age=="40-44 years", 8, age2),
                    age2 = ifelse(age=="45-49 years", 9, age2),
                    age2 = ifelse(age=="50-54 years", 10, age2),
                    age2 = ifelse(age=="55-59 years", 11, age2),
                    age2 = ifelse(age=="60-64 years", 12, age2),
                    age2 = ifelse(age=="65-69 years", 13, age2),
                    age2 = ifelse(age=="70-74 years", 14, age2),
                    age2 = ifelse(age=="75-79 years", 15, age2),
                    age2 = ifelse(age=="80-84", 16, age2),
                    age2 = ifelse(age=="85-89", 17, age2),
                    age2 = ifelse(age=="90-94", 18, age2),
                    age2 = ifelse(age=="95+ years",   19, age2))

dt19[, age:=NULL]

dt<-merge(dt14, dt19, by=c("age2", "location", "sex", "cause"))

allcause<-dt[cause=="All causes"]
setnames(allcause, c("death14", "deathrt14", "death19", "deathrt19"),
         c("alldeath14", "alldeathrt14", "alldeath19", "alldeathrt19"))
allcause[,c("cause", "prev14", "prevrt14", "prev19", "prevrt19",
            "pop14", "pop19"):=NULL]

dt<-merge(dt, allcause, by=c("age2", "sex", "location", "age"))
dt<-dt[cause!="All causes"]

dt[, othermx14:=alldeath14-death14]
dt[, othermx19:=alldeath19-death19]
dt[, othermxrt14:=alldeathrt14 - deathrt14]
dt[, othermxrt19:=alldeathrt19 - deathrt19]

dt[, well14:=pop14-prev14-alldeath14]
dt[,well19:=pop19-prev19-alldeath19]

dt[, wellAARC:=log((well19/pop19)/(well14/pop14))/5]
dt[, sickAARC:=log(prevrt19/prevrt14)/5]
dt[, deadAARC:=log(deathrt19/deathrt14)/5]
dt[, deadotherAARC:=log(othermxrt19/othermxrt14)/5]

dt[sickAARC<0, sickAARC:=0]
dt[sickAARC>1, sickAARC:=0.99]
dt[deadAARC<0, deadAARC:=0]
dt[deadAARC>1, deadAARC:=0.99]
dt[deadotherAARC<0, deadotherAARC:=0]
dt[deadotherAARC>1, deadotherAARC:=0.99]


dt[,age2:=1]

rows<-as.numeric(nrow(dt))

reprow<-function(row){
  floor((row-1)/rows)
}

DT<-dt[rep(seq(1,nrow(dt)), 5)][, age2:=age2+reprow(.I)]

DT[age2==1, Well:=well14]
DT[age2==1, Sick:=prev14]
DT[age2==1, Dead:=death14]
DT[age2==1, DeadOther:=othermx14]

for(i in 2:5){
DT2<-DT[age2<=i &age2>=i-1]
DT2[, Well2:=shift(Well)*(1+wellAARC), by=.(age, sex, location, cause)]
DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Well2")]
DT[age2==i, Well:=DT2[,Well2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Sick2:=shift(Sick)*(1+sickAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Sick2")]
  DT[age2==i, Sick:=DT2[,Sick2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Dead2:=shift(Dead)*(1+deadAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Dead2")]
  DT[age2==i, Dead:=DT2[,Dead2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, DeadOther2:=shift(DeadOther)*(1+deadotherAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "DeadOther2")]
  DT[age2==i, DeadOther:=DT2[,DeadOther2]]
}

DT[, IR:=(Sick-(shift(Sick)-Dead))/shift(Well),  by=.(age, sex, location, cause)]
DT[, CF:=Dead/shift(Sick),  by=.(age, sex, location, cause)]

DT[ , avgIR:=mean(na.omit(IR)), by=.(age, sex, location, cause)]
DT[ , avgCF:=mean(na.omit(CF)), by=.(age, sex, location, cause)]
DT[ , start:=gbd_age_start(age)]
DT[ , midptage:= start+2]

DT_final<-unique(DT[,c("midptage", "age", "sex", "location", "cause", "avgIR", "avgCF")])
DT_final[, year:=year2]

DT_final[avgIR<0 | is.na(avgIR), avgIR:=0]
DT_final[avgCF>1, avgCF:=0.9]
DT_final[avgIR>1, avgIR:=0.9]
}
#end of function

#warnings ok
newrates<-get.new.rates(dt, 2009,2014)

for(i in 1:10){
  DT_final<-  get.new.rates(dt, 2009+i, 2014+i)
  newrates<-rbindlist(list(DT_final, newrates), use.names = T)
}

newrates$avgCF[is.na(newrates$avgCF)]<-0
DT_final<-newrates #store for debugging
any(is.na(DT_final))
unique(DT_final$age)

#assume 95+ is ~90-94
over95<-DT_final[age=="90-94 years"]
over95[, age:="95+ years"]
over95[, midptage:=97]

new<-rbindlist(list(over95, DT_final))
new<-new[order(location, sex, midptage)]
unique(new$midptage)

setnames(new, c("avgIR", "avgCF"), c("IR", "CF"))

### some causes at certain ages lost to NAs ###
## add and set NAs to zero ##
cse<-unique(new$cause)
unique(new$year) #why only 2014-2019?

df<-expand.grid(age = ages,
               cause = cse,
               year = 2014:2019,
               location = locs,
               sex = c("Female", "Male"))

tps<-full_join(df, new)%>%
      group_by(age, year, location, sex, cause)%>%
      mutate(midptage = gbd_age_start(age)+2,
             IR = ifelse(is.na(IR), 0 , IR),
             CF = ifelse(is.na(CF), 0, CF))

any(is.na(tps))

#### get single age groups #####
tps<-tps%>%ungroup()%>%select(-age)%>%
      rename(age = midptage)%>%
      group_by(year, location, cause, sex)%>%
      summarise(IR = approxfun(age, IR)(min(age):max(age)),
                CF = approxfun(age, CF)(min(age):max(age)),
                age = min(age):max(age))

any(is.na(tps))

##################
#need to add ages 0 and 1 (assuming ~ 2 y/o)
#probably fine for NCDs, not all causes

temp<-tps%>%filter(age==2)

tps<-bind_rows(tps, temp%>%mutate(age=1))%>%
      bind_rows(., temp%>%mutate(age=0))


ggplot(tps%>%filter(year==2019), 
       aes(x=age, y=IR, color=cause))+
  facet_wrap(~sex)+
  geom_point()

ggplot(tps%>%filter(year==2019), 
       aes(x=age, y=CF, color=cause))+
  facet_wrap(~sex)+
  geom_point()

############# Combine merge with other rates #############
other<-fread("data/base_rates.csv")
dataout<-right_join(other, tps, by=c("age", "sex", "location", "year", "cause"))%>%
      filter(age<=95)

any(is.na(dataout))
unique(other$year)

write.csv(dataout, "data/tps_new.csv", row.names = F)


