
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(imputeTS)
library(readxl)

##############################
#data required:
  #incoming population (if not using fertility)
  #transition probabilities
  #initial state populations
##############################

#baseline rates calculated in file:
b_rates<-fread("data/tps_new.csv")
b_rates[CF>0.8, CF:=0.8]
b_rates[IR>0.8, IR:=0.8]
b_rates[CF<0, CF:=0]
b_rates[IR<0, IR:=0]

any(is.na(b_rates))

pop19<-b_rates%>%filter(age==0, year<2019)%>%
      select(age, year, sex, location, iso3, Nx)%>%
      unique()%>%
      rename(Nx0 = Nx)

sex_ratio<-pop19%>%group_by(sex, location, iso3)%>%
      summarise(Nx0=sum(Nx0))%>%
      spread(sex, Nx0)%>%
      mutate(ratio = Female/(Male+Female))%>%
      pull(ratio)

#pop0, assuming 50:50 sex ratio
popf<-read_excel("data/UN_PPP2022_Output_Births.xlsx", skip=16, sheet = 'Median')%>%
      rename(iso3 = `ISO3 Alpha-code`,
             location = `Region, subregion, country or area *`)%>%
      select(-Index, -Variant, -Notes, -`Location code`, -`ISO2 Alpha-code`,
             -`SDMX code**`, -Type, -`Parent code`)%>%
      filter(iso3 == "SAU")%>%
      gather(year, Nx0, -location, -iso3)%>%
      mutate(year = as.numeric(year),
             Nx0 = 1000*as.numeric(Nx0)*sex_ratio,
             sex = "Female",
             age = 0)

popm<-read_excel("data/UN_PPP2022_Output_Births.xlsx", skip=16, sheet = 'Median')%>%
      rename(iso3 = `ISO3 Alpha-code`,
             location = `Region, subregion, country or area *`)%>%
      select(-Index, -Variant, -Notes, -`Location code`, -`ISO2 Alpha-code`,
             -`SDMX code**`, -Type, -`Parent code`)%>%
      filter(iso3 == "SAU")%>%
      gather(year, Nx0, -location, -iso3)%>%
      mutate(year = as.numeric(year),
             Nx0 = 1000*as.numeric(Nx0)*(1-sex_ratio),
             sex = "Male",
             age = 0)

pop0<-bind_rows(popf, popm, pop19)%>%filter(year<=2050)

ggplot(pop0, aes(x=year, y=Nx0, color=sex))+
      geom_point()

#WPP predicting far more births that GBD - scale down to make consistent with historical GBD data
230000/320000
220000/300000
#roughly 70% difference

pop0<-bind_rows(popf%>%mutate(Nx0 = Nx0*0.7), popm%>%mutate(Nx0 = Nx0*0.7), pop19)%>%filter(year<=2050)

#interpolate births for years 2019,2020,2021
add<-pop0%>%filter(year==2018)

#warnings ok
pop0<-bind_rows(pop0, add%>%mutate(year=2019, Nx0=NA),
                add%>%mutate(year=2020, Nx0=NA),
                add%>%mutate(year=2021, Nx0=NA))%>%
      group_by(sex, location, age)%>%
      arrange(year)%>%
      mutate(Nx0 =na.interpolation(Nx0))

ggplot(pop0, aes(x=year, y=Nx0, color=sex))+
      geom_point()

pop0<-as.data.table(pop0%>%select(-iso3))

unique(b_rates$year)

##############################
#Run model to calibrate TPs 2009-2019#
##############################

state.transition<-function(b_rates, pop0,  IRadjust, CFadjust){

      base_rates<-merge(b_rates, pop0[year<=2019], by=c("year", "location", "sex", "age"), all=TRUE)%>%
            arrange(year, age)
      base_rates[age==0 & year>2014, Nx:=Nx0]
      base_rates[, Nx0:=NULL]

      ## calculate initial states for the incoming year 2009 and all years for age 0 population
      base_rates[year==2014 | age==0, sick:=Nx*prev.rate]
      base_rates[year==2014 | age==0, dead:=Nx*dis.mx]
      base_rates[year==2014 | age==0, well:=Nx*(1-(prev.rate+all.mx))]

      base_rates[age==0 | year==2014, pop:=Nx]
      base_rates[age==0 | year==2014, all.dead:=Nx*all.mx]

      base_rates[, IR:=IR*IRadjust]
      base_rates[, CF:=CF*CFadjust]

      base_rates[,IRadjust:=IRadjust]
      base_rates[,CFadjust:=CFadjust]

      base_rates[CF>0.9, CF:=0.9]
      base_rates[IR>0.9, IR:=0.9]

      #i<-1
      #STATE TRANSITIONS#
      for(i in 1:6){

            b2<-base_rates[year<=2014+i & year>=2014+i-1]
            b2[,age2:=age+1]

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
            b2[,all.dead2:=all.dead2+(pop2*bg.mx.all)]
            b2[all.dead2<0, all.dead2:=0]

            #well
            b2[, well2:=pop2-all.dead2-sick2]
            b2[well2<0, well2:=0] #prevent possible negatives

            #re-combined into original data.table
            b2<-b2[year==2014+i & age2<96, c("age2", "sick2", "dead2", "well2", "pop2", "all.dead2", "sex", "location", "cause")]
            setnames(b2, "age2", "age")
            base_rates[year==2014+i & age>0, sick:=b2[,sick2]]
            base_rates[year==2014+i & age>0, dead:=b2[,dead2]]
            base_rates[year==2014+i & age>0, well:=b2[,well2]]
            base_rates[year==2014+i & age>0, pop:=b2[,pop2]]
            base_rates[year==2014+i & age>0, all.dead:=b2[,all.dead2]]

      }

      base_rates%>%select(year, location, sex, age, cause, IR, CF, well, sick, dead, pop, all.dead, IRadjust, CFadjust)
}


test<-state.transition(b_rates, pop0, 1, 1)%>%filter(year>=2014)%>%
      group_by(location, year, cause, sex)%>%
      summarise(dead = sum(dead),
                sick = sum(sick))

any(is.na(test))
ggplot(test, aes(x=year, y=dead, color=cause))+
      geom_point()+
      facet_wrap(~sex)

out.df<-data.table(year = numeric(),
                   location = character(),
                   sex = character(),
                   age = numeric(),
                   cause = character(),
                   IR = numeric(),
                   CF = numeric(),
                   well = numeric(),
                   sick = numeric(),
                   dead = numeric(),
                   pop = numeric(),
                   all.dead = numeric(),
                   IRadjust = numeric(),
                   CFadjust = numeric())

##############################
#Run model 121 times
##############################

time1<-Sys.time()
for(i in -10:10){
      for(j in -10:10){
            temp<-state.transition(b_rates, pop0, 1+(i/100), 1+(j/100))%>%filter(year>=2014)
            out.df<-bind_rows(out.df, temp)
      }
}

time2<-Sys.time()
time2-time1 # ~60 minutes

any(is.na(out.df))
unique(out.df$CFadjust)

#Remove any scenarios where CF>0.9 or CF<0
#Remove any scenarios where IR>0.9 or IR<0

out.df<-out.df%>%filter(CF<=0.9 & CF>0 & IR>0 & IR<=0.9)

#################################
#Minimize root mean squared error compared to GBD 2009-2019
#Weight fatal estimates 2x non-fatal estimates
#################################

gbd<-read.csv("data/gbd_data.csv", stringsAsFactors = F)%>%
      filter(metric=="Number" & measure!="Incidence" & year>=2009)%>%
      spread(measure, val)%>%
      select(-metric)%>%
      mutate(Deaths = ifelse(is.na(Deaths), 0 , Deaths))

unique(gbd$age)
any(is.na(gbd))

#inv<-gbd%>%filter(is.na(Deaths))

#Put modeled results back into 5-year age groups
dt<-as.data.table(out.df)
dt[,intervention:=paste0("IR", IRadjust, "CF", CFadjust)]

dt17<-dt[age<5,]
dt17[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt17[,age:=NULL]
dt17[,age:="<5 years"]

dt18<-dt[age>=5 & age<10,]
dt18[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt18[,age:=NULL]
dt18[,age:="5-9 years"]

dt19<-dt[age>=10 & age<15,]
dt19[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt19[,age:=NULL]
dt19[,age:="10-14 years"]

dt20<-dt[age>=15 & age<20,]
dt20[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt20[,age:=NULL]
dt20[,age:="15-19 years"]

dt1<-dt[age>=20 & age<25,]
dt1[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt1[,age:=NULL]
dt1[,age:="20-24 years"]

dt2<-dt[age>=25 & age<30,]
dt2[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt2[,age:=NULL]
dt2[,age:="25-29 years"]

dt3<-dt[age>=30 & age<35,]
dt3[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt3[,age:=NULL]
dt3[,age:="30-34 years"]

dt4<-dt[age>=35 & age<40,]
dt4[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt4[,age:=NULL]
dt4[,age:="35-39 years"]

dt5<-dt[age>=40 & age<45,]
dt5[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt5[,age:=NULL]
dt5[,age:="40-44 years"]

dt6<-dt[age>=45 & age<50,]
dt6[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt6[,age:=NULL]
dt6[,age:="45-49 years"]

dt7<-dt[age>=50 & age<55,]
dt7[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt7[,age:=NULL]
dt7[,age:="50-54 years"]

dt8<-dt[age>=55 & age<60,]
dt8[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt8[,age:=NULL]
dt8[,age:="55-59 years"]

dt9<-dt[age>=60 & age<65,]
dt9[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt9[,age:=NULL]
dt9[,age:="60-64 years"]

dt10<-dt[age>=65 & age<70,]
dt10[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt10[,age:=NULL]
dt10[,age:="65-69 years"]

dt11<-dt[age>=70 & age<75,]
dt11[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt11[,age:=NULL]
dt11[,age:="70-74 years"]

dt12<-dt[age>=75 & age<80,]
dt12[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt12[,age:=NULL]
dt12[,age:="75-79 years"]

dt13<-dt[age>=80 & age<85,]
dt13[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt13[,age:=NULL]
dt13[,age:="80-84"]

dt14<-dt[age>=85 & age<90,]
dt14[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt14[,age:=NULL]
dt14[,age:="85-89"]

dt15<-dt[age>=90 & age<95,]
dt15[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt15[,age:=NULL]
dt15[,age:="90-94"]

dt16<-dt[age>=95,]
dt16[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt16[,age:=NULL]
dt16[,age:="95+ years"]

dt<-rbindlist(list(dt1,dt2,dt3,dt4,dt5,
                   dt6,dt7,dt8,dt9,dt10,
                   dt11, dt12, dt13, dt14,
                   dt15, dt16, dt17, dt18,
                   dt19, dt20))

rm(dt1,dt2,dt3,dt4,dt5,
   dt6,dt7,dt8,dt9,dt10,
   dt11, dt12, dt13, dt14,
   dt15, dt16, dt17, dt18,
   dt19, dt20)

dt<-dt[, c("year", "location", "cause", "sex", "age", "sick", "dead", "intervention")]
setnames(dt, c("sick", "dead"), c("Prevalence", "Deaths"))
dt<-dt[,c("age", "sex", "location", "year", "intervention", "Deaths", "Prevalence", "cause")]


#############################
#Compare to GBD
#############################
gbd2<-data.table(gbd%>%rename(gbdDeaths = Deaths, gbdPrev = Prevalence))
data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])

any(is.na(data2))
unique(data2$age)

data2<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))

data2$derror<-(data2$gbdDeaths-data2$Deaths)^2
data2$perror<-(data2$gbdPrev-data2$Prevalence)^2

data2<-data2%>%group_by(location, sex, cause, intervention, age)%>%
      summarise(RMSE_deaths=sqrt(mean(derror)),RMSE_prev=sqrt(mean(perror)))

data.adj<-as.data.table(data2)
data.adj[, error:=2*RMSE_deaths + RMSE_prev]
test<-data.adj[ , .SD[which.min(error)], by=.(location, sex, cause, age)]


###########################
#Plot and pull adjustments
###########################


library(stringr)
test[, IRadjust:=NA]
test[, CFadjust:=NA]

test[, IRadjust:=as.numeric(gsub(".*IR(.+)CF.*", "\\1", intervention))]
test[, CFadjust:=as.numeric(gsub(".*CF(.+).*", "\\1", intervention))]

data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])
plot<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))%>%
      right_join(., test)%>%
      ungroup()%>%
      select(year, sex, cause, location, age, Deaths, Prevalence, gbdDeaths, gbdPrev)%>%
      gather(metric, val, -location, -sex, -age,-year,-cause)

ggplot(plot%>%filter(cause=="Ischemic heart disease", metric%in%c("Deaths", "gbdDeaths")),
       aes(x=year, y=val, color=metric, shape = sex))+
      geom_point()+
      facet_wrap(~age)

ggplot(plot%>%filter(cause=="Ischemic heart disease",sex=="Female", metric%in%c("Prevalence", "gbdPrev")),
       aes(x=year, y=val, color=metric))+
      geom_point()+
      facet_wrap(~age)



#review for all causes
cse<-unique(plot$cause)

for(c in cse){

      ggplot(plot%>%filter(cause==c, metric%in%c("Deaths", "gbdDeaths")),
             aes(x=year, y=val, color=metric, shape=sex))+
            geom_point()+
            facet_wrap(~age)+
            ggtitle(paste(c, "deaths"))

      ggsave(paste0("check_plots/deaths_", c, ".jpeg"), height=8, width=12)

      ggplot(plot%>%filter(cause==c, metric%in%c("Prevalence", "gbdPrev")),
             aes(x=year, y=val, color=metric, shape=sex))+
            geom_point()+
            facet_wrap(~age)+
            ggtitle(paste(c, "cases"))

      ggsave(paste0("check_plots/prev_", c, ".jpeg"), height=8, width=12)

}


adjustments<-test[, c("sex", "location", "cause", "IRadjust", "CFadjust", "age")]

write.csv(adjustments, "data/temp_save.csv")
adjustments<-read.csv("data/temp_save.csv", stringsAsFactors = F)%>%select(-X)
any(is.na(adjustments))

#########
#run it again
#########
age_match<-data.frame(age=0:95)%>%
      mutate(age.group = ifelse(age<5, "<5 years", NA),
             age.group = ifelse(age>=5 & age<10, "5-9 years", age.group),
             age.group = ifelse(age>=10 & age<15, "10-14 years", age.group),
             age.group = ifelse(age>=15 & age<20, "15-19 years", age.group),
             age.group = ifelse(age>=20 & age<25, "20-24 years", age.group),
             age.group = ifelse(age>=25 & age<30, "25-29 years", age.group),
             age.group = ifelse(age>=30 & age<35, "30-34 years", age.group),
             age.group = ifelse(age>=35 & age<40, "35-39 years", age.group),
             age.group = ifelse(age>=40 & age<45, "40-44 years", age.group),
             age.group = ifelse(age>=45 & age<50, "45-49 years", age.group),
             age.group = ifelse(age>=50 & age<55, "50-54 years", age.group),
             age.group = ifelse(age>=55 & age<60, "55-59 years", age.group),
             age.group = ifelse(age>=60 & age<65, "60-64 years", age.group),
             age.group = ifelse(age>=65 & age<70, "65-69 years", age.group),
             age.group = ifelse(age>=70 & age<75, "70-74 years", age.group),
             age.group = ifelse(age>=75 & age<80, "75-79 years", age.group),
             age.group = ifelse(age>=80 & age<85, "80-84", age.group),
             age.group = ifelse(age>=85 & age<90, "85-89", age.group),
             age.group = ifelse(age>=90 & age<95, "90-94", age.group),
             age.group = ifelse(age==95, "95+ years", age.group))


b_rates<-left_join(b_rates, age_match)%>%
      left_join(.,adjustments%>%rename(age.group = age))%>%
      mutate(CFadjust = ifelse(is.na(CFadjust), 0, CFadjust),
             IRadjust = ifelse(is.na(IRadjust), 0, IRadjust),
             CF = CF*CFadjust,
             IR = IR*IRadjust)%>%
      select(-age.group, -IRadjust, -CFadjust)%>%
      filter(year>=2014)


out.df<-data.table(year = numeric(),
                   location = character(),
                   sex = character(),
                   age = numeric(),
                   cause = character(),
                   IR = numeric(),
                   CF = numeric(),
                   well = numeric(),
                   sick = numeric(),
                   dead = numeric(),
                   pop = numeric(),
                   all.dead = numeric(),
                   IRadjust = numeric(),
                   CFadjust = numeric())


##############################
#Run model 121 times
##############################

time1<-Sys.time()
for(i in -10:10){
      for(j in -20:10){
            temp<-state.transition(b_rates, pop0, 1+(i/100), 1+(j/100))%>%filter(year>=2014)
            out.df<-bind_rows(out.df, temp)
      }
}

time2<-Sys.time()
time2-time1 # ~60 minutes

any(is.na(out.df))
unique(out.df$CFadjust)

#Remove any scenarios where CF>0.9 or CF<0
#Remove any scenarios where IR>0.9 or IR<0

out.df<-out.df%>%filter(CF<=0.9 & CF>0 & IR>0 & IR<=0.9)


##################################################
#Put modeled results into 5-year age groups
dt<-as.data.table(out.df)
dt[,intervention:=paste0("IR", IRadjust, "CF", CFadjust)]

dt17<-dt[age<5,]
dt17[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt17[,age:=NULL]
dt17[,age:="<5 years"]

dt18<-dt[age>=5 & age<10,]
dt18[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt18[,age:=NULL]
dt18[,age:="5-9 years"]

dt19<-dt[age>=10 & age<15,]
dt19[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt19[,age:=NULL]
dt19[,age:="10-14 years"]

dt20<-dt[age>=15 & age<20,]
dt20[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt20[,age:=NULL]
dt20[,age:="15-19 years"]

dt1<-dt[age>=20 & age<25,]
dt1[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt1[,age:=NULL]
dt1[,age:="20-24 years"]

dt2<-dt[age>=25 & age<30,]
dt2[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt2[,age:=NULL]
dt2[,age:="25-29 years"]

dt3<-dt[age>=30 & age<35,]
dt3[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt3[,age:=NULL]
dt3[,age:="30-34 years"]

dt4<-dt[age>=35 & age<40,]
dt4[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt4[,age:=NULL]
dt4[,age:="35-39 years"]

dt5<-dt[age>=40 & age<45,]
dt5[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt5[,age:=NULL]
dt5[,age:="40-44 years"]

dt6<-dt[age>=45 & age<50,]
dt6[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt6[,age:=NULL]
dt6[,age:="45-49 years"]

dt7<-dt[age>=50 & age<55,]
dt7[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt7[,age:=NULL]
dt7[,age:="50-54 years"]

dt8<-dt[age>=55 & age<60,]
dt8[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt8[,age:=NULL]
dt8[,age:="55-59 years"]

dt9<-dt[age>=60 & age<65,]
dt9[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt9[,age:=NULL]
dt9[,age:="60-64 years"]

dt10<-dt[age>=65 & age<70,]
dt10[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt10[,age:=NULL]
dt10[,age:="65-69 years"]

dt11<-dt[age>=70 & age<75,]
dt11[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt11[,age:=NULL]
dt11[,age:="70-74 years"]

dt12<-dt[age>=75 & age<80,]
dt12[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt12[,age:=NULL]
dt12[,age:="75-79 years"]

dt13<-dt[age>=80 & age<85,]
dt13[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt13[,age:=NULL]
dt13[,age:="80-84"]

dt14<-dt[age>=85 & age<90,]
dt14[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt14[,age:=NULL]
dt14[,age:="85-89"]

dt15<-dt[age>=90 & age<95,]
dt15[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt15[,age:=NULL]
dt15[,age:="90-94"]

dt16<-dt[age>=95,]
dt16[,c("sick", "dead") := list(sum(sick), sum(dead)), by=.(cause, sex, year, intervention, location)]
dt16[,age:=NULL]
dt16[,age:="95+ years"]

dt<-rbindlist(list(dt1,dt2,dt3,dt4,dt5,
                   dt6,dt7,dt8,dt9,dt10,
                   dt11, dt12, dt13, dt14,
                   dt15, dt16, dt17, dt18,
                   dt19, dt20))

rm(dt1,dt2,dt3,dt4,dt5,
   dt6,dt7,dt8,dt9,dt10,
   dt11, dt12, dt13, dt14,
   dt15, dt16, dt17, dt18,
   dt19, dt20)

dt<-dt[, c("year", "location", "cause", "sex", "age", "sick", "dead", "intervention")]
setnames(dt, c("sick", "dead"), c("Prevalence", "Deaths"))
dt<-dt[,c("age", "sex", "location", "year", "intervention", "Deaths", "Prevalence", "cause")]


#############################
#Compare to GBD
#############################
gbd2<-data.table(gbd%>%rename(gbdDeaths = Deaths, gbdPrev = Prevalence))
data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])

any(is.na(data2))

data2<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))

data2$derror<-(data2$gbdDeaths-data2$Deaths)^2
data2$perror<-(data2$gbdPrev-data2$Prevalence)^2

data2<-data2%>%group_by(location, sex, cause, intervention, age)%>%summarise(RMSE_deaths=sqrt(mean(derror)),
                                                                             RMSE_prev=sqrt(mean(perror)))

data.adj<-as.data.table(data2)
data.adj[, error:=RMSE_deaths]
test<-data.adj[ , .SD[which.min(error)], by=.(location, sex, cause, age)]

###########################
#Plot and pull adjustments
###########################

test[, IRadjust:=NA]
test[, CFadjust:=NA]

test[, IRadjust:=as.numeric(gsub(".*IR(.+)CF.*", "\\1", intervention))]
test[, CFadjust:=as.numeric(gsub(".*CF(.+).*", "\\1", intervention))]

data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])
plot<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))%>%
      right_join(., test)%>%
      ungroup()%>%
      select(year, sex, cause, location, age, Deaths, Prevalence, gbdDeaths, gbdPrev)%>%
      gather(metric, val, -location, -sex, -age,-year,-cause)


for(c in cse){

      ggplot(plot%>%filter(cause==c, metric%in%c("Deaths", "gbdDeaths")),
             aes(x=year, y=val, color=metric, shape=sex))+
            geom_point()+
            facet_wrap(~age)+
            ggtitle(paste(c, "deaths"))

      ggsave(paste0("check_plots2/deaths_", c, ".jpeg"), height=8, width=12)

      ggplot(plot%>%filter(cause==c, metric%in%c("Prevalence", "gbdPrev")),
             aes(x=year, y=val, color=metric, shape=sex))+
            geom_point()+
            facet_wrap(~age)+
            ggtitle(paste(c, "cases"))

      ggsave(paste0("check_plots2/prev_", c, ".jpeg"), height=8, width=12)

}

adjustments<-test[, c("sex", "location", "cause", "IRadjust", "CFadjust", "age")]%>%
      rename(age.group = age)%>%
      left_join(., age_match)%>%
      select(-age.group)

#write.csv(adjustments, "adjustments.csv", row.names = F)

ab_rates<-left_join(b_rates, adjustments)%>%
      mutate(CFadjust = ifelse(is.na(CFadjust), 0, CFadjust),
             IRadjust = ifelse(is.na(IRadjust), 0, IRadjust),
             IR = IR*IRadjust,
             CF = CF*CFadjust)%>%
      select(-IRadjust, -CFadjust)

any(is.na(ab_rates))

write.csv(ab_rates, "data/tps_adjusted.csv", row.names = F)
write.csv(pop0, "data/pop0.csv", row.names=F)
