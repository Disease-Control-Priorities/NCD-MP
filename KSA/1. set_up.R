#File to get single-year age group estimates for historic rates (2009-2019):
#Pop
#All cause mortality
#Cause-specific mortality
#Prevalence

rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(purrr, data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)

###############################################
#Data
# https://vizhub.healthdata.org/gbd-results/?params=gbd-api-2019-permalink/11280a1c916618c1eea87cd7b937b8c4

causes<-c("Asthma", "Chronic kidney disease due to diabetes mellitus type 1",
          "Chronic kidney disease due to diabetes mellitus type 2",
          "Chronic kidney disease due to hypertension","Chronic obstructive pulmonary disease",
          "Diabetes mellitus type 1","Diabetes mellitus type 2","Esophageal cancer",
          "Hypertensive heart disease", "Intracerebral hemorrhage","Ischemic heart disease",
          "Ischemic stroke", "Larynx cancer", "Lip and oral cavity cancer","Nasopharynx cancer", 
          "Other pharynx cancer", "Stomach cancer", "Tracheal, bronchus, and lung cancer")

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

##############################################
df.gbd<-fread("data/IHME-GBD_2019_DATA-6d00461a-1.csv")%>%
  select(-upper, -lower)%>%
  filter(age %in% ages,
         location %in% locs)

unique(df.gbd$cause)
unique(df.gbd$age)

pop<-df.gbd%>%
  filter(measure == "Deaths")%>%
  spread(metric, val)%>%
  mutate(Nx = Number/(Rate/100000))%>%
  filter(cause=="All causes")%>%
  select(location, sex, age, year, Nx)

any(is.na(pop))

#pull out death counts
#dead.other is just All cause mortality minus that specific cause-mortality
deaths<-df.gbd%>%
  filter(cause %in% c("All causes", causes))%>%
  filter(measure == "Deaths", metric=="Number")%>%
  group_by(cause, location, sex, age, year)%>%
  summarise(val = sum(val))%>%
  spread(cause, val)%>%
  gather(cause, val, -location, -sex, -age, -year, -`All causes`)%>%
  rename(dead.all = `All causes`,
         dead.cause = val)%>%
  mutate(dead.cause = ifelse(is.na(dead.cause),0,dead.cause),
         dead.other = dead.all - dead.cause)

any(is.na(deaths))

BGmx<-deaths%>%group_by(location, sex, age, year)%>%
  summarise(dead.all = mean(dead.all),
            dead.cause = sum(dead.cause))%>%
  mutate(dead.bg = dead.all-dead.cause)%>%
  select(-dead.all, -dead.cause)

any(is.na(BGmx))

prev<-df.gbd%>%
  filter(measure == "Prevalence", metric=="Number", cause%in%causes)%>%
  select(val, cause, location, sex, age, year)%>%
  rename(sick = val)

any(is.na(prev))

df<-right_join(deaths, prev, by = c("location", "sex", "age", "year", "cause"))
any(is.na(df))

df<-left_join(df, pop)%>%mutate(well = Nx - sick)
any(is.na(df))


#get single age groups for base rates
base_rates<-df%>%
  mutate(all.mx = dead.all/Nx,
         bg.mx = (dead.all-dead.cause)/Nx,
         bg.mx.all = dead.other/Nx,
         prev.rate = sick/Nx,
         dis.mx = dead.cause/Nx)%>%
  select(-dead.all, -Nx, -dead.cause, -sick, -well, -dead.other)%>%
  mutate(start = gbd_age_start(age),
         stop = gbd_age_stop(age),
         midpoint = (start+stop)/2)%>%
  ungroup()%>%
  select(-age)%>%
  rename(age = midpoint)%>%
  group_by(year, location, cause, sex)%>%
  summarise(all.mx = approxfun(age, all.mx)(min(age):max(age)),
            dis.mx = approxfun(age, dis.mx)(min(age):max(age)),
            bg.mx = approxfun(age, bg.mx)(min(age):max(age)),
            bg.mx.all = approxfun(age, bg.mx.all)(min(age):max(age)),
            prev.rate = approxfun(age, prev.rate)(min(age):max(age)),
            age = min(age):max(age))

any(is.na(base_rates)) #warnings ok
unique(base_rates$age)

##################
#need to add ages 0 and 1 (assuming ~ 2 y/o)
#probably fine for NCDs, not all causes

temp<-base_rates%>%filter(age==2)

base_rates<-bind_rows(base_rates, temp%>%mutate(age=1))%>%
  bind_rows(., temp%>%mutate(age=0))

#add single age estimates from GBD 2019
pop9<-fread("data/IHME_GBD_2019_POP_SYA_2009_Y2021M01D28.csv")
pop10<-fread("data/IHME_GBD_2019_POP_SYA_2010_Y2021M01D28.csv")
pop11<-fread("data/IHME_GBD_2019_POP_SYA_2011_Y2021M01D28.csv")
pop12<-fread("data/IHME_GBD_2019_POP_SYA_2012_Y2021M01D28.csv")
pop13<-fread("data/IHME_GBD_2019_POP_SYA_2013_Y2021M01D28.csv")
pop14<-fread("data/IHME_GBD_2019_POP_SYA_2014_Y2021M01D28.csv")
pop15<-fread("data/IHME_GBD_2019_POP_SYA_2015_Y2021M01D28.csv")
pop16<-fread("data/IHME_GBD_2019_POP_SYA_2016_Y2021M01D28.csv")
pop17<-fread("data/IHME_GBD_2019_POP_SYA_2017_Y2021M01D28.csv")
pop18<-fread("data/IHME_GBD_2019_POP_SYA_2018_Y2021M01D28.csv")
pop19<-fread("data/IHME_GBD_2019_POP_SYA_2019_Y2021M01D28.csv")

gbdpop<-rbindlist(list(pop9,pop10,pop11,pop12,pop13,pop14,
                       pop15,pop16,pop17,pop18,pop19))%>%
  filter(sex_name!="both" & location_id!=533) #Remove Georgia the state

gbdpop$age_group<-as.numeric(gbdpop$age_group_name)
gbdpop$age_group[gbdpop$age_group_name=="<1 year"]<-0
gbdpop$age_group[gbdpop$age_group_name=="95 plus"]<-95

totalpop<-gbdpop%>%rename(location = location_name)%>%
  mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))
totalpop$sex_name[totalpop$sex_name=="male"]<-"Male"
totalpop$sex_name[totalpop$sex_name=="female"]<-"Female"

rm(pop9,pop10,pop11,pop12,pop13,pop14,
   pop15,pop16,pop17,pop18,pop19, gbdpop)

#Combine
base_rates<-left_join(as.data.frame(base_rates)%>%filter(year>=2009), 
                      totalpop%>%select(location, iso3, age_group, val, sex_name, year_id)%>%
                        rename(year = year_id, sex = sex_name, age = age_group, Nx = val))

unique(base_rates$location)
##################
#Save
write.csv(base_rates, "data/base_rates.csv", row.names = F)

##################
#Save gbd data
write.csv(df.gbd%>%filter(cause%in%c("All causes", causes)), "data/gbd_data.csv", row.names = F)

