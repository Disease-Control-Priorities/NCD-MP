rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)

#################################################################################
## Tobacco intervention effects ##
tob<-read.csv("data/tob_tps.csv", stringsAsFactors = F)%>%
  filter(year>=2019)

#repeat for 2023-2040 (assume constant TP)
add<-tob%>%filter(year==2023)%>% 
  select(-year)

for(i in 2024:2030){
  tob<-bind_rows(tob, add%>%mutate(year=i))
}

ggplot(tob%>%filter(age==55), aes(x=year, y=Current, color=Sex))+
  geom_point()

unique(tob$age)

tob_RR<-read.csv("data/tobacco_RRs.csv", stringsAsFactors = F)%>%
  rename(sex=Sex)


tob_eff<-read.csv("data/tob_effs.csv", stringsAsFactors = F)%>%
  mutate(age_start = 10,
         age_stop = ifelse(Age=="10 to 25", 25, 65))%>%
  select(-Age, -Source)%>%
  spread(Target, RRR)%>%
  mutate(cessation = 1-(1-ifelse(is.na(cessation), 0, cessation))*(1-ifelse(is.na(both), 0, both)),
         initiation = 1-(1-ifelse(is.na(initiation), 0, initiation))*(1-ifelse(is.na(both), 0, both))
         )%>%
  select(-both)

##all ints

all_eff<-tob_eff%>%
  filter(age_stop==25)%>%
  group_by(age_start, age_stop)%>%
  summarise(cessation = 1-prod((1-cessation)),
            initiation = 1-prod((1-initiation)))%>%
  bind_rows(., tob_eff%>%group_by(age_start)%>%
              summarise(cessation = 1-prod((1-cessation)),
                        initiation = 1-prod((1-initiation)))%>%
              mutate(age_stop = 65,
                     age_start=26)
            )%>%
  mutate(Code=1, Intervention="All tobacco interventions")

#combine intervention impacts and cohort effect for initiation
tob_eff<-bind_rows(tob_eff, all_eff)%>%
  merge(data.frame(age=10:65))%>%
  merge(data.frame(year=2019:2030))%>%
  mutate(cessation = ifelse(age>=age_start & age<=age_stop, cessation, 0),
         initiation2 = ifelse(age>=age_start & (age-age_stop)<=(year-2024), initiation, 0))%>%
  group_by(Code, Intervention, age, year)%>%
  summarise(cessation = 1-prod((1-cessation)),
            initiation = 1-prod((1-initiation)))

#source("utils/tob_fxn.R")

#apply effects in 2024
tob_prime<-left_join(tob, tob_eff)%>%
  mutate(Ex = ifelse( year>= 2024, Ex + Current*cessation, Ex),
         Current = ifelse(year>=2024, Current*(1-cessation)*(1-initiation), Current),
         Never = 100-Ex-Current)

ggplot(tob_prime%>%filter(age==27), aes(x=year, y=Current, color=Sex))+
  geom_point()+
  facet_wrap(~Code)

ggplot(tob_prime%>%filter(age==55), aes(x=year, y=Ex, color=Sex))+
  geom_point()+
  facet_wrap(~Code)

#get alphas
impact.tob<-tob%>%
  select(-init, -cess, -check)%>%
  rename(Nonsmoker = Never, Smoker = Current, Exsmoker=Ex, sex=Sex)%>%
  gather(Category, pi, -sex, -age, -year)%>%
  unique()%>%
  left_join(., tob_RR)%>%
  group_by(sex, age, year, Cause)%>%
  summarise(alpha =sum(pi*RR))%>%
  left_join(., tob_RR)

any(is.na(impact.tob))

#run tobacco model for each intervention
#tob_df<-tob.model(tob, 2020, 10, "baseline")%>%mutate(Code=0)
#test<-tob.model(tob, 2020, 10, 1.0)

#for(i in c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7)){
#  tob_df<-bind_rows(tob_df, tob.model(tob, 2020, 10, i)%>%mutate(Code=i))
  
#}

#any(is.na(tob_df))

pi_star<-tob_prime%>%
  select(age, year, sex=Sex, Nonsmoker = Never, Smoker = Current, Exsmoker = Ex, Code)%>%
  gather(Category, pi2, -age, -year, -sex, -Code)

impact.tob<-left_join(impact.tob, pi_star)%>%
  mutate(impact = (RR/alpha)*pi2)%>%
  group_by(Code, Cause, age, sex, year)%>%
  summarise(impact = sum(impact))%>%
  filter(Code>0)%>%
  mutate(target="IR")

any(is.na(impact.tob))

## add impact on case fatality
tob.paf<-read.csv("data/tob_PAFs.csv", stringsAsFactors = F)%>%
  mutate(target="CF")%>%
  merge(., data.frame(year=2019:2030))%>%
  merge(., data.frame(age=10:65))%>%
  merge(., data.frame(sex=c("Male", "Female")))%>%
  merge(., tob_eff%>%filter(cessation!=0)%>%
  select(Code, cessation))%>%
  left_join(., tob%>%select(age, year, sex=Sex, Current))%>%
  mutate(impact = 1-PAF*cessation*Current/100,
         impact = ifelse(year<2024, 1, impact))%>%
  select(-PAF, -cessation, -Current)

any(is.na(tob.paf))

## change initiation calculation ##
#impact.tob<-read.csv("data/tob_PAFs.csv", stringsAsFactors = F)%>%
#  mutate(target="IR")%>%
#  merge(., data.frame(year=2019:2030))%>%
#  merge(., data.frame(age=10:65))%>%
#  merge(., data.frame(sex=c("Male", "Female")))%>%
#  merge(., tob_eff%>%
 #         mutate(rrr = 1-(1-cessation)*(1-initiation))%>%
#          select(Code, rrr, age_start, age_stop)
#        )%>%
#  left_join(., tob%>%select(age, year, sex=Sex))%>%
#  mutate(impact = ifelse(age>=age_start & age<=age_stop, 1-PAF*rrr, NA))%>%
#  na.omit()%>%
#  mutate(impact = ifelse(year<2024, 1, impact))%>%
#  select(-PAF, -rrr, -age_start, -age_stop)



#Combine
impact.tob<-bind_rows(impact.tob, tob.paf%>%select(-Intervention))

#extend for 0-24 and 66-95
add<-impact.tob%>%filter(age==65)
any(is.na(add))
unique(impact.tob$target)

#this is janky
for(i in 0:9){
  impact.tob<-bind_rows(impact.tob, add%>%mutate(age=i, impact=1))
}

for(i in 66:95){
  
  impact.tob<-bind_rows(impact.tob, add%>%mutate(age=i))
}


any(is.na(impact.tob))

  
#################################################################################

# Pull second hand smoke impacts

ints<-read.csv("data/interventions.csv", stringsAsFactors = F)

impact.ssh<-ints%>%filter(Intervention=="Second-hand smoke reduction")%>%
  mutate(Code=2.0)%>%
  select(-source)%>%
  merge(., data.frame(year=2019:2030))%>%
  mutate(impact = (1-RR)*(year-2023)/7,
         impact = 1-ifelse(year<2023, 0, impact))%>%
  select(Code, Cause=cause, target, year, impact)%>%
  merge(., data.frame(age=0:95))%>%
  merge(., data.frame(sex = c("Male", "Female")))%>%
  mutate(impact = ifelse(age<10, 1, impact))


#################################################################################

## HTN/DM intervention effects ##

library(zoo)

cascade<-read.csv("data/htn_dm_cascade.csv", stringsAsFactors = F)%>%
  select(year, sex, HTN, Aware, Treated, Control, DM, connection.to.care, quality.of.care)


screen1<-cascade%>%filter(year==2023)%>%
  select(-year)%>%
  merge(., data.frame(year=2023:2030))%>%
  mutate(Aware = ifelse(year==2030, 0.75,
                        ifelse(year<=2023, Aware, NA)))%>%
  group_by(sex)%>%
  mutate(Aware = na.approx(Aware),
         Treated = Aware * connection.to.care,
         Control = Treated * quality.of.care)%>%
  bind_rows(., cascade%>%filter(year<2023))

screen2<-cascade%>%filter(year==2023)%>%
  select(-year)%>%
  merge(., data.frame(year=2023:2030))%>%
  mutate(Aware = ifelse(year==2030, 0.95,
                        ifelse(year<=2023, Aware, NA)))%>%
  group_by(sex)%>%
  mutate(Aware = na.approx(Aware),
         Treated = Aware * connection.to.care,
         Control = Treated * quality.of.care)%>%
  bind_rows(., cascade%>%filter(year<2023))
  
treat1<-cascade%>%filter(year==2023)%>%
  select(-year)%>%
  merge(., data.frame(year=2023:2030))%>%
  mutate(connection.to.care = ifelse(year==2030, 0.95,
                        ifelse(year<=2023, connection.to.care, NA)),
         quality.of.care = ifelse(year==2030, 0.75,
                                     ifelse(year<=2023, quality.of.care, NA)))%>%
  group_by(sex)%>%
  mutate(connection.to.care = na.approx(connection.to.care),
         quality.of.care = na.approx(quality.of.care),
         Treated = Aware * connection.to.care,
         Control = Treated * quality.of.care)%>%
  bind_rows(., cascade%>%filter(year<2023))

treat2<-screen2%>%
  mutate(connection.to.care = ifelse(year==2030, 0.95,
                                     ifelse(year<=2023, connection.to.care, NA)),
         quality.of.care = ifelse(year==2030, 0.75,
                                  ifelse(year<=2023, quality.of.care, NA)))%>%
  group_by(sex)%>%
  mutate(connection.to.care = na.approx(connection.to.care),
         quality.of.care = na.approx(quality.of.care),
         Treated = Aware * connection.to.care,
         Control = Treated * quality.of.care)


ggplot(treat2, aes(x=year, y=Control, color=sex))+
  geom_point()

test<-ints%>%filter(Intervention!="Second-hand smoke reduction")%>%
  select(-source)%>%spread(Intervention, RR)

impact.htn<-bind_rows(screen1%>%mutate(Code=5.1), screen2%>%mutate(Code=5.2),
                      treat1%>%mutate(Code=5.3), treat2%>%mutate(Code = 5))%>%
  left_join(., cascade%>%filter(year==2023)%>%
                        rename(base.treat = Treated, base.control=Control)%>%
                        select(sex, base.treat, base.control))%>%
  mutate(treat.inc = Treated-base.treat,
         treat.inc = ifelse(treat.inc<0.00001, 0,treat.inc),
         control.inc = Control-base.control,
         control.inc = ifelse(control.inc<0.00001, 0, control.inc))%>%
  select(year, sex, Code, treat.inc, control.inc, HTN)%>%
  merge(., test)%>%
  mutate(treat.impact = (treat.inc-control.inc)*(1-`Hypertension treatment`),
         control.impact = control.inc*(1-`Hypertension control`),
         impact = (1-treat.impact)*(1-control.impact),
         impact = ifelse(target=="CF", 1-(1-impact)*HTN, impact))%>%
  select(year, sex, Code, target, cause, impact)%>%
  na.omit()%>%
  filter(year>=2019)%>%
  merge(., data.frame(age=0:95))

impact.dm<-bind_rows(screen1%>%mutate(Code=7.1), screen2%>%mutate(Code=7.2),
                     treat1%>%mutate(Code=7.3), treat2%>%mutate(Code = 7))%>%
  left_join(., cascade%>%filter(year==2023)%>%
              rename(base.treat = Treated, base.control=Control)%>%
              select(sex, base.treat, base.control))%>%
  mutate(treat.inc = Treated-base.treat,
         treat.inc = ifelse(treat.inc<0.00001, 0,treat.inc),
         control.inc = Control-base.control,
         control.inc = ifelse(control.inc<0.00001, 0, control.inc))%>%
  select(year, sex, Code, treat.inc, control.inc, DM)%>%
  merge(., test)%>%
  mutate(treat.impact = (treat.inc-control.inc)*(1-`Diabetes treatment`),
         control.impact = control.inc*(1-`Diabetes control`),
         impact = (1-treat.impact)*(1-control.impact),
         impact = ifelse(target=="CF", 1-(1-impact)*DM, impact))%>%
  select(year, sex, Code, target, cause, impact)%>%
  na.omit()%>%
  filter(year>=2019)%>%
  merge(., data.frame(age=0:95))

#################################################################################

interv.df<-bind_rows(impact.tob%>%rename(cause = Cause), 
                     impact.ssh%>%rename(cause = Cause),
                     impact.htn, impact.dm)%>%
  filter(target=="IR")%>%
  rename(IR_eff=impact)%>%
  select(-target)%>%
  full_join(.,bind_rows(impact.tob%>%rename(cause = Cause), 
                        impact.ssh%>%rename(cause = Cause),
                        impact.htn, impact.dm)%>%
              filter(target=="CF")%>%
              rename(CF_eff=impact)%>%
              select(-target))%>%
  mutate(IR_eff = ifelse(is.na(IR_eff), 1, IR_eff),
         CF_eff = ifelse(is.na(CF_eff), 1, CF_eff))

any(is.na(interv.df))

## Main model ##

loc<-"Saudi Arabia"
pop0<-read.csv("data/pop0.csv", stringsAsFactors = F) 
pop0<-data.table(pop0)

b_rates<-read.csv("data/tps_adjusted.csv", stringsAsFactors = F)%>%
  filter(year>=2019)

#Cause list 
cse<-unique(b_rates$cause)
unique(interv.df$cause)%in%cse   #oops, need to add breast and cervical cancers
cse

any(is.na(b_rates))

#repeat b_rates for 2019-2040
add<-b_rates%>%filter(year==2019)

for (i in 2020:2030){
      b_rates<-bind_rows(b_rates, add%>%mutate(year=i))
}


b_rates<-data.table(b_rates)

any(is.na(b_rates))

##############################################

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

#Test it
test<-state.transition(b_rates, pop0, "baseline")
any(is.na(test))

#plot 
p_test<-test%>%group_by(year, cause, sex)%>%
  summarise(dead=sum(dead))%>%
  mutate(scenario="baseline")

ggplot(p_test, aes(x=year, y=dead, color=cause))+
  geom_point()+
  facet_wrap(~sex)

test2<-state.transition(b_rates, pop0, 1.1)
any(is.na(test2))

p_test2<-test2%>%group_by(year, cause, sex)%>%
  summarise(dead=sum(dead))%>%
  mutate(scenario="Code=1")%>%
  bind_rows(., p_test)%>%
  spread(scenario, dead)%>%
  mutate(DA = baseline - `Code=1`)

ggplot(p_test2, aes(x=year, y=DA, color=cause))+
  geom_point()+
  facet_wrap(~sex)

###########################
#Run for tobacco
###########################

ints<-unique(interv.df$Code)

out.df<-state.transition(b_rates, pop0, "baseline")%>%mutate(Code=0)

for(i in ints){
  
  out.df<-bind_rows(out.df, state.transition(b_rates, pop0, i)%>%mutate(Code=i))
  
}

df_out<-out.df%>%group_by(year, Code)%>%
  summarise(dead = sum(dead),
            newcases = sum(newcases))

baseline<-df_out%>%filter(Code==0)

names<-read.csv("data/int_names.csv", stringsAsFactors = F)%>%
  bind_rows(., data.frame(Code=1, Intervention="All smoking interventions"),
            data.frame(Code=5, Intervention = "All HTN interventions"),
            data.frame(Code=7, Intervention = "All DM interventions"))

###Tobacco interventions###

DA<-left_join(df_out%>%filter(Code>=1 & Code<=2), names)%>%
  select(-newcases)%>%
  left_join(., baseline%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
  mutate(Deaths.Averted = base.dead - dead)%>%
  mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
  group_by(Code)%>%
  arrange(year)%>%
  mutate(Cumulative.DA = signif(cumsum(Deaths.Averted),3))%>%
  arrange(Code)

CA<-left_join(df_out%>%filter(Code!=0), names)%>%
  select(-dead)%>%
  left_join(., baseline%>%select(-dead, -Code)%>%rename(base.case=newcases))%>%
  mutate(Cases.Averted = base.case - newcases)%>%
  mutate(Cases.Averted = ifelse(Cases.Averted<0, 0 , signif(Cases.Averted,3)))%>%
  group_by(Code)%>%
  arrange(year)%>%
  mutate(Cumulative.CA = signif(cumsum(Cases.Averted),3))%>%
  arrange(Code)

tob.out2<-left_join(DA, CA)%>%select(-dead, -base.dead, -base.case, -newcases)%>%
  filter(year>=2023)

write.csv(tob.out2%>%
            group_by(Code, Intervention)%>%
            summarise(DA = sum(Deaths.Averted),
                      CA = sum(Cases.Averted)), "outputs/tobacco_results2.csv")


#### HTN and DM interventions ####

DA<-left_join(df_out%>%filter(Code>2), names)%>%
  select(-newcases)%>%
  left_join(., baseline%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
  mutate(Deaths.Averted = base.dead - dead)%>%
  mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
  group_by(Code)%>%
  arrange(year)%>%
  mutate(Cumulative.DA = signif(cumsum(Deaths.Averted),3))%>%
  arrange(Code)

CA<-left_join(df_out%>%filter(Code>2), names)%>%
  select(-dead)%>%
  left_join(., baseline%>%select(-dead, -Code)%>%rename(base.case=newcases))%>%
  mutate(Cases.Averted = base.case - newcases)%>%
  mutate(Cases.Averted = ifelse(Cases.Averted<0, 0 , signif(Cases.Averted,3)))%>%
  group_by(Code)%>%
  arrange(year)%>%
  mutate(Cumulative.CA = signif(cumsum(Cases.Averted),3))%>%
  arrange(Code)

htn.out2<-left_join(DA, CA)%>%select(-dead, -base.dead, -base.case, -newcases)%>%
  filter(year>=2023)

write.csv(htn.out2%>%
            group_by(Code, Intervention)%>%
            summarise(DA = sum(Deaths.Averted),
                      CA = sum(Cases.Averted)), "outputs/htn_results2.csv")



#### Plots ####
library("viridis") 

ggplot(tob.out2%>%filter(Code>1), aes(x=year, y=Cumulative.DA, 
                                      fill=str_wrap(Intervention,60)))+
  geom_area()+
  theme_bw()+
  xlab("Year")+
  ylab("Cumulative deaths averted")+
  labs(fill="Intervention")+
  scale_fill_viridis(discrete = TRUE)

ggsave("outputs/tobacco_deaths.jpeg", width=9, height=4)


library(RColorBrewer)

htn.plot<-htn.out2%>%select(-Intervention, -Cumulative.CA, -Cases.Averted, -Deaths.Averted)%>%
  spread(Code, Cumulative.DA)%>%
  mutate(`5` = `5` - `5.2`,
         `7` = `7` - `7.2`,
         `5.2` = `5.2` - `5.1`,
         `7.2` = `7.2` - `7.1`)%>%
  select(-`5.3`, -`7.3`)%>%
  gather(Code, Cumulative.DA, -year)%>%
  mutate(Code = as.numeric(Code))%>%
  left_join(., names)%>%
  mutate(Intervention = ifelse(Code==5, "Enhance hypertension intervention uptake and adherence", Intervention),
         Intervention = ifelse(Code==7, "Enhance diabetes intervention uptake and adherence", Intervention),
         Intervention = ifelse(Code==5.2, "Make early hypertension detection available in non clinic settings", Intervention),
         Intervention = ifelse(Code==7.2, "Make early diabetes detection available in non clinic settings", Intervention)
  )%>%
  mutate(Code = ifelse(Code==5, 5.4, Code),
         Code = ifelse(Code==7, 7.4, Code))%>%
  mutate(group = ifelse(Code>=7, "Diabetes", "Hypertension"))


#htn.plot$Intervention<-factor(htn.plot$Intervention, levels=levels(htn.plot$Code))
  
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(htn.plot, aes(x=year, y=Cumulative.DA, fill=str_wrap(Intervention,40)))+
  geom_area()+
  theme_bw()+
  xlab("Year")+
  ylab("Cumulative deaths averted")+
  facet_wrap(~group)+
  scale_fill_manual(values=cbPalette[c(5,3,2,6,7,4)])+
  labs(fill="Intervention")

ggsave("outputs/htn_dm_deaths.jpeg", width=9, height=4)

