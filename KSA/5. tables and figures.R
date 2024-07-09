rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)

##########################################################################
#########################################################################

df<-read.csv("all_results.csv", stringsAsFactors = F)

age_match<-data.frame(age=0:95,
                      age.group = c(rep("<5 years",5), 
                        rep("5-9 years",5), 
                        rep("10-14 years",5),
                        rep("15-19 years",5),
                        rep("20-24 years",5),
                      rep("25-29 years",5), 
                      rep("30-34 years",5),
                      rep("35-39 years",5),
                      rep("40-44 years",5),
                      rep("45-49 years",5),
                      rep("50-54 years",5),
                      rep("55-59 years",5),
                      rep("60-64 years",5),
                      rep("65-69 years",5),
                      rep("70-74 years",5),
                      rep("75-79 years",5),
                      rep("80-84",5),
                      rep("85-89",5),
                      rep("90-94",5),
                      rep("95+ years",1))
)

age_match2<-data.frame(age=0:99,
                      age_group_name = c(rep("<1 year",1), 
                                    rep("1 to 4",4),
                                    rep("5 to 9",5),
                                    rep("10 to 14",5),
                                    rep("15 to 19",5),
                                    rep("20 to 24",5),
                                    rep("25 to 29",5), 
                                    rep("30 to 34",5),
                                    rep("35 to 39",5),
                                    rep("40 to 44",5),
                                    rep("45 to 49",5),
                                    rep("50 to 54",5),
                                    rep("55 to 59",5),
                                    rep("60 to 64",5),
                                    rep("65 to 69",5),
                                    rep("70 to 74",5),
                                    rep("75 to 79",5),
                                    rep("80 to 84",5),
                                    rep("85 to 89",5),
                                    rep("90 to 94",5),
                                    rep("95 to 99",5))
)

ratios<-read.csv("data/IHME-GBD_2019_DATA-b3b87b2a-1.csv", stringsAsFactors = F)%>%
  select(-upper, -lower, -metric, -year, -location)%>%
  rename(age.group = age)%>%
  left_join(., age_match)%>%
  select(-age.group)%>%
  spread(measure, val)%>%
  mutate(yldtoyll = `YLDs (Years Lived with Disability)`/`YLLs (Years of Life Lost)`,
         dis.weight = `YLDs (Years Lived with Disability)`/Prevalence)%>%
  mutate(yldtoyll = ifelse(is.na(yldtoyll), 0, yldtoyll),
         dis.weight = ifelse(is.na(dis.weight), 0 , dis.weight))%>%
  select(- `YLDs (Years Lived with Disability)`,
         - `YLLs (Years of Life Lost)`,
         - Prevalence)
  

LE<-read.csv("data/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_5_WSHOCK_Y2020M11D13.csv")%>%
  filter(location_name %in% c("Saudi Arabia", "Global"),
         year_id==2019,
         sex_name !="both",
         measure_name == "Life expectancy")%>%
  select(sex_name, location_name, age_group_name, val)


for (i in c(6:20,28,30:33)){
  LE<-bind_rows(LE,
                read.csv(paste0("data/IHME_GBD_2019_LIFE_TABLES_1950_2019_ID_",i,"_WSHOCK_Y2020M11D13.csv")
  )%>%
    filter(location_name %in% c("Saudi Arabia", "Global"),
           year_id==2019,
           sex_name !="both",
           measure_name == "Life expectancy")%>%
    select(sex_name, location_name, age_group_name, val)
  )
}

LE<-LE%>%
  rename(sex = sex_name)%>%
  mutate(sex = ifelse(sex=="male", "Male", "Female"))%>%
  left_join(., age_match2)%>%
  rename(exp = val)

#######################

#Deaths averted
DA<-df%>%filter(year>=2023, year<=2040, age>=25)%>%
  group_by(scenario)%>%
  summarise(deaths = sum(dead))%>%
  spread(scenario, deaths)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Deaths.averted)

# DALYS
YLLs<-left_join(df, LE%>%filter(location_name=="Global"))%>%
  mutate(yll = dead*exp)

YLDs<-left_join(df, ratios)%>%
  mutate(yld = sick*dis.weight)

DALYS<-left_join(YLLs, YLDs)%>%
  filter(year>=2023, year<=2040, age>=25)%>%
  mutate(daly = yll + yld)%>%
  group_by(scenario)%>%
  summarise(dalys = sum(daly))%>%
  spread(scenario, dalys)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Dalys.averted)

# All results #
tab1<-left_join(DA, DALYS)%>%
  mutate(GDP.percapita = 20110.32,
         Econ.benefit = GDP.percapita*2.3*Dalys.averted)

write.csv(tab1, "Table1.csv", row.names = F)

# By sex #
DA<-df%>%filter(year>=2023, year<=2040, age>=25)%>%
  group_by(scenario, sex)%>%
  summarise(deaths = sum(dead))%>%
  spread(scenario, deaths)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Deaths.averted, -sex)

DALYS<-left_join(YLLs, YLDs)%>%
  filter(year>=2023, year<=2040, age>=25)%>%
  mutate(daly = yll + yld)%>%
  group_by(scenario, sex)%>%
  summarise(dalys = sum(daly))%>%
  spread(scenario, dalys)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Dalys.averted, -sex)


tab2<-left_join(DA, DALYS)%>%
  mutate(GDP.percapita = 20110.32,
         Econ.benefit = GDP.percapita*2.3*Dalys.averted)

write.csv(tab2, "Table1_bysex.csv", row.names = F)

#by age#
DA<-df%>%filter(year>=2023, year<=2040, age>=25)%>%
  mutate(age.group = ifelse(age<40, "Under 40", 
                            ifelse(age>=40 & age<70, "40-69", "70+")))%>%
  group_by(scenario, age.group)%>%
  summarise(deaths = sum(dead))%>%
  spread(scenario, deaths)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Deaths.averted, -age.group)

DALYS<-left_join(YLLs, YLDs)%>%
  filter(year>=2023, year<=2040, age>=25)%>%
  mutate(age.group = ifelse(age<40, "Under 40", 
                            ifelse(age>=40 & age<70, "40-69", "70+")))%>%
  mutate(daly = yll + yld)%>%
  group_by(scenario, age.group)%>%
  summarise(dalys = sum(daly))%>%
  spread(scenario, dalys)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Dalys.averted, -age.group)


tab3<-left_join(DA, DALYS)%>%
  mutate(GDP.percapita = 20110.32,
         Econ.benefit = GDP.percapita*2.3*Dalys.averted)

write.csv(tab3, "Table1_byage.csv", row.names = F)


#IHD in males benefit
left_join(YLLs, YLDs)%>%
  filter(year>=2023, year<=2040, age>=25, sex=="Male", cause=="Ischemic heart disease")%>%
  mutate(age.group = ifelse(age<40, "Under 40", 
                            ifelse(age>=40 & age<70, "40-69", "70+")))%>%
  mutate(daly = yll + yld)%>%
  group_by(scenario)%>%
  summarise(dalys = sum(daly))%>%
  spread(scenario, dalys)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(Scenario, Dalys.averted)%>%
  mutate(benefit = 20110.32*2.3*Dalys.averted/1e9)

###



#plots
plot1<-df%>%filter(year>=2023, year<=2040, age>=25)%>%
  group_by(year, scenario)%>%
  summarise(dead=sum(dead))%>%
  spread(scenario, dead)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(scenario, death.averted, -year)

library(scales)

ggplot(plot1, aes(x=year, y=death.averted, color=scenario))+
  geom_line()+
  theme_bw()+
  ylab("Deaths averted")+
  xlab("Year")+
  labs(color = "Scenario")+
  scale_y_continuous(label = comma)

ggsave("deaths_averted.tiff", width = 8, height=5, dpi=300)
ggsave("deaths_averted.eps", width = 8, height=5, dpi=300)
ggsave("deaths_averted.jpeg", width = 8, height=5, dpi=300)
ggsave("deaths_averted.pdf", width=8, height = 5, dpi=300)

plot2<-df%>%filter(year>=2023, year<=2040, age>=25)%>%
  group_by(year, scenario, cause)%>%
  summarise(dead=sum(dead))%>%
  spread(scenario, dead)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(scenario, death.averted, -year, -cause)

ggplot(plot2, aes(x=year, y=death.averted, color=scenario))+
  geom_line()+
  theme_bw()+
  ylab("Deaths averted")+
  xlab("Year")+
  labs(color = "Scenario")+
  facet_wrap(~cause)+
  scale_y_continuous(label = comma, limits=c(0,10000))

ggsave("deaths_averted_bycause.tiff", width = 8, height=5, dpi=300)
ggsave("deaths_averted_bycause.eps", width = 8, height=5, dpi=300)
ggsave("deaths_averted_bycause.jpeg", width = 8, height=5, dpi=300)
ggsave("deaths_averted_bycause.pdf", width = 8, height=5, dpi=300)

plot3<-left_join(YLLs, YLDs)%>%
  filter(year>=2023, year<=2040, age>=25)%>%
  mutate(daly = yll + yld)%>%
  group_by(scenario, year)%>%
  summarise(dalys = sum(daly))%>%
  spread(scenario, dalys)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(scenario, daly.averted, -year)

ggplot(plot3, aes(x=year, y=daly.averted, color=scenario))+
  geom_line()+
  theme_bw()+
  ylab("DALYs averted")+
  xlab("Year")+
  labs(color = "Scenario")+
  scale_y_continuous(label = comma, limits=c(0,3e5))

ggsave("dalys_averted.tiff", width = 8, height=5, dpi=300)
ggsave("dalys_averted.eps", width = 8, height=5, dpi=300)
ggsave("dalys_averted.jpeg", width = 8, height=5, dpi=300)
ggsave("dalys_averted.pdf", width = 8, height=5, dpi=300)


plot4<-left_join(YLLs, YLDs)%>%
  filter(year>=2023, year<=2040, age>=25)%>%
  mutate(daly = yll + yld)%>%
  group_by(scenario, year, cause)%>%
  summarise(dalys = sum(daly))%>%
  spread(scenario, dalys)%>%
  mutate(Ideal = Baseline - Ideal,
         Intervention = Baseline - Intervention)%>%
  select(-Baseline)%>%
  gather(scenario, daly.averted, -year, -cause)

ggplot(plot4, aes(x=year, y=daly.averted, color=scenario))+
  geom_line()+
  theme_bw()+
  ylab("DALYs averted")+
  xlab("Year")+
  labs(color = "Scenario")+
  facet_wrap(~cause)+
  scale_y_continuous(label = comma, limits=c(0,2e5))

ggsave("dalys_averted_bycause.eps", width = 8, height=5, dpi=300)
ggsave("dalys_averted_bycause.tiff", width = 8, height=5, dpi=300)
ggsave("dalys_averted_bycause.jpeg", width = 8, height=5, dpi=300)
ggsave("dalys_averted_bycause.pdf", width = 8, height=5, dpi=300)


#Ischemic heart disease
plot2%>%group_by(cause, scenario)%>%summarise(deaths.averted = sum(death.averted))
plot2%>%group_by(scenario)%>%summarise(deaths.averted = sum(death.averted))

87007/113133
62641/80421

plot4%>%group_by(cause, scenario)%>%summarise(daly.averted = sum(daly.averted))
plot4%>%group_by(scenario)%>%summarise(daly.averted = sum(daly.averted))

2076857/2856174
1492738/1999191

#cumulative deaths and dalys averted by cause

plot5<-plot2%>%
  group_by(cause, scenario)%>%
  mutate(cum.da = cumsum(death.averted))

ggplot(plot5, aes(x=year, y=cum.da, fill=cause))+
  geom_area()+
  facet_wrap(~scenario)+
  ylab("Cumulative deaths averted")+
  xlab("Year")+
  labs(fill = "Cause")+
  theme_bw()+
  scale_y_continuous(label = comma, limits = c(0,120000))

ggsave("cumulative_deaths_averted.eps", height = 5, width = 8, dpi=300)
ggsave("cumulative_deaths_averted.tiff", height = 5, width = 8, dpi=300)
ggsave("cumulative_deaths_averted.jpeg", height = 5, width = 8, dpi=300)
ggsave("cumulative_deaths_averted.pdf", height = 5, width = 8, dpi=300)


plot6<-plot4%>%
  group_by(cause, scenario)%>%
  mutate(cum.da = cumsum(daly.averted))

ggplot(plot6, aes(x=year, y=cum.da, fill=cause))+
  geom_area()+
  facet_wrap(~scenario)+
  ylab("Cumulative DALYs averted")+
  xlab("Year")+
  labs(fill = "Cause")+
  theme_bw()+
  scale_y_continuous(label = comma, limits=c(0,3e6))

ggsave("cumulative_dalys_averted.tiff", height = 5, width = 8, dpi=300)
ggsave("cumulative_dalys_averted.eps", height = 5, width = 8, dpi=300)
ggsave("cumulative_dalys_averted.jpeg", height = 5, width = 8, dpi=300)
ggsave("cumulative_dalys_averted.pdf", height = 5, width = 8, dpi=300)





