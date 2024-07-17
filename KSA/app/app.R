#
library(shiny)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(zoo)
library(RColorBrewer)
library(shinycssloaders)
library(viridis)


names<-read.csv("int_names.csv", stringsAsFactors = F)%>%
  bind_rows(., data.frame(Code=1, Intervention="All smoking interventions"),
            data.frame(Code=5, Intervention = "All HTN interventions"),
            data.frame(Code=7, Intervention = "All DM interventions"))

ints<-read.csv("int_names.csv", stringsAsFactors = F)
tob<-read.csv("tob.csv", stringsAsFactors = F)
tob_RR<-read.csv("tobacco_RRs.csv", stringsAsFactors = F)%>%
  rename(sex=Sex)

tob_eff<-read.csv("tob_effs.csv", stringsAsFactors = F)%>%
  mutate(age_start = 10,
         age_stop = ifelse(Age=="10 to 25", 25, 65))%>%
  select(-Age, -Source)%>%
  spread(Target, RRR)%>%
  mutate(cessation = 1-(1-ifelse(is.na(cessation), 0, cessation))*(1-ifelse(is.na(both), 0, both)),
         initiation = 1-(1-ifelse(is.na(initiation), 0, initiation))*(1-ifelse(is.na(both), 0, both))
  )%>%
  select(-both)

cascade<-read.csv("htn_dm_cascade.csv", stringsAsFactors = F)%>%
  select(year, sex, HTN, Aware, Treated, Control, DM, connection.to.care, quality.of.care)

b_rates<-data.table(read.csv("base_rates.csv", stringsAsFactors = F))
pop0<-data.table(read.csv("pop.csv", stringsAsFactors = F))
ints2<-read.csv("interventions.csv", stringsAsFactors = F)

results<-read.csv("results.csv", stringsAsFactors = F)
costs<-read.csv("cost_template.csv", stringsAsFactors = F)%>%select(Code, Total)

DA_tot<-left_join(results%>%filter(Code==1), names)%>%
  select(-newcases)%>%
  left_join(., results%>%filter(Code==0)%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
  mutate(Deaths.Averted = base.dead - dead)%>%
  mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
  pull(Deaths.Averted)

DA_tot2<-left_join(results%>%filter(Code>1 & Code<=2), names)%>%
  select(-newcases)%>%
  left_join(., results%>%filter(Code==0)%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
  mutate(Deaths.Averted = base.dead - dead)%>%
  pull(Deaths.Averted)


ratio<-sum(DA_tot)/sum(DA_tot2)


###############
#FXNS
#################

'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

#run model
tob.model<-function(b_rates_tob, tob_eff_in, year1, n, code){
  
  
  if(code=="baseline"){
    
    b_df<-b_rates_tob
    
  }
  
  else{
    
    age_df<-data.frame(agetemp=10:65)
    
    int.df<-tob_eff_in%>%filter(Code==code)%>%
      merge(., age_df)%>%
      filter(agetemp>=age_start & agetemp<=age_stop)%>%
      rename(age=agetemp)%>%
      select(cessation, initiation, age)
    
    b_df<-b_rates_tob%>%left_join(., int.df)%>%
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


###

state.transition<-function(b_rates, pop0, code, interv.df){ 
  
  base_rates<-merge(b_rates, pop0[year<=2030], by=c("year", "location", "sex", "age"), all=TRUE)%>%
    arrange(year, age)%>%
    filter(year>=2019)
  
  cse<-unique(b_rates$cause)
  
  
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



#####


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NCD Master Plan"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          h4("Select and customize interventions"),
            checkboxGroupInput("tob_sel", "Tobacco:", ints$Intervention[ints$Code<2],
                               selected = ints$Intervention[ints$Code<2]),
            h5("Tobacco customizations:"),
            sliderInput("tax", "Additional specific excise tax (SAR):", min=0,max=100, value=38.5, step=0.5),
            textOutput("pack_price"),
            textOutput("percent_tax"),
            br(),
          
            checkboxGroupInput("htn_sel", "Hypertension:", ints$Intervention[ints$Code%in%c(5.1,5.2,5.3)],
                               selected = ints$Intervention[ints$Code%in%c(5.1,5.2,5.3)]),
            sliderInput("screen1", "Screening coverage target in clinical settings(%):", 0,75,75),
            uiOutput("screen2"),
            sliderInput("treat1", "Connection to care (%):", 80, 100, 95),
            sliderInput("treat2", "Quality of care (%):", 55, 100, 75),
            checkboxGroupInput("dm_sel", "Diabetes:", ints$Intervention[ints$Code%in%c(7.1,7.2,7.3)],
                               selected = ints$Intervention[ints$Code%in%c(7.1,7.2,7.3)]),
          sliderInput("dm_screen1", "Screening coverage target in clinical settings(%):", 0,75,75),
          uiOutput("dm_screen2"),
          sliderInput("dm_treat1", "Connection to care (%):", 80, 100, 95),
          sliderInput("dm_treat2", "Quality of care (%):", 55, 100, 75)
          ),

        # Show a plot of the generated distribution
        mainPanel(
          h4("To run the cost effectiveness analysis, choose your interventions and customize parameters in the
             menu on the left. Then upload your costing data and press the 'Run' button to get your custom results.
             Results may take a minute to load.
             You can download the template for the costing data required below. 
             If no data are uploaded, default assumptions are used."), br(),
          h4("Code for the model can be found: https://github.com/Disease-Control-Priorities/NCD-MP"),br(),
          
          downloadButton("cost_template", "Download cost template"),br(), 
          fileInput("upload", "Upload custom cost data", accept=".csv", multiple = T),
          actionButton("run", "Run analysis", class="btn-warning"), br(),br(),
          
          withSpinner(plotOutput("tob_plot"), 1),
          withSpinner(plotOutput("htn_plot"), 1),
          tableOutput("results_tab")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$cost_template <- downloadHandler(
    filename = function() {
      "cost_template.csv"
    },
    content = function(file) {
      write.csv(read.csv("cost_template.csv", stringsAsFactors = F), file)
    }
  )
  
  
  
    output$pack_price<-renderText({
      
      
      paste0("Total pack price: ", input$tax+28, " SAR")
      
    })
    
    output$percent_tax<-renderText({
      
      paste0("Percent of pack price from excise tax: ", 100*round(input$tax/(input$tax+28), 2), " %")
      
      
    })
    
    output$screen2<-renderUI({
      
      sliderInput("screen2", "Screening coverage extended to non-clinic settings (%):", min=input$screen1, max=100, value=95)
      
    })
    
    output$dm_screen2<-renderUI({
      
      sliderInput("dm_screen2", "Screening coverage extended to non-clinic settings (%):", min=input$dm_screen1, max=100, value=95)
      
    })
    
    
    v<-reactiveValues(tob_data = results%>%filter(Code<2 & Code>0),
                      htn_data = results%>%filter(Code>2),
                      baseline = results%>%filter(Code==0),
                      htn_codes = c(5.1, 5.2, 5.3, 7.1, 7.2, 7.3),
                      costs = costs,
                      ratio = ratio)
    
    observeEvent( input$run, {
      
      codes<-ints%>%filter(Intervention %in% input$tob_sel)%>%pull(Code)
      v$tob_data<-results%>%filter(Code %in% codes)
      
      v$htn_codes<-c(ints%>%filter(Intervention %in% input$htn_sel)%>%pull(Code),
               ints%>%filter(Intervention %in% input$dm_sel)%>%pull(Code)) 
      
      if(!is.null(input$upload)){
      v$costs<-read.csv(input$upload$datapath, header=T, stringsAsFactors = F)%>%select(Code, Total)
      }
      
      else{}
      
      if(input$tax!=38.5 & 1.7 %in% codes){
        
        imp<-0.48*input$tax/28
        imp<-min(0.95, imp)
        #codes<-c(1.1,1.2,1.3,1.4,1.7)
        
        tob_eff_new<-tob_eff%>%
          mutate(cessation = ifelse(Code==1.7, imp, cessation),
                 initiation = ifelse(Code==1.7, imp, initiation))%>%
          filter(Code!=1)
        
        all_eff<-tob_eff_new%>%
          filter(Code %in% codes)%>%
          filter(age_stop==25)%>%
          filter(Code %in% codes)%>%
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
        
        tob_eff_new<-bind_rows(tob_eff_new, all_eff)
        
        
        impact.tob<-tob.model(tob, tob_eff_new, 2020, 10, "baseline")%>%
          select(-init, -cess, -check)%>%
          rename(Nonsmoker = Never, Smoker = Current, Exsmoker=Ex, sex=Sex)%>%
          gather(Category, pi, -sex, -age, -year)%>%
          left_join(., tob_RR)%>%
          group_by(sex, age, year, Cause)%>%
          summarise(alpha =sum(pi*RR))%>%
          left_join(., tob_RR)
        
        tob_df<-tob.model(tob, tob_eff_new, 2020, 10, "baseline")%>%mutate(Code=0)%>%
          bind_rows(., tob.model(tob, tob_eff_new, 2020, 10, 1.7)%>%mutate(Code=1.7))%>%
          bind_rows(., tob.model(tob, tob_eff_new, 2020, 10, 1.0)%>%mutate(Code=1.0))
        
        pi_star<-tob_df%>%
          select(age, year, sex=Sex, Nonsmoker = Never, Smoker = Current, Exsmoker = Ex, Code)%>%
          gather(Category, pi2, -age, -year, -sex, -Code)
        
        impact.tob<-left_join(impact.tob, pi_star)%>%
          mutate(impact = (RR/alpha)*pi2)%>%
          group_by(Code, Cause, age, sex, year)%>%
          summarise(impact = sum(impact))%>%
          filter(Code>0)%>%
          mutate(target="IR")
        
        impact.tob<-bind_rows(impact.tob, impact.tob%>%mutate(target="CF"))
        
        #extend for 0-24 and 66-95
        add<-impact.tob%>%filter(age==65)

        #this is janky
        for(i in 0:9){
          impact.tob<-bind_rows(impact.tob, add%>%mutate(age=i, impact=1))
        }
        
        for(i in 66:95){
          
          impact.tob<-bind_rows(impact.tob, add%>%mutate(age=i))
        }
        
        interv.df<-impact.tob%>%rename(cause = Cause)%>%
          filter(target=="IR")%>%
          rename(IR_eff=impact)%>%
          select(-target)%>%
          full_join(., impact.tob%>%rename(cause = Cause)%>%
                      filter(target=="CF")%>%
                      rename(CF_eff=impact)%>%
                      select(-target))%>%
          mutate(IR_eff = ifelse(is.na(IR_eff), 1, IR_eff),
                 CF_eff = ifelse(is.na(CF_eff), 1, CF_eff))
        
      #any(is.na(interv.df))   
        out.df<-data.frame()
        
        sels<-unique(interv.df$Code)
        
        for(i in sels){
          
          out.df<-bind_rows(out.df, state.transition(b_rates, pop0, i, interv.df)%>%mutate(Code=i))
          
        }
        
        update<-out.df%>%group_by(year, Code)%>%
          summarise(dead = sum(dead),
                    newcases = sum(newcases))
        
        v$tob_data<-v$tob_data%>%filter(Code %!in% sels)%>%
          bind_rows(., update)
        
        #DA_tot<-left_join(v$tob_data%>%filter(Code==1), names)%>%
        #  select(-newcases)%>%
        #  left_join(., results%>%filter(Code==0)%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
        #  mutate(Deaths.Averted = base.dead - dead)%>%
        #  mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
        #  pull(Deaths.Averted)
        
        #DA_tot2<-left_join(v$tob_data%>%filter(Code>1 & Code<=2), names)%>%
          #select(-newcases)%>%
          #left_join(., results%>%filter(Code==0)%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
          #mutate(Deaths.Averted = base.dead - dead)%>%
          #pull(Deaths.Averted)
        
        
        #v$ratio<-sum(DA_tot)/sum(DA_tot2)
        
      }
      
      else{
        
        all_eff<-tob_eff%>%
          filter(age_stop==25)%>%
          filter(Code %in% codes)%>%
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
        
        #update tobacco numbers to reflect non-additive sum
        impact.tob<-tob.model(tob, all_eff, 2020, 10, "baseline")%>%
          select(-init, -cess, -check)%>%
          rename(Nonsmoker = Never, Smoker = Current, Exsmoker=Ex, sex=Sex)%>%
          gather(Category, pi, -sex, -age, -year)%>%
          left_join(., tob_RR)%>%
          group_by(sex, age, year, Cause)%>%
          summarise(alpha =sum(pi*RR))%>%
          left_join(., tob_RR)
        
        tob_df<-tob.model(tob, all_eff, 2020, 10, 1.0)%>%mutate(Code=1.0)
        
        pi_star<-tob_df%>%
          select(age, year, sex=Sex, Nonsmoker = Never, Smoker = Current, Exsmoker = Ex, Code)%>%
          gather(Category, pi2, -age, -year, -sex, -Code)
        
        impact.tob<-left_join(impact.tob, pi_star)%>%
          mutate(impact = (RR/alpha)*pi2)%>%
          group_by(Code, Cause, age, sex, year)%>%
          summarise(impact = sum(impact))%>%
          filter(Code>0)%>%
          mutate(target="IR")
        
        impact.tob<-bind_rows(impact.tob, impact.tob%>%mutate(target="CF"))
        
        #extend for 0-24 and 66-95
        add<-impact.tob%>%filter(age==65)
        
        #this is janky
        for(i in 0:9){
          impact.tob<-bind_rows(impact.tob, add%>%mutate(age=i, impact=1))
        }
        
        for(i in 66:95){
          
          impact.tob<-bind_rows(impact.tob, add%>%mutate(age=i))
        }
        
        interv.df<-impact.tob%>%rename(cause = Cause)%>%
          filter(target=="IR")%>%
          rename(IR_eff=impact)%>%
          select(-target)%>%
          full_join(., impact.tob%>%rename(cause = Cause)%>%
                      filter(target=="CF")%>%
                      rename(CF_eff=impact)%>%
                      select(-target))%>%
          mutate(IR_eff = ifelse(is.na(IR_eff), 1, IR_eff),
                 CF_eff = ifelse(is.na(CF_eff), 1, CF_eff))
        
        #any(is.na(interv.df))   
        out.df<-data.frame()
        
        sels<-unique(interv.df$Code)
        
        for(i in sels){
          
          out.df<-bind_rows(out.df, state.transition(b_rates, pop0, i, interv.df)%>%mutate(Code=i))
          
        }
        
        update<-out.df%>%group_by(year, Code)%>%
          summarise(dead = sum(dead),
                    newcases = sum(newcases))
        
        v$tob_data<-v$tob_data%>%filter(Code %!in% sels)%>%
          bind_rows(., update)
        
        #DA_tot<-left_join(v$tob_data%>%filter(Code==1), names)%>%
        #  select(-newcases)%>%
        #  left_join(., results%>%filter(Code==0)%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
        #  mutate(Deaths.Averted = base.dead - dead)%>%
        #  mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
        #  pull(Deaths.Averted)
        
        #DA_tot2<-left_join(v$tob_data%>%filter(Code>1 & Code<=2), names)%>%
        #  select(-newcases)%>%
        #  left_join(., results%>%filter(Code==0)%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
        #  mutate(Deaths.Averted = base.dead - dead)%>%
        #  pull(Deaths.Averted)
        
        
        #v$ratio<-sum(DA_tot)/sum(DA_tot2)
        
      }
      
      
      #htn/dm
      
      if(input$screen1 != 75 | input$screen2 != 95 | input$treat1 !=95 | input$treat2 !=75 |input$dm_screen1 != 75 | input$dm_screen2 != 95 | input$dm_treat1 !=95 | input$dm_treat2 !=75){
        
        test<-ints2%>%filter(Intervention!="Second-hand smoke reduction")%>%
          select(-source)%>%spread(Intervention, RR)
        
        screen1<-cascade%>%filter(year==2023)%>%
          select(-year)%>%
          merge(., data.frame(year=2023:2030))%>%
          mutate(Aware = ifelse(year==2030, input$screen1/100,
                                ifelse(year<=2023, Aware, NA)))%>%
          group_by(sex)%>%
          mutate(Aware = na.approx(Aware),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)%>%
          bind_rows(., cascade%>%filter(year<2023))
        
        screen2<-cascade%>%filter(year==2023)%>%
          select(-year)%>%
          merge(., data.frame(year=2023:2030))%>%
          mutate(Aware = ifelse(year==2030, input$screen2/100,
                                ifelse(year<=2023, Aware, NA)))%>%
          group_by(sex)%>%
          mutate(Aware = na.approx(Aware),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)%>%
          bind_rows(., cascade%>%filter(year<2023))
        
        treat1<-cascade%>%filter(year==2023)%>%
          select(-year)%>%
          merge(., data.frame(year=2023:2030))%>%
          mutate(connection.to.care = ifelse(year==2030, input$treat1/100,
                                             ifelse(year<=2023, connection.to.care, NA)),
                 quality.of.care = ifelse(year==2030, input$treat2/100,
                                          ifelse(year<=2023, quality.of.care, NA)))%>%
          group_by(sex)%>%
          mutate(connection.to.care = na.approx(connection.to.care),
                 quality.of.care = na.approx(quality.of.care),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)%>%
          bind_rows(., cascade%>%filter(year<2023))
        
        treat2<-screen2%>%
          mutate(connection.to.care = ifelse(year==2030, input$treat1/100,
                                             ifelse(year<=2023, connection.to.care, NA)),
                 quality.of.care = ifelse(year==2030, input$treat2/100,
                                          ifelse(year<=2023, quality.of.care, NA)))%>%
          group_by(sex)%>%
          mutate(connection.to.care = na.approx(connection.to.care),
                 quality.of.care = na.approx(quality.of.care),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)
        
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
        
        dm_screen1<-cascade%>%filter(year==2023)%>%
          select(-year)%>%
          merge(., data.frame(year=2023:2030))%>%
          mutate(Aware = ifelse(year==2030, input$dm_screen1/100,
                                ifelse(year<=2023, Aware, NA)))%>%
          group_by(sex)%>%
          mutate(Aware = na.approx(Aware),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)%>%
          bind_rows(., cascade%>%filter(year<2023))
        
        dm_screen2<-cascade%>%filter(year==2023)%>%
          select(-year)%>%
          merge(., data.frame(year=2023:2030))%>%
          mutate(Aware = ifelse(year==2030, input$dm_screen2/100,
                                ifelse(year<=2023, Aware, NA)))%>%
          group_by(sex)%>%
          mutate(Aware = na.approx(Aware),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)%>%
          bind_rows(., cascade%>%filter(year<2023))
        
        dm_treat1<-cascade%>%filter(year==2023)%>%
          select(-year)%>%
          merge(., data.frame(year=2023:2030))%>%
          mutate(connection.to.care = ifelse(year==2030, input$dm_treat1/100,
                                             ifelse(year<=2023, connection.to.care, NA)),
                 quality.of.care = ifelse(year==2030, input$dm_treat2/100,
                                          ifelse(year<=2023, quality.of.care, NA)))%>%
          group_by(sex)%>%
          mutate(connection.to.care = na.approx(connection.to.care),
                 quality.of.care = na.approx(quality.of.care),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)%>%
          bind_rows(., cascade%>%filter(year<2023))
        
        dm_treat2<-dm_screen2%>%
          mutate(connection.to.care = ifelse(year==2030, input$dm_treat1/100,
                                             ifelse(year<=2023, connection.to.care, NA)),
                 quality.of.care = ifelse(year==2030, input$dm_treat2/100,
                                          ifelse(year<=2023, quality.of.care, NA)))%>%
          group_by(sex)%>%
          mutate(connection.to.care = na.approx(connection.to.care),
                 quality.of.care = na.approx(quality.of.care),
                 Treated = Aware * connection.to.care,
                 Control = Treated * quality.of.care)
        
        impact.dm<-bind_rows(dm_screen1%>%mutate(Code=7.1), dm_screen2%>%mutate(Code=7.2),
                             dm_treat1%>%mutate(Code=7.3), dm_treat2%>%mutate(Code = 7))%>%
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
        
        ##all intereventions
        interv.df<-bind_rows(#impact.tob%>%rename(cause = Cause), 
          #impact.ssh%>%rename(cause = Cause)%>%
          #   mutate(impact = 1-(1-impact)),
          impact.htn, impact.dm)%>%
          filter(target=="IR")%>%
          rename(IR_eff=impact)%>%
          select(-target)%>%
          full_join(.,bind_rows(#impact.tob%>%rename(cause = Cause), 
            #impact.ssh%>%rename(cause = Cause),
            impact.htn, impact.dm)%>%
              filter(target=="CF")%>%
              rename(CF_eff=impact)%>%
              select(-target))%>%
          mutate(IR_eff = ifelse(is.na(IR_eff), 1, IR_eff),
                 CF_eff = ifelse(is.na(CF_eff), 1, CF_eff))
        
        
        ##set up data frame
        
        out.df<-data.frame()
        
        sels<-c(5,5.1,5.2,5.3,7,7.1,7.2,7.3)
        
        for(i in sels){
          
          out.df<-bind_rows(out.df, state.transition(b_rates, pop0, i, interv.df)%>%mutate(Code=i))
          
        }
        
        v$htn_data<-out.df%>%group_by(year, Code)%>%
          summarise(dead = sum(dead),
                    newcases = sum(newcases))
        
      }
      
      else{
        v$htn_data<-results%>%filter(Code>2)
      }
      
      
    })
    
    output$tob_plot<-renderPlot({

      baseline<-v$baseline%>%filter(Code==0)
      
      DA<-left_join(v$tob_data%>%filter(Code>=1 & Code<=2), names)%>%
        select(-newcases)%>%
        left_join(., baseline%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
        mutate(Deaths.Averted = base.dead - dead)%>%
        mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
        group_by(Code)%>%
        arrange(year)%>%
        mutate(Cumulative.DA = signif(cumsum(v$ratio*Deaths.Averted),3))%>%
        arrange(Code)
      
      #ratio<-sum(DA%>%filter(Code==1)%>%pull(Deaths.Averted))/sum(DA%>%filter(Code!=1)%>%pull(Deaths.Averted))
      
      ggplot(DA%>%filter(Code!=1), 
             aes(x=year, y=Cumulative.DA, fill=str_wrap(Intervention,40)))+
        geom_area()+
        theme_bw()+
        xlab("Year")+
        ylab("Cumulative deaths averted")+
        labs(fill="Intervention")+
        ggtitle("Deaths averted by tobacco policies")
      
    })
    
    output$htn_plot<-renderPlot({
      
      cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      
       
      DA<-left_join(v$htn_data, names)%>%
        select(-newcases)%>%
        left_join(., v$baseline%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
        mutate(Deaths.Averted = base.dead - dead)%>%
        mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
        group_by(Code)%>%
        arrange(year)%>%
        mutate(Cumulative.DA = signif(cumsum(Deaths.Averted),3))%>%
        arrange(Code)
      
      htn.plot<-DA%>%select(-Intervention, -Deaths.Averted, -dead, -base.dead)%>%
        filter(Code %in% c(5,5.1,5.2,5.3,7,7.1,7.2,7.3))%>%
        spread(Code, Cumulative.DA)%>%
        mutate(`5.3` = `5` - `5.2`,
               `7.3` = `7` - `7.2`,
               `5.2` = `5.2` - `5.1`,
               `7.2` = `7.2` - `7.1`)%>%
        gather(Code, Cumulative.DA, -year)%>%
        mutate(Code = as.numeric(Code))%>%
        filter(Code %in% v$htn_codes)%>%
        left_join(., names)%>%
        mutate(Intervention = ifelse(Code==5.3, "Enhance hypertension intervention uptake and adherence", Intervention),
               Intervention = ifelse(Code==7.3, "Enhance diabetes intervention uptake and adherence", Intervention),
               Intervention = ifelse(Code==5.2, "Make early hypertension detection available in non clinic settings", Intervention),
               Intervention = ifelse(Code==7.2, "Make early diabetes detection available in non clinic settings", Intervention)
        )%>%
        mutate(group = ifelse(Code>=7, "Diabetes", "Hypertension"))
      
      legend_ord <- levels(with(htn.plot, reorder(Intervention, Code)))
      
      ggplot(htn.plot, aes(x=year, y=Cumulative.DA, fill=str_wrap(Intervention,40)))+
        geom_area()+
        theme_bw()+
        xlab("Year")+
        ylab("Cumulative deaths averted")+
        facet_wrap(~group)+
        scale_fill_manual(values=cbPalette[c(5,3,2,6,7,4)])+
        labs(fill="Intervention")+
        ggtitle("Deaths averted by hypertension and diabetes treatment")
      
    })
    
    output$results_tab<-renderTable({
      
      DA<-left_join(v$htn_data, names)%>%
        select(-newcases)%>%
        left_join(., v$baseline%>%select(-newcases, -Code)%>%rename(base.dead=dead))%>%
        mutate(Deaths.Averted = base.dead - dead)%>%
        mutate(Deaths.Averted = ifelse(Deaths.Averted<0, 0, signif(Deaths.Averted,3)))%>%
        group_by(Code)%>%
        arrange(year)%>%
        mutate(Cumulative.DA = signif(cumsum(Deaths.Averted),3))%>%
        arrange(Code)
      
      htn.plot<-DA%>%select(-Intervention, -Deaths.Averted, -dead, -base.dead)%>%
        filter(Code %in% c(5,5.1,5.2,5.3,7,7.1,7.2,7.3))%>%
        spread(Code, Cumulative.DA)%>%
        mutate(`5.3` = `5` - `5.2`,
               `7.3` = `7` - `7.2`,
               `5.2` = `5.2` - `5.1`,
               `7.2` = `7.2` - `7.1`)%>%
        gather(Code, Cumulative.DA, -year)%>%
        mutate(Code = as.numeric(Code))%>%
        filter(Code %in% v$htn_codes)%>%
        left_join(., names)%>%
        mutate(Intervention = ifelse(Code==5.3, "Enhance hypertension intervention uptake and adherence", Intervention),
               Intervention = ifelse(Code==7.3, "Enhance diabetes intervention uptake and adherence", Intervention),
               Intervention = ifelse(Code==5.2, "Make early hypertension detection available in non clinic settings", Intervention),
               Intervention = ifelse(Code==7.2, "Make early diabetes detection available in non clinic settings", Intervention)
        )%>%
        mutate(group = ifelse(Code>=7, "Diabetes", "Hypertension"))
      
      
      htn2<-htn.plot%>%filter(year==2030)%>%select(-year, -group)%>%
        rename(`Deaths averted` = Cumulative.DA)
        
      
      
      df<-left_join(v$tob_data, names)%>%
        filter(Code!=1)%>%
        left_join(., v$baseline%>%select(-newcases, -Code)%>%rename(base=dead))%>%
        group_by(Code, Intervention)%>%
        summarise(dead = sum(dead),
                  base = sum(base))%>%
        mutate(`Deaths averted` =round(v$ratio*(base - dead)))%>%
      select(-dead, -base)%>%
        bind_rows(., htn2)%>%
        left_join(., v$costs)%>%
        rename(`Total cost (SAR)` = Total)%>%
        mutate(`Cost per death averted (SAR)` = `Total cost (SAR)` / `Deaths averted`)%>%
        ungroup()%>%
        select(-Code)
        
      
      df
      
      
    })

   
}

# Run the application 
shinyApp(ui = ui, server = server)
