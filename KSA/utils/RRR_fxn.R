get.RRR <- function(riskfactors, intervention_effects, baseline_dist, RR_data){
  
  #list of selections
  sels<-c("Risk factor", "Cause", "Country", "Age", "Sex")
  # Incorporating RR data
  df <- baseline_dist %>%
    select(- Year) %>%
    left_join(RR_data, .) %>%
    group_by(`Risk factor`, Cause, Country, Age, Sex) %>%
    mutate(alpha       = sum(Proportion * RR),
           # Baseline scenario: incidence estimates of the 3 smoking status categories
           y_category  = 1 * RR / alpha,
           # Baseline scenario: weighted (average) incidence of smoking
           y_base      = sum(y_category * Proportion))
  
  #make a list 
  RRR<-new_prop<-dist_RR<-list(length(riskfactors))

  for (i in 1:length(riskfactors)){
    dist_RR[[i]]<-as.data.frame(df%>%
                                  filter(`Risk factor` == riskfactors[i])%>%
                                  select(1:6,9) %>% 
                                  spread(Category, Proportion))
    #https://github.com/tidyverse/tidyr/issues/839
    #switching to spread here since it respects the order of Category factors
    
    new_prop[[i]]<-dist_RR[[i]]%>%as.data.frame()%>%
      select(-sels)%>%
      as.matrix() %*% as.matrix(intervention_effects[[riskfactors[i]]])
    
    RRR[[i]]<-bind_cols(as.data.frame(new_prop[[i]]), as.data.frame(dist_RR[[i]])%>%select(sels))%>%
      gather(Category, Proportion_interv, -sels)%>%
      full_join(., as.data.frame(df%>%
                                   filter(`Risk factor` == riskfactors[i])%>%
                                   select(1:6,11)) 
      )%>%
      group_by(`Risk factor`, Cause, Country, Sex, Age)%>%
      mutate(RRR = sum(Proportion_interv * y_category))%>%
      select(`Risk factor`, Cause, Country, Sex, Age, RRR)%>%
      unique()
  }
  
  names(RRR)<-names(new_prop)<-names(dist_RR)<-riskfactors
  return(RRR)
}