#US 2020 Democratic elections: https://en.wikipedia.org/wiki/Results_of_the_2020_Democratic_Party_presidential_primaries

library(tidyverse)
library(rvest)
library(xml2)
library(ggplot2)
library(reshape2)


url=read_html("https://en.wikipedia.org/wiki/Results_of_the_2020_Democratic_Party_presidential_primaries")

table= url %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>% html_table(header = T, fill = T)

table=table[[1]]

rawtable=as.data.frame(table)

colnames(rawtable)[1]="Date"
colnames(rawtable)[2]="State"
colnames(rawtable)[ncol(rawtable)]="Total"

rawtable=rawtable[-c(1,2,60),]

totalresults=rawtable %>% gather(key,value,-Date, -State) %>% separate(value, into = c("popvote", "others"), sep = "%") %>% mutate(popvote=paste0(popvote,"%")) %>% 
  separate(others,into = c("Delegates", "rest"), sep = "delegate") %>% select(-rest) %>% 
  melt(id.vars=c("Date","State","key"),measures.vars = c("popvote", "Delegates")) %>% 
  dcast(Date+State~key+variable)


colnames(totalresults)

stillcandidates=totalresults %>% select(-c(Total_Delegates, Total_popvote)) %>% mutate(Date_OK=as.Date(Date, format="%b %d")) %>% arrange(Date_OK) %>% 
  mutate(State=gsub("\\(caucuses\\)","",State), State=gsub("firehouse caucus", "", State), State=gsub("US Virgin Islands","United States Virgin Islands",State ))


stillcandidates$Winner = "Primary pending"
stillcandidates$Photo = NA
stillcandidates$Winner_class = NA
stillcandidates$Sanders_style = "Sanders"
stillcandidates$Buttigieg_style = "Buttigieg"
stillcandidates$Warren_style = "Warren"
stillcandidates$Biden_style = "Biden"
stillcandidates$Klobuchar_style = "Klobuchar"
stillcandidates$Bloomberg_style = "Bloomberg"
stillcandidates$Others_style = "Others"


write.csv(stillcandidates, "USDem_tovisualise.csv")
