library(shiny)
library(shinydashboard)
library(flexdashboard)
library(rsconnect)
library("reshape2")
library("tidyquant")
library("quantmod")
library("TTR")
library("xts")
library("rio")
library("parsedate")
library("tidyverse")
library("ggplot2")
library("readr")
library("tidyr")
library("openxlsx")
library("csodata")
library("htmlwidgets")
library("plotly")
library("reshape2")
library("zoo")
library("dygraphs")
library("rio")
library("openxlsx")
library("parsedate")
library("janitor")
library("dplyr")
library("labeling")
library("purrr")
library("readxl")
library('stringr')
library('Rcpp')
library('ggrepel')
library('fresh')


data_uea1 <- read.csv("try05.csv")  %>%
  mutate(Obs = row_number())

CBI <- read.csv("CBI.csv")

data_mdd1 <- read_excel("Nowcast_inputs.xlsx") %>%
  mutate(Obs = row_number()) %>%
  filter(Obs < 25)

highlight_mdd <- data_mdd1 %>% 
  filter(Obs > 23) 

highlight_mdd1 <- data_mdd1 %>% 
  filter(Obs > 22) 

nohighlight_mdd <- data_mdd1 %>% 
  filter(Obs < 24) 

nohighlight_mdd1 <- data_mdd1 %>% 
  filter(Obs < 25) 

graph_uea2 <- ggplot(data_uea1, aes(x=Month)) + geom_bar(aes(fill=Economic.Indicators, y=Impact1), position="stack", stat="identity") + geom_point(data=subset(data_uea1, MDD.Growth != 0), aes(y=Underlying.Economic.Activity), colour = "black", fill="black", size = 2) + geom_line(data=subset(data_uea1, MDD.Growth != 0), aes(y=Underlying.Economic.Activity, group=1), stat = "identity", position = "identity", colour = "black") + labs(title = "Drivers of Underlying Economic Activity", caption = "Source: Central Statistics Office", x = "Month", y = "Underlying Economic Activity") + scale_x_discrete(breaks = c("2020M01", "2020M07", "2021M01", "2021M07", "2022M01", "2022M07", "2023M01", "2023M07")) + scale_fill_manual(values=c(
  "#5d5f56",
  "#5c6f9b", 
  "#062137",
  "#0090d4",
  "#00a685",
  "#dbac00",
  "#a39161",
  "#21455f",
  "#004d44",
  "#6f0e0e")) + theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.border = element_blank(),      
    axis.line = element_line(colour = "#004d44"),         
    axis.ticks = element_line(colour = "#004d44"),
    axis.text = element_text(color="#004d44", size = 7.5),
    text=element_text(color="#004d44"))



graph_mdd <- ggplot(data_mdd1, aes(Quarter, MDD)) + geom_area(data=nohighlight_mdd1, aes(group=1), colour = "#004d44", fill="#004d44", alpha=0.4) + geom_area(data=highlight_mdd1, aes(group=1), colour = "#a39161", fill="#a39161", alpha=0.8) + geom_line(aes(group=1), colour = "#004d44") + geom_line(data=highlight_mdd1, aes(group=1), colour = "#a39161") + geom_point(data=data_mdd1, aes(x=Quarter, y=MDD, group=1), colour = "#a39161", fill = "#a39161", alpha=5, size=1.5) + geom_point(data=nohighlight_mdd, aes(x=Quarter, y=MDD, group=1), colour = "#004d44", fill = "#004d44", alpha=5, size=1.5) + labs(caption = "Source: Central Statistics Office", x = "Quarter", y = "Year-on-Year MDD Growth", colour = "#004d44") + scale_x_discrete(breaks = c("2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1", "2023Q1"), labels =c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = 2, linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),         
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_mdd_qoq <- ggplot(data_mdd1, aes(Quarter, MDD_QoQ)) + geom_area(data=nohighlight_mdd1, aes(group=1), colour = "#004d44", fill="#004d44", alpha=0.4) + geom_area(data=highlight_mdd1, aes(Quarter, MDD_QoQ, group=1), colour = "#a39161", fill="#a39161", alpha=0.8) + geom_line(aes(group=1), colour = "#004d44") + geom_line(data=highlight_mdd1, aes(group=1), colour = "#a39161") + geom_point(data=data_mdd1, aes(x=Quarter, y=MDD_QoQ, group=1), colour = "#a39161", fill = "#a39161", alpha=5, size=1.5) + geom_point(data=nohighlight_mdd, aes(x=Quarter, y=MDD_QoQ, group=1), colour = "#004d44", fill = "#004d44", alpha=5, size=1.5) + labs(caption = "Source: Central Statistics Office", x = "Quarter", y = "Quarter-on-Quarter MDD Growth", colour = "#004d44") + scale_x_discrete(breaks = c("2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1", "2023Q1"), labels =c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),         
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_pce_qoq <- ggplot(data_mdd1, aes(Quarter, PCE_QoQ)) + geom_area(data=nohighlight_mdd1, aes(group=1), colour = "#004d44", fill="#004d44", alpha=0.4) + geom_area(data=highlight_mdd1, aes(group=1), colour = "#a39161", fill="#a39161", alpha=0.8) + geom_line(aes(group=1), colour = "#004d44") + geom_line(data=highlight_mdd1, aes(group=1), colour = "#a39161") + geom_point(data=data_mdd1, aes(x=Quarter, y=PCE_QoQ, group=1), colour = "#a39161", fill = "#a39161", alpha=5, size=1.5) + geom_point(data=nohighlight_mdd, aes(x=Quarter, y=PCE_QoQ, group=1), colour = "#004d44", fill = "#004d44", alpha=5, size=1.5) + geom_line(aes(group=1), colour = "#004d44") + geom_line(data=highlight_mdd1, aes(group=1), colour = "#a39161") + labs(caption = "Source: Central Statistics Office", x = "Quarter", y = "Quarter-on-Quarter PCE Growth", colour = "#004d44") + scale_x_discrete(breaks = c("2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1", "2023Q1"), labels =c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),         
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_pce <- ggplot(data_mdd1, aes(Quarter, PCE)) + geom_area(data=nohighlight_mdd1, aes(group=1), colour = "#004d44", fill="#004d44", alpha=0.4) + geom_area(data=highlight_mdd1, aes(group=1), colour = "#a39161", fill="#a39161", alpha=0.8) + geom_line(aes(group=1), colour = "#004d44") + geom_line(data=highlight_mdd1, aes(group=1), colour = "#a39161") + geom_point(data=data_mdd1, aes(x=Quarter, y=PCE, group=1), colour = "#a39161", fill = "#a39161", alpha=5, size=1.5) + geom_point(data=nohighlight_mdd, aes(x=Quarter, y=PCE, group=1), colour = "#004d44", fill = "#004d44", alpha=5, size=1.5) + geom_line(aes(group=1), colour = "#004d44") + geom_line(data=highlight_mdd1, aes(group=1), colour = "#a39161") + labs(caption = "Source: Central Statistics Office", x = "Quarter", y = "Year-on-Year PCE Growth", colour = "#004d44") + scale_x_discrete(breaks = c("2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1", "2023Q1"), labels =c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),         
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))


graph_pce1 <- graph_pce + annotate("text", x = "2023Q3", y = -2, label = "Nowcast", colour = "#a39161", size = 3.5)
graph_pce2 <- graph_pce_qoq + annotate("text", x = "2023Q3", y = -2, label = "Nowcast", colour = "#a39161", size = 3.5)

graph_mdd1 <- graph_mdd + annotate("text", x = "2023Q3", y = -2.5, label = "Nowcast", colour = "#a39161", size = 3.5)
graph_mdd2 <- graph_mdd_qoq + annotate("text", x = "2023Q3", y = -1, label = "Nowcast", colour = "#a39161", size = 3.5)

graph_uea3 <- ggplotly(graph_uea2) %>%
  layout(legend=list(title=list(text='Drivers of Economic Activity')))

graph_uea4 <- ggplot(data_uea1, aes(x=Month)) + geom_bar(aes(fill=Economic.Indicators, y=Impact1), position="stack", stat="identity") + geom_line(data=subset(data_uea1, MDD.Growth != 0), aes(y=Underlying.Economic.Activity, group=1), stat = "identity", position = "identity", colour = "black") + geom_point(data=subset(data_uea1, MDD.Growth != 0), aes(y=Underlying.Economic.Activity, group=1), stat = "identity", position = "identity", colour = "black", size = 0.75) + labs(caption = "Source: Central Statistics Office", x = "Month", y = "Underlying Economic Activity") + scale_x_discrete(breaks = c("2020M01", "2020M07", "2021M01", "2021M07", "2022M01", "2022M07", "2023M01", "2023M07")) + scale_fill_manual(values=c(
  "#5d5f56",
  "#5c6f9b", 
  "#062137",
  "#0090d4",
  "#00a685",
  "#dbac00",
  "#a39161",
  "#21455f",
  "#004d44",
  "#6f0e0e")) + theme(
    panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.border = element_blank(),      
    axis.line = element_line(colour = "#004d44"),         
    axis.ticks = element_line(colour = "#004d44"),
    axis.text = element_text(color="#004d44", size = 7.5),
    text=element_text(color="#004d44"),
    legend.position = "bottom")

graph_uea5 <- ggplotly(graph_uea4) %>%
  layout(legend=list(
    title=list(
      text='Drivers of Economic Activity')))

unemployment <- read.csv("UnemploymentCountry.csv")

graph_unemployment <- ggplot(unemployment, aes(x = Month)) + geom_line(aes(y = Total, group=1), colour = "#004d44") + geom_line(aes(y = Youth, group=1), colour = "#a39161") + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Month", y = "Unemployment Rate") + scale_x_discrete(breaks = c("1998M01", "2000M01", "2002M01", "2004M01", "2006M01", "2008M01", "2010M01", "2012M01", "2014M01", "2016M01", "2018M01", "2020M01", "2022M01")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

postings <- read.csv("Postings.csv")

 

graph_postings <- ggplot(postings, aes(x = Date)) + geom_line(aes(y = Ireland, group=1), colour = "#004d44") + geom_line(aes(y = US, group=1), colour = "#a39161") + geom_line(aes(y = Germany, group=1), colour = "#00a685") + geom_line(aes(y = UK, group=1), colour = "#0090D4") + geom_rect(aes(xmin = "2020-04-01", xmax = "2020-12-01", ymin = 122, ymax = 158), fill = "grey", alpha = 0.5) + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Date", y = "Indexed Number of Jobs Posts (Base = Feb 2020)") + scale_x_discrete(breaks = c("2020-06-01", "2020-12-01", "2021-06-01", "2021-12-01", "2022-06-01", "2022-12-01", "2023-06-01", "2023-12-01"),  labels = c("June 2020", "December 2020", "June 2021", "December 2021", "June 2022", "December 2022", "June 2023", "December 2023")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_postings1 <- graph_postings + annotate("text", x = "2020-08-01", y = 155, label = "Ireland", colour = "#004d44", size = 3) + annotate("text", x = "2020-08-01", y = 145, label = "United States", colour = "#a39161", size = 3) + annotate("text", x = "2020-08-01", y = 135, label = "Germany", colour = "#00a685", size = 3) + annotate("text", x = "2020-08-01", y = 125, label = "United Kingdom", colour = "#0090D4", size = 3)


wagecomparision <- read.csv("WageComparision.csv")

graph_wagecomparision <- ggplot(wagecomparision, aes(x = Month)) + geom_line(aes(y = Ireland, group=1), colour = "#004d44") + geom_line(aes(y = US, group=1), colour = "#a39161") + geom_line(aes(y = EuroArea, group=1), colour = "#00a685") + geom_line(aes(y = UK, group=1), colour = "#0090D4") + geom_rect(aes(xmin = "2019M04", xmax = "2020M02", ymin = 5.1, ymax = 7.4), fill = "grey", alpha = 0.5) + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Month", y = "Wage Growth Rate") + scale_x_discrete(breaks = c("2019M01", "2019M07", "2020M01", "2020M07", "2021M01", "2021M07", "2022M01", "2022M07", "2023M01", "2023M07")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_wagecomparision1 <- graph_wagecomparision + annotate("text", x = "2019M09", y = 7.1, label = "Ireland", colour = "#004d44", size = 3) + annotate("text", x = "2019M09", y = 6.5, label = "United States", colour = "#a39161", size = 3) + annotate("text", x = "2019M09", y = 5.9, label = "Euro Area", colour = "#00a685", size = 3) + annotate("text", x = "2019M09", y = 5.3, label = "United Kingdom", colour = "#0090D4", size = 3)


graph_unemployment1 <- graph_unemployment + annotate("text", x = "2011M11", y = 18, label = "Total Unemployment", colour = "#004d44", size = 3.5) + annotate("text", x = "2011M11", y = 33, label = "Youth Unemployment", colour = "#a39161", size = 3.5)

PALF <- read.csv("PALF.csv")

graph_PALF <- ggplot(PALF, aes(x = Quarter)) + geom_line(aes(y = PALF, group=1), colour = "#004d44") + geom_line(aes(y = Average, group=1), colour = "#a39161", linetype = "dashed") + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Quarter", y = "Value") + scale_x_discrete(breaks = c("2015Q1", "2016Q1", "2017Q1", "2018Q1", "2019Q1", "2020Q1", "2021Q1", "2022Q1", "2023Q1")) + theme(
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_PALF1 <- graph_PALF + annotate("text", x = "2022Q3", y = 0.637, label = "Average 2015-19", colour = "#a39161", size = 3)

totalpermits <- read.csv("TotalPermits.csv")

graph_totalpermits <- ggplot(totalpermits, aes(x = Month)) + geom_line(aes(y = Permits, group=1), colour = "#004d44") + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Month", y = "Number of Permits") + scale_x_discrete(breaks = c("2015M01", "2016M01", "2017M01", "2018M01", "2019M01", "2020M01", "2021M01", "2022M01", "2023M01")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

sectorpermits <- read.csv("WorkPermits.csv")

graph_permits <- ggplot(sectorpermits, aes(x=Month)) + geom_bar(aes(fill=Sector, y=Share), position="stack", stat="identity") + labs( caption = "Source: Central Statistics Office", x = "Month", y = "Share") + scale_x_discrete(breaks = c("2022M01", "2022M04", "2022M07", "2022M10", "2023M01", "2023M04", "2023M07", "2023M10")) + scale_fill_manual(values=c(
  "#5d5f56", 
  "#062137",
  "#0090d4",
  "#00a685",
  "#dbac00",
  "#a39161",
  "#004d44")) + theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.border = element_blank(),      
    axis.line = element_line(colour = "#004d44"),         
    axis.ticks = element_line(colour = "#004d44"),
    axis.text = element_text(color="#004d44", size = 7.5),
    text=element_text(color="#004d44"))

pmi <- read.csv("PMI.csv")


graph_pmi <- ggplot(pmi, aes(x=Month)) + geom_line(aes(y = Services, group=1), colour = "#004d44") + geom_line(aes(y = Manufacturing, group=1), colour = "#00a685") + geom_rect(aes(xmin = "2021M01", xmax = "2023M10", ymin = 50, ymax = 75), fill = "#004d44", alpha = 0.3) + geom_rect(aes(xmin = "2021M01", xmax = "2023M10", ymin = 25, ymax = 50), fill = "#a39161", alpha = 0.3) + geom_line(aes(y = Construction, group=1), colour = "#a39161") + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Month", y = "Value") + scale_x_discrete(breaks = c("2021M01", "2021M07", "2022M01", "2022M07", "2023M01", "2023M07")) + theme(
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_pmi1 <- graph_pmi + annotate("text", x = "2022M05", y = 35, label = "Contraction", colour = "#a39161", size = 5) + annotate("text", x = "2022M05", y = 65, label = "Expansion", colour = "#004d44", size = 5)

PMOD <- read.csv("PMOD1.csv")

graph_PMOD <- ggplot(PMOD, aes(x=Value, y=Sector, fill=Month)) + geom_bar(stat="identity", width = 0.7, position=position_dodge()) + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Value", y = "Sector") + theme(
  panel.grid.major.x = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_PMOD1 <- graph_PMOD + scale_fill_manual(values=c('#004d44',"#a39161", "#00a685"))

wages <- read.csv("Wages.csv")

graph_wages <- ggplot(wages, aes(x=Month)) + geom_line(aes(y = IndeedWageTracker, group=1), colour = "#004d44") + geom_line(aes(y = CoreHICP, group=1), colour = "#a39161", linetype = "dashed") + labs(caption = "Source: Central Statistics Office, Average refers to period 2015-19", x = "Month", y = "Value") + scale_x_discrete(breaks = c("2019M01", "2019M07", "2020M01", "2020M07", "2021M01", "2021M07", "2022M01", "2022M07", "2023M01", "2023M07")) + theme(
  panel.grid.major.y = element_line(colour = "#004d44", linetype = "dotted", linewidth = 0.1),
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.border = element_blank(),      
  axis.line = element_line(colour = "#004d44"),   
  axis.ticks = element_line(colour = "#004d44"),
  axis.text = element_text(color="#004d44", size = 7.5),
  text=element_text(color="#004d44"))

graph_wages1 <- graph_wages + annotate("text", x = "2019M08", y = 0.3, label = "Inflation", colour = "#a39161", size = 4) + annotate("text", x = "2019M08", y = 4.2, label = "Wage Growth", colour = "#004d44", size = 4)


my_theme = create_theme(
  adminlte_color(
    green = "#004d44"
  )
)


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Nowcast Graphs", tabName = "nowcast", icon = icon("dashboard")),
    menuItem("Labour Market", icon = icon("person"), tabName = "labour",
             badgeLabel = "New", badgeColor = "green")
  )
)

body <- dashboardBody(
  use_theme(my_theme),
  tabItems(
        tabItem(tabName = "nowcast",
          h2("Nowcast Graphs"),
  fluidRow(
           tabBox(
             width = 9,
             selected = "UEA",
             tabPanel("UEA", graph_uea5),
             tabPanel("MDD Year-on-Year", ggplotly(graph_mdd1)),
             tabPanel("MDD Quarter-on-Quarter", ggplotly(graph_mdd2)),
             tabPanel("PCE Year-on-Year", ggplotly(graph_pce1)),
             tabPanel("PCE Quarter-on-Quarter", ggplotly(graph_pce2))
             ),
    
           tabBox(
             width = 3,
             selected = "Summary",
             tabPanel("Summary", p("The most recent Nowcast for 2023Q4 suggests that:"),
                      p("Modified Domestic Demand growth for 2023Q4 is estimated to be 1.69% Year-on-Year and 1.74% Quarter-on-Quarter."),
                      p("Personal Consumption Expenditure change for 2023Q4 is forecast to be 3.56% Year-on-Year and 5.26% Quarter-on-Quarter."),
                      p("Underlying Economic Activity for October was -0.07.")),
             tabPanel("Updates", "Live list of data inputs to go here, like the Fed Nowcast."),
             )
  ),
  
  fluidRow(
           tabBox(
             width = 12,
             selected = "Methodology",
             tabPanel("Methodology",
                      p("The Methodology used for estimating the figures used in these graphs is outlined in detail in the paper 'Where are we now? Examining Irish Economic Developments in Real-Time' by Luke Rehill and Luke Daly (2020)."),
                      p("In summary, the Nowcasts are made by a dynamic factor model that incorporates Kalman-filtering techniques. This type of Nowcasting model has been tested and found to be effective in a wide variety of countries at different stages of the business cycle. The Nowcast estimates are made based on a variety of data sources that are updated on a monthly basis, covering areas as wide ranging as industrial output, consumer sentiment, and the government bond market.")),
             tabPanel("List of Variables",
                      p("The Consumption variable uses measures such as retail sales to estimate the extent to which consumer spending is driving economic activity within the state."),
                      p("The Financial category measures the contribution to economic activity of fluctuations in Irish stock and government bond markets."),
                      p("The Fiscal variable shows the effect of government spending and taxation on the economy."),
                      p("Industrial Output measures the contribution of manufacturing to economic activity."),
                      p("The Investment variable shows the contribution of private capital expenditure to economic activity."),
                      p("Labour Market measures the extent to which deviations from the trend in labour demand and the participation rate contribute to growth."),
                      p("Prices isolates the portion of economic activity which can be attributed to changes in the price of goods and services."),
                      p("The Soft category incorporates data such as consumer surveys and Purchasing Manager Indices, which are intended to capture changes to market and consumer sentiment that are difficult to measure in real-time with hard data."),
             )
             )
  )
           ),
  
  tabItem(tabName = "labour",
          h2("Labour Market"),
          fluidRow(
            tabBox(
              width = 9,
              selected = "Unemployment",
              tabPanel("Unemployment", ggplotly(graph_unemployment1)),
              tabPanel("Estimates of Payroll Employees", ggplotly(graph_PMOD1)),
              tabPanel("Total Employment Permits", ggplotly(graph_totalpermits)),
              tabPanel("Employment Permits by Sector", ggplotly(graph_permits)),
              tabPanel("Employment PMIs", ggplotly(graph_pmi1)),
              tabPanel("Indeed Job Postings", ggplotly(graph_postings1)), 
              tabPanel("Nominal Wage Growth vs Inflation", ggplotly(graph_wages1)),
              tabPanel("Wage Growth Comparision", ggplotly(graph_wagecomparision1))
              
              
            ),
            
            tabBox(
              width = 3,
              selected = "Summary",
              tabPanel("Summary", 
                       p("The unemployment rate is __% overall, relative to __% in the previous month and an average rate of 7.2% between 2015 and 2019."),
                       p("PMOD data suggests overall employment growth, with especially strong performances in the ___ sectors, but a fall in employment in ___."),
                       p("Employment PMIs remained above 50, suggesting growth in employment (_ for construction, __ for services, and __ for industry). "),
                       p("The Health sector remained the single largest beneficiary of the work permits issued, accounting for __% of the total."),
                       p("Data provided by Indeed suggests nominal wage growth of __%, relative to inflation of __%.")),
              tabPanel("Updates", "Live list of data inputs to go here, like the Fed Nowcast."))
            
            ),
  
  
          
          fluidRow(
            tabBox(
              width = 12,
              selected = "Explanation of Variables",
              tabPanel("Explanation of Variables",
                       p("Youth unemployment measures the unemployment rate of those aged between 15 and 24 years, while total unemployment includes the entire labour force aged between 15 and 74."),
                       p("The Estimates of Payroll Employees are taken from the CSO series Monthly Estimates of Payroll Employees using Administrative Data, which uses PAYE data froom revenue to estimate the number of employees by sector, age and gender."),
                       p("The Total Employment Permits figure combines all of the work permits issued by the state irrespective of the category, and as such includes recipients of the Critical Skills Employment Permit, the General Employment Permit, Dependant/Partner/Spouse Employment Permit, ect.."),
                       p("Purchasing Managers' Indexes (PMIs) are indicators constructed using surveys of private sector companies. A value above 50 indicates an expansion, while a value above this level suggests a contraction. Employment PMIs are a subsection of these indexes which focus specifically on levels of employment."),
                       p("The Indeed Job Posting variable measures the number of jobs posted on the job site Indeed, indexed to 1 February 2020."),
                       p("The wage growth variables are also derived from Indeed data, with the wages listed for newly posted job adverts being used as a proxy for overall wage developments"))
              
            )
          )
  

  )
  )
)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Economic Indicators"),
  sidebar,
  body
)

server <- function(input, output) {
  observeEvent(input$switchtab, {
    newtab <- switch(input$tabs,
                     "nowcast" = "labour",
                     "labour" = "nowcast"
    )
    updateTabItems(session, "tabs", newtab)
  })
}

shinyApp(ui = ui, server = server)


