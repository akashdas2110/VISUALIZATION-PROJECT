rm(list=ls())
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(here)
library(treemapify)
library(ggpubr)
library(treemapify)

###data section####

mat=read.csv(file="matches2.csv")
salary=read.csv("salary.csv")
del=read.csv("deliveries2.csv")
del_w=read.csv("del_w.csv")
mat=mat[(mat$season>2007 & mat$season<2018),]

toss_stat=mat%>%
  group_by(toss_winner) %>% 
  summarise(freq=n(),toss_decision=toss_decision)
toss_stat=toss_stat[(toss_stat$freq>25),]

toss_stat2=mat%>% 
  group_by(toss_winner)%>%
  summarise(r=sum(win_by_runs))

bm=del %>% 
  group_by(batsman)%>% 
  summarise(tot_run=sum(batsman_runs),balls_played=n())
bm1=bm[(bm$balls_played>2500),]

b=del_w%>% 
  group_by(bowler)%>% 
  summarise(balls=n(),overs=n()/6,w=sum(wkt),runs_consumed=sum(total_runs),economy=sum(total_runs)/n(),sr=sum(total_runs)/sum(wkt))%>%
  arrange(-w)


br=b[(b$w>95),]

salary=salary[( salary$Runs > 0),]
sal=salary %>%
  group_by(Name) %>% 
  summarise(run=sum(Runs),money=sum(Final.Price),money_per_run=sum(Final.Price)/sum(Runs)/100000)

sal=sal[(sal$run > 2000),]
sal=sal[order(sal$money_per_run,sal$run),]
sal2=sal[1:12,]
sal33=sal[13:24,]
salary=read.csv(file="salary.csv")
salary=salary[( salary$Wkts > 0),]
sal_b=salary %>%
  group_by(Name) %>% 
  summarise(Wkts=sum(Wkts),money=sum(Final.Price),money_per_wkts=sum(Final.Price)/sum(Wkts)/1000000)%>%
  arrange( money_per_wkts)

sal_b=sal_b[(sal_b$Wkts > 75),]
sal_b1=sal_b[1:11,]
sal_b2=sal_b[12:21,]

team_boundary=del[(del$batsman_runs==4),]
t_4=team_boundary%>%
  group_by(bowling_team) %>% 
  summarise(fours=n())
t_4=t_4[(t_4$fours>800),]

team_over_boundary=del[(del$batsman_runs==6),]
t_6=team_over_boundary%>%
  group_by(bowling_team) %>% 
  summarise(sixes=n())

t_6=t_6[(t_6$sixes>250),]
salary_1=read.csv(file="salary.csv")
team_stat=salary_1 %>%
  group_by(Team) %>% 
  summarise(fours=sum(Fours),sixes=sum(Sixes),four_wkt=sum(FourWkts),five_wkt=sum(FiveWkts),centuries=sum(Hundreds),halfcenturies=sum(Fifties),matches=n())
team_stat=team_stat[(team_stat$matches>50),]


ui = dashboardPage(
  dashboardHeader(title="IPL - Interactive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Team-wise analysis ",tabName = "team"),
      menuItem("Top batsmans",tabName = "bat"),
      menuItem("Top Bowlers", tabName = "ball"),
      menuItem("Salary-wise analysis", tabName = "salary")
     
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("about",
              fluidPage(
                fluidRow(
                  column(
                    12,tags$h1(tags$u("INTRODUCTION"),align="center"),
                    
tags$h2("About The Project"),
tags$h4("My project contains 4 parts: "),
tags$p(tags$b("1. Team wise performance analysis:"),
        tags$ul(tags$li("  Which team preferred batting or fielding after winning the tosses in IPL ?")),
        tags$ul(tags$li("  Which team scored how many boundaries,over-boundaries,centuries and half-centuries in IPL ?")),
        tags$ul(tags$li("  Which team took how many 4-wicket hauls and 5-wicket hauls in IPL?"))
        
        ),
tags$p(tags$b("2. Performance Analysis Of Top Batsmen :"),
       tags$ul(tags$li("  Against which bowlers the batsman scored well or struggle to score in IPL?")),
       tags$ul(tags$li("  In which innings the batsman scored how many runs in IPL ?")),
       tags$ul(tags$li("  Against which teams batsman performed well or bad in IPL ?"))
       
),
tags$p(tags$b("3. Performance Analysis Of Top Bowlers :"),
       tags$ul(tags$li("  Against which batsmen the bowler gave less amount of runs or more amount of runs along with strikerate ")),
       tags$ul(tags$li("  In which innings the bowler took how many wickets in IPL ?")),
       tags$ul(tags$li("  Against which teams bowler performed well or bad in IPL ?"))
       
),
tags$p(tags$b("4. Salary Wise Analysis:"),
       tags$ul(tags$li("  Which players received low amount of money but performed well ?")),
       tags$ul(tags$li("  Which players received huge amount of money but performed moderately ?"))
       
       
)
                  )
                )
              )
        
      ),
      tabItem("team",
              tabsetPanel(
                
                
                tabPanel("TOSS",
                         
                         
                         fluidPage(titlePanel("TOSS DECISIONS"),
                                   
                                    
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       radioButtons("choice1","make selections:",choices=c("ALL","Chennai Super Kings","Deccan Chargers","Delhi Daredevils","Kings XI Punjab","Kolkata Knight Riders","Mumbai Indians","Rajasthan Royals","Royal Challengers Bangalore","Sunrisers Hyderabad"),selected = "ALL")
                                       
                                     ),
                                     
                                    
                                     mainPanel(
                                       plotOutput("d1"),
                                       textOutput("t1")
                                     )
                                   )
                         )),
                tabPanel("FOURS",
                         fluidPage( 
                           titlePanel(" TOTAL NO. OF 4'S SCORED BY A TEAM VS. DIFFERENT OPPONENTS"),
                           
                           
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("choices","make selections:",choices=c("ALL",t_4$bowling_team),selected = "ALL")
                               
                             ),
                             
                            
                             mainPanel(
                               plotOutput("d2")
                               ,textOutput("t2")
                             )
                           )
                           )),
                tabPanel("SIXES",
                         fluidPage( 
                           titlePanel("TOTAL NO. OF 6's SCORED BY A TEAM VS. DIFFERENT OPPONENTS"),
                           
                            
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("choice666","make selections:",choices=c("ALL",t_6$bowling_team),selected = "ALL")
                               
                             ),
                             
                             
                             mainPanel(
                               plotOutput("d666")
                               ,textOutput("t3")
                             )
                           )
                         )),
                tabPanel("50's",
                         fluidPage( 
                           
                           plotOutput("d50")
                           ,textOutput("t4")
                          )),
                tabPanel("100's",
                         fluidPage( 
                           
                           plotOutput("d100")
                           ,textOutput("t5")
                         )),
                tabPanel("4-WKT HAUL",
                         fluidPage( 
                           plotOutput("d4wkt")
                           ,textOutput("t6")
                           
                         )),
                tabPanel("5-WKT HAUL",
                         fluidPage( 
                           
                               plotOutput("d5wkt")
                               ,textOutput("t7")
                             
                         ))
                
                
                
              )
              
              
      ),
      
      tabItem("bat",
              tabsetPanel(
                tabPanel("HIGHEST RUN SCORERS",
                         fluidPage( 
                           
                           plotOutput("d0")
                           ,textOutput("t8")
                         )
                         
                         ),
                tabPanel("AGAINST BOWLERS",
                         fluidPage(
                           titlePanel("Batsman's Performance Against Different Bowlers"),
                           
                            
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("choice","make selections:",choices=c(bm1$batsman),selected = bm1$batsman[1])
                               
                             ),
                             
                             
                             mainPanel(
                               plotOutput("d3")
                               ,textOutput("t9")
                             )
                           )
                         )
                         
                ),
                tabPanel("INNINGS WISE ",
                         fluidPage(
                           titlePanel("Innings Wise Run Scored By The Batsman"),
                           
                            
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("choice3","make selections:",choices=c(bm1$batsman),selected = bm1$batsman[1])
                               
                             ),
                             
                             
                             mainPanel(
                               plotOutput("d33")
                               ,textOutput("t10")
                             )
                           )
                         )
                         
                         
                
                         
                ),
                
                tabPanel("AGAINST OPPONENTS",
                         fluidPage(
                           titlePanel("Batsman's Performance Against Opponent Teams"),
                           
                           
                           sidebarLayout(
                             sidebarPanel(
                               
                               radioButtons("choice4","make selections:",choices=c(bm1$batsman),selected = bm1$batsman[1])
                               
                             ),
                             
                             
                             mainPanel(
                               plotOutput("d44")
                               ,textOutput("t11")
                             )
                           )
                         ))
                
                
                )
                
                
      ),
      tabItem("ball",tabsetPanel(  tabPanel("HIGHEST WICKET TAKERS",
                                             fluidPage( 
                                               
                                               plotOutput("dhw")
                                               ,textOutput("t12")
                                             )
                                             
      ),
                                      
        
                                      tabPanel("AGAINST BATSMEN",
                                             fluidPage( 
                                               titlePanel("Bowler's Performance Against Different Batsmen"),
                                               
                                               
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   
                                                   radioButtons("choice5","make selections:",choices=c(br$bowler),selected =br$bowler[1])
                                                   
                                                 ),
                                                 
                                                 
                                                 mainPanel(
                                                   plotOutput("d5")
                                                   ,textOutput("t13")
                                                 )
                                               )
                                             )
                                    ),
        tabPanel("INNINGS WISE",fluidPage(titlePanel("Innings Wise Wicket taken By The Bowler"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     radioButtons("choice6","make selections:",choices=c(br$bowler),selected =br$bowler[1])
                                     
                                   ),
                                   
                                  
                                   mainPanel(
                                     plotOutput("d6")
                                     ,textOutput("t14")
                                   
                                 )
          
        ))),
        tabPanel("PERFORMANCE AGAINST OPPONENTS",fluidPage(titlePanel("Bowler's Performance Against Opponent Teams"),
                                          
                                           
                                          sidebarLayout(
                                            sidebarPanel(
                                              
                                              radioButtons("choice7","make selections:",choices=c(br$bowler),selected =br$bowler[1])
                                              
                                            ),
                                            
                                            
                                            mainPanel(
                                              plotOutput("d7")
                                              ,textOutput("t15")
                                              
                                            )
                                            
                                          )) 
                 ))),
      tabItem("salary",tabsetPanel(
        tabPanel("PROFITABLE PLAYERS",h2("select"),fluidPage(actionButton("button1","batsman"),actionButton("button2","bowler"),
                                                                       plotOutput("oe1"),textOutput("t16"))),
        tabPanel("EXPENSIVE PLAYERS",h2("select"),fluidPage(actionButton("button3","batsman"),actionButton("button4","bowler"),plotOutput("oe2"),textOutput("t17")))))
      
      
    )
  )

)


server <- function(input, output) {
  
  
  x_toss=reactive({
    if (input$choice1=="ALL"){
      toss_stat
    }
    else if (input$choice1=="Chennai Super Kings"){
      toss_stat[(toss_stat$toss_winner=="Chennai Super Kings"),]
    }
    else if (input$choice1=="Deccan Chargers"){
      toss_stat[(toss_stat$toss_winner=="Deccan Chargers"),]
    }
    else if (input$choice1=="Delhi Daredevils"){
      toss_stat[(toss_stat$toss_winner=="Delhi Daredevils"),]
    }
    else if (input$choice1=="Kings XI Punjab"){
      toss_stat[(toss_stat$toss_winner=="Kings XI Punjab"),]
    }
    else if (input$choice1=="Kolkata Knight Riders"){
      toss_stat[(toss_stat$toss_winner=="Kolkata Knight Riders"),]
    }
    else if (input$choice1=="Rajasthan Royals"){
      toss_stat[(toss_stat$toss_winner=="Rajasthan Royals"),]
    }
    else if (input$choice1=="Royal Challengers Bangalore"){
      toss_stat[(toss_stat$toss_winner=="Royal Challengers Bangalore"),]
    }
    else if (input$choice1=="Sunrisers Hyderabad"){
      toss_stat[(toss_stat$toss_winner=="Sunrisers Hyderabad"),]
    }
    else if (input$choice1=="Mumbai Indians"){
      toss_stat[(toss_stat$toss_winner=="Mumbai Indians"),]
    }
  })
  
  
  
  
  x_1=reactive({
    ms=del %>% 
      filter(batsman == input$choice) %>% 
      group_by(bowler)%>% 
      summarise(tot_run=sum(batsman_runs),balls_played=n())%>%
      arrange(-balls_played)
    ms$strike_rate=ms$tot_run/ms$balls_played*100
    ms[c(1:20),]
    
    
  })
  
  x_44=reactive({
    if (input$choices=="ALL"){
      t_4
    }
    else {
      
      f=team_boundary %>% 
        filter(batting_team == input$choices) %>% 
        group_by(bowling_team)%>% 
        summarise(fours=n())
      f=f[(f$fours>100),]
      f
      
    }
  })
    x_666=reactive({
      if (input$choice666=="ALL"){
        t_6
      }
      else {
        
        s=team_over_boundary %>% 
          filter(batting_team == input$choice666) %>% 
          group_by(bowling_team)%>% 
          summarise(sixes=n())
        s=s[(s$sixes>30),]
        s
      }
  })
    
    
    
    
  x_2=reactive({
    ms_inn=del %>% 
      filter(batsman == input$choice3) %>% 
      group_by(inning) %>% 
      summarise(runs=sum(batsman_runs))
    x=ms_inn[(ms_inn$inning<=2),]
    x$innings=c("1st_innings","2nd_innings")
    x
    
    
    
  })
  x_3=reactive({
    
    ms3=del %>% 
      filter(batsman == input$choice4) %>% 
      group_by(bowling_team)%>% 
      summarise(tot_run=sum(batsman_runs),balls_played=n())%>%
      arrange(-balls_played)
    ms3=ms3[(ms3$balls_played>100),]
    ms3$strike_rate=ms3$tot_run/ms3$balls_played*100
    ms3
    
    })
  x_5=reactive({
    
    ms5=del %>% 
      filter(bowler == input$choice5) %>% 
      group_by(batsman)%>% 
      summarise(tot_run=sum(batsman_runs),balls_played=n())%>%
      arrange(-balls_played)
    ms5=ms5[c(1:20),]
    ms5$strike_rate=ms5$tot_run/ms5$balls_played*100
    ms5
    
    
    
    
  })
  
  x_6=reactive({
    ms_inn=del_w%>% 
      filter(bowler == input$choice6) %>% 
      group_by(inning) %>% 
      summarise(total_wkts=sum(wkt))
    x=ms_inn[(ms_inn$inning<=2),]
    x$innings=c("1st_innings","2nd_innings")
    x
    
    
    
  })
  
  x_7=reactive({
    ms_br=del_w%>% 
      filter(bowler == input$choice7) %>% 
      group_by(batting_team) %>% 
      summarise(total_wkts=sum(wkt),balls=n(),economy=(sum(total_runs)/n())) %>% 
      arrange(-balls)
    ms_br=ms_br[c(1:8),]
    ms_br
    
    
  })
  
  
 
  output$d1<- renderPlot({
    
    ggplot(x_toss(),aes(x=toss_winner,fill=toss_decision))+geom_bar(position = "dodge")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(y="Toss_Decision_Frequency",x="Team Names",title = "Post Toss Decision(Bat/Field) BY Different Teams",subtitle = "IPL Season : 2008-2017",tag=input$choice1 )
    
  })
  output$d2 <- renderPlot({
    
    ggplot(x_44(), aes(x = "", y = fours, fill = bowling_team)) +
      geom_col(color = "black") +
      coord_polar(theta = "y")+geom_text(aes(label = fours),position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Team")) +
      theme_void()+labs(title = "Total NO. Of Boundaries Scored Against Different Teams",subtitle = "IPL Season : 2008-2017",tag = input$choices)
    
    
    
  })
  output$d666 <- renderPlot({
    
    ggplot(x_666(), aes(x = "", y = sixes, fill = bowling_team)) +
      geom_col(color = "black") +
      coord_polar(theta = "y")+geom_text(aes(label = sixes),position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Team")) +
      theme_void()+labs(title = "Total NO. Of Over Boundaries Scored Against Different Teams",subtitle = "IPL Season : 2008-2017",tag = input$choice666)
    
 
    
  })
    
  output$d3 <- renderPlot({
    
    ggplot(x_1(),aes(strike_rate,tot_run))+geom_point()+geom_text(aes(label=bowler),hjust=1, vjust=1,cex=3)+geom_hline(yintercept =80)+geom_vline(xintercept =120)+labs(x="Strike Rate",y="Runs Scored",title = "Selected Batsman Vs. Different Bowlers He Played Most No. Of Times",subtitle = "IPL Season : 2008-2017",tag = input$choice)
    
  })
  
  
  
  output$d44 <- renderPlot({
    a=ggplot(x_3(),aes(reorder(bowling_team,-tot_run),tot_run,fill=bowling_team))+geom_col(position = "dodge")+theme(legend.position = "none")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(x="Opponent Teams",y="Runs Scored",title = "Runs Scored By The Selected Batsman Against Different Teams",subtitle = "IPL Season : 2008-2017",tag=input$choice4)
    b=ggplot(x_3(),aes(reorder(bowling_team,-strike_rate),strike_rate,fill=bowling_team))+geom_col(position = "dodge")+theme(legend.position = "none")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(x="Opponent Teams",y="Strike Rate",title = " Strike Rate Against Different Teams",subtitle = "IPL Season : 2008-2017",tag = input$choice4)
    ggarrange(a,b, ncol = 2, nrow = 1)
    
    
    
    
  })
  
  
  
  
  
  output$d33 <- renderPlot({
    ggplot(x_2(), aes(x = 3, y = runs, fill = innings)) +
      geom_col(color = "black") +
      geom_text(aes(label = runs),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "GnBu") +
      xlim(c(0.2, 3 + 0.5)) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())+labs(title = "Runs Scored By The Selected Batsman In Different Innings",subtitle = "IPL Season : 2008-2017",tag = input$choice3)
    
  })
  
  output$d5 <- renderPlot({
    ggplot(x_5(),aes(strike_rate,tot_run))+geom_point()+geom_text(aes(label=batsman),hjust=1, vjust=1,cex=3)+geom_hline(yintercept =80)+geom_vline(xintercept =120)+labs(x="Strike Rate",y="Runs Scored",title = "Selected Bowler Vs. Different Bowlers He Played Most No. Of Times",subtitle = "IPL Season : 2008-2017",tag = input$choice5)
    
    
    
  })
  
  output$d6 <- renderPlot({
    ggplot(x_6(), aes(x = 3, y = total_wkts, fill = innings)) +
      geom_col(color = "black") +
      geom_text(aes(label = total_wkts),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer(palette = "GnBu") +
      xlim(c(0.2, 3 + 0.5)) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())+labs(title = "Wicket Taken By The Selected Bowler In Different Innings",subtitle = "IPL Season : 2008-2017",tag = input$choice6)
    
  })
  
  output$d7 <- renderPlot({
    a=ggplot(x_7(),aes(reorder(batting_team,-total_wkts),total_wkts,fill=batting_team))+geom_col(position = "dodge")+theme(legend.position = "none")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(x="Opponent Teams",y="Total Wicket Taken",title = "Wickets Taken By The Selected Bowler Against Different Teams",subtitle = "IPL Season : 2008-2017",tag = input$choice7)
    b=ggplot(x_7(),aes(reorder(batting_team,economy),economy,fill=batting_team))+geom_col(position = "dodge")+theme(legend.position = "none")+theme(axis.text.x=element_text(angle=90,hjust=1))+labs(x="Opponent Teams",y="Economy",title = "Economy Against Different Teams",subtitle = "IPL Season : 2008-2017",tag = input$choice7)
    ggarrange(a,b, ncol = 2, nrow = 1)
 
  })
  observeEvent(input$button1,(output$oe1= renderPlot({ ggplot(sal2,aes(money_per_run,reorder(Name, -money_per_run),fill=Name))+geom_col()+theme(legend.position = "none")+labs(x="Money Per Run(/100000)",y="Batsman Names",title = "Money Per Run Rate of Different Batsman",subtitle = "(minimum 2000 balls played),IPL:2008-2017")})))
  observeEvent(input$button1,(output$t16= renderText("From the above diagram we can say that batsmen on the top are most profitable batsman for his team, they received less amount of money but score well.Teams like to purchase these type of players whose money-per-run rate is lower and they can give them good results in less amount of money.")
  ))
  
  observeEvent(input$button2,(output$oe1= renderPlot({ ggplot(sal_b1,aes(money_per_wkts,reorder(Name,-money_per_wkts),fill=Name))+geom_col()+theme(legend.position = "none")+labs(x="Money Per Run(*1000000)",y="Bowler Names",title = "Money Per Wicket Rate of Different Bowler",subtitle = "(minimum 75 wickets taken),IPL:2008-2017")})))
  observeEvent(input$button2,(output$t16= renderText("From the above diagram we can say that bowlers on the top are most profitable bowler for his team, they received less amount of money but take good amount of wickets.Teams like to purchase these type of players whose money-per-wicket rate is lower and they can give them good results in less amount of money.")
  ))
  observeEvent(input$button3,(output$oe2= renderPlot({ ggplot(sal33,aes(money_per_run,reorder(Name,money_per_run),fill=Name))+geom_col()+theme(legend.position = "none")+labs(x="Money Per Run(/100000)",y="Batsman Names",title = "Money Per Run Rate of Different Batsman",subtitle = "(minimum 2000 balls played),IPL:2008-2017")})))
  observeEvent(input$button3,(output$t17= renderText("From the above diagram we can say that batsmen on the top are most expensive batsman for his team, they received large amount of money but score moderate amount of runs.So their money-per-run rate is higher.")
  ))
  observeEvent(input$button4,(output$oe2= renderPlot({ ggplot(sal_b2,aes(money_per_wkts,reorder(Name,money_per_wkts),fill=Name))+geom_col()+theme(legend.position = "none")+labs(x="Money Per Run(*1000000)",y="Bowler Names",title = "Money Per Wicket Rate of Different Bowler",subtitle = "(minimum 75 wickets taken),IPL:2008-2017")})))
  observeEvent(input$button4,(output$t17= renderText("From the above diagram we can say that bowlers on the top are most expensive bowler for his team, they received large amount of money but take moderate amount of wickets.So their money-per-wicket rate is higher.")
  ))
  
  
  
  output$d0=renderPlot({
    ggplot(bm1,aes(fill=batsman,tot_run,reorder(batsman,tot_run)))+geom_col()+theme(legend.position = "none")+labs(x="Total Of Runs Scored",y="Batsman Names",title = "Total Runs Scored By Different Players ",subtitle = "(minimum 2500 runs),IPL Season : 2008-2017")
  
    })
  output$dhw=renderPlot({
    ggplot(br,aes(fill=bowler,w,reorder(bowler,w)))+geom_col()+theme(legend.position = "none")+labs(x="Total no. of wkts taken",y="Bowler Names",title = "Total no. of wkts taken By Different Players ",subtitle = "(minimum 95 wickets),IPL Season : 2008-2017")
    
  })
  
  output$d50=renderPlot({
    ggplot(team_stat, aes(area = halfcenturies, fill = Team,
                          label = paste(Team,halfcenturies , sep = "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15) +
      theme(legend.position = "none")+labs(title = "Total NO. Of Half-Centuries Scored BY Different Teams",subtitle = "IPL Season : 2008-2017")

    })
  
  output$d100=renderPlot({
    ggplot(team_stat, aes(area = centuries, fill = Team,
                          label = paste(Team,centuries , sep = "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15) +
      theme(legend.position = "none")+labs(title = "Total NO. Of Centuries Scored BY Different Teams",subtitle = "IPL Season : 2008-2017")
    })
  output$d4wkt=renderPlot({ 
    ggplot(team_stat, aes(x = "", y = four_wkt, fill = Team)) +
      geom_col(color = "black") +
      coord_polar(theta = "y")+geom_text(aes(label = four_wkt),position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Team")) +
      theme_void()+labs(title = "Total NO. Of Four Wicket Hauls Taken BY Different Teams",subtitle = "IPL Season : 2008-2017")
  })
  output$d5wkt=renderPlot({ 
    ggplot(team_stat, aes(x = "", y = five_wkt, fill = Team)) +
      geom_col(color = "black") +
      coord_polar(theta = "y")+geom_text(aes(label = five_wkt),position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Team")) +
      theme_void()+labs(title = "Total NO. Of Five Wicket Hauls Taken BY Different Teams",subtitle = "IPL Season : 2008-2017")
  })
  output$t1=renderText("From the above diagram we can easily see the preferences of each teams after winning the toss in IPL(Season : 2008-2017)
")
  output$t2=renderText("From the above pie diagram we can see that which team scores how many boundries in IPL(Season : 2008-2017)
")
  output$t3=renderText("From the above pie diagram we can see that which team scores how many over-boundries in IPL(Season : 2008-2017)
")
  output$t4=renderText("From the above treemap diagram we can see that which team scores how many half-centuries in IPL(Season : 2008-2017)
")
  output$t5=renderText("From the above treemap diagram we can see that which team scores how many centuries in IPL(Season : 2008-2017)
")
  output$t6=renderText("From the above pie diagram we can see that which team takes 4 wicket haul how many times in IPL(Season : 2008-2017) 
")
  output$t7=renderText("From the above pie diagram we can see that which team takes 5 wicket haul how many times in IPL(Season : 2008-2017) 
")
  output$t8=renderText("From the above horizontal bar diagram we can see that the Highest Run Scorers in IPL(Season : 2008-2017), and now we try to analyze the performence of these star batsmen in IPL.
  Based on this analyzed result opponent players/teams can make their strategies against that particular batsman. 
")
  output$t9=renderText("The above scatter plot shows performance of the selected batsman against the bowlers he played most.Observe that the bowlers in the 1st quadrant are favourite bowlers of that batsman, as he scored most number of runs against them with high strike rate and he struggled to play the bowlers in 3rd quadrant.")
  output$t10=renderText("The above donut diagram describes us selected player's batting performence in 1st and 2nd innings.")
  output$t11=renderText("The above bar diagram describes us batsman's scored run and strike rate against other teams.")
  output$t12=renderText("From the above horizontal bar diagram we can see that the Highest Wicket Takers in IPL(Season : 2008-2017), and now we try to analyze the performence of these star bowlers in IPL.
  Based on this analyzed result opponent players/teams can make their strategies against that particular bowler. 
")
  output$t13=renderText("The above scatter plot shows performance of the selected bowler against the batsmen he bowled most number of times.Observe that the batsmen in the 3rd quadrant are favourite bowlers of that batsman, as they scored less number of runs against the bowler with very low strike rate and he struggled to bowled the batsmen in 1st quadrant.")
  output$t14=renderText("The above donut diagram describes us selected player's bowling performence in 1st and 2nd innings.")
  output$t15=renderText("The above bar diagram describes us number of wickets taken by the bowler and his economy rate against other teams.")
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)






