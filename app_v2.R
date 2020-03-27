#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(plotly)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SWAP's Aircrew Scheduler"),

    # Sidebar with a slider inputs
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "num_crews_in",
                        label = "Number of Crews:",
                        min = 1,
                        max = 50,
                        value = c(20, 30)),
            sliderInput(inputId = "crew_availability",
                        label = "Crew Availability (as %)",
                        min = 0,
                        max = 1,
                        value = 1),
            sliderInput(inputId = "msn_req_in",
                        label = "Number of Simultaneous Missions",
                        min = 1,
                        max = 15,
                        value = 8),
            sliderInput(inputId = "ROM",
                        label = "ROM",
                        min = 5,
                        max = 20,
                        value = 14),
            sliderInput(inputId = "pre_msn",
                        label = "Pre-Mission Days",
                        min = 0,
                        max = 3,
                        value = 1),
            sliderInput(inputId = "max_msnTime_in",
                        label = "Max Mission Time",
                        min = 1,
                        max = 50,
                        value = c(10,30)),
            numericInput(inputId ="msn_time_plot",
                         label = "Mission Time for Schedule Builder",
                         value = 14,
                         min = 0),
            numericInput(inputId ="num_crews_plot",
                         label = "Number of Crews for Schedule Builder",
                         value = 20,
                         min = 0),
            numericInput(inputId ="req_msn_plot",
                         label = "Required Msns for Schedule Builder",
                         value = 8,
                         min = 0),
            numericInput(inputId ="ROM_plot",
                         label = "ROM for Schedule Builder",
                         value = 14,
                         min = 0),
            numericInput(inputId ="pre_msn_plot",
                         label = "Pre-msn Rest for Schedule Builder",
                         value = 1,
                         min = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plot"),
           plotOutput("plot2"),
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
schedule_long<-reactive(
  {
  #Shedule Plot
  msn_time<-input$msn_time_plot
  msn_req<-input$req_msn_plot
  n_crews<-input$num_crews_plot
  ROM<-input$ROM_plot
  pre_msn<-input$pre_msn_plot
  
  Crew_Int<-(msn_time/(msn_req))
  Crew_Cycle<-Crew_Int*(n_crews-1)+Crew_Int
  Crew<-seq(1,n_crews,by=1)
  
  White_Space<-Crew_Cycle-ROM-msn_time-pre_msn
  Msn_start_1<-Crew_Int*(Crew-1)+2
  Msn_stop_1<-Msn_start_1+msn_time
  ROM_stop_1<-Msn_stop_1+ROM
  White_stop_1<-ROM_stop_1+White_Space
  Msn_start_2<-White_stop_1+1+pre_msn
  Msn_stop_2<-Msn_start_2+msn_time
  ROM_stop_2<-Msn_stop_2+ROM
  White_stop_2<-ROM_stop_2+White_Space
 
  data1<-data.frame(cbind(Crew, Msn_start_1, Msn_stop_1, ROM_stop_1, White_stop_1, Msn_start_2, Msn_stop_2, ROM_stop_2, White_stop_2))
 dfs <- data1 %>%
   pivot_longer(cols = -c("Crew"), names_to = 'segment', values_to = 'days') %>%
   separate(., col = segment, into = c('type', 'start_stop', 'msn_num'), sep = '_') %>%
   pivot_wider(id_cols = c(Crew, type, msn_num), names_from = start_stop, values_from = days) %>%
   group_by(Crew, msn_num) %>%
   arrange(stop) %>%
   mutate(start_adj = ifelse(is.na(start), lag(stop), start))
  return(dfs)
}
 )

    # Do calculations
    # this makes date_in responsive to changes in the inputs
    data_in <- reactive(
        {
        num_crews_seq <- seq(input$num_crews_in[1], input$num_crews_in[2])    
        max_msnTime_seq <- seq(input$max_msnTime_in[1], input$max_msnTime_in[2])
        
        df <- expand.grid(num_crews_seq,input$msn_req_in,max_msnTime_seq) %>%
            rename(Number_of_Crews = "Var1", Req_Missions = "Var2", Max_Msn_Time = "Var3") %>%
            mutate(WhiteSpace = 0)
        
        for (i in 1:nrow(df)) {
            num_crews <- df[i,1]
            msn_req <- df[i,2]
            msn_time <- df[i,3]
            n_crews <- num_crews * input$crew_availability
            #Crew 9 starts when Crew 1 is done = msn_time.
            #so 9/8, (msn_time/(msn_req))*(Crew#-1)+2 (subtract 1)
            #Each crew's start is, assuming Crew 1 starts Day 2
            ##This is the CREW Interval
            #(msn_time/(msn_req))
            ##Then each crew take off time:
            #(msn_time/(msn_req))*(Crew#-1)+2

            #NOW what is the Cycle Interval
            ##Crew 1 must take one crew interval after when CREW MAX takes off
            ##(msn_time/(msn_req))*(n_crews-1)+2+(msn_time/(msn_req))
            Total_limit <- ((msn_time/(msn_req)))*(n_crews - 1)+ (msn_time/(msn_req))
            df[i,4] <- Total_limit - input$ROM - msn_time - input$pre_msn
        }
        
        df <- df %>%
            mutate(time_bw_launch = Max_Msn_Time/Req_Missions) %>%
            mutate(time_req_msns = Max_Msn_Time/Req_Missions*Req_Missions)
        return(df)
        }
    )
    
    output$table <- renderTable(data_in())
    
    output$plot <- renderPlotly(
        plot_ly(data = data_in(), x = ~Number_of_Crews, y = ~WhiteSpace,color=~Max_Msn_Time,
                         type = 'scatter',
                         mode = 'markers'
                )
    )
    
    output$plot2<-renderPlot( ggplot(schedule_long(), aes(x=start_adj, y = Crew)) +
        geom_segment(aes_string(x = start_adj, y = Crew, xend = stop, yend = Crew, group = Crew, color = type))+
        scale_colour_manual(values=c("green", "Red", "Blue"))+
        xlab("Day"))    
}

# Run the application 
shinyApp(ui = ui, server = server)
