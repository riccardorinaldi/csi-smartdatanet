##Soren Heitmann
##July 7, 2016
##sorenh@jhu.edu

library(shiny)

#Script runs a shiny application
#To learn more about shiny: shinyapps.io
#main.R must be run before executing this script
#This produces the interactive toBike visualization on bike use over time

#Low value: low percent spaces free (bikes mostly parked)
#High value: high percent spaces free (bikes mostly in-use)
#Torino Yellow: #ffc61a
#Torino Blue: #3399ff
col <- colorRamp(c("#ffc61a","#3399ff"))
by_station$station_pct_fill <- rgb(col(by_station$station_pct_free),max=255)

times <- unique(by_station[,list(window_mean_time,period)])

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("bikemap", width = "100%", height = "100%"),
  absolutePanel(top = 0, left = 0,width="100%",style="background: rgba(54, 54, 54, 0.5);",
                fluidPage(width="100%",style="margin-left:50px;margin-top:10px;padding-bottom:20px;",
                          fluidRow(
                            column(width=4,
                                   sliderInput("timeline",
                                               label=NULL, 
                                               ticks=F, 
                                               min=1, 
                                               max=dim(times)[1], 
                                               value=min(times$window_mean_time), 
                                               step=1,
                                               timeFormat="%H:%M",
                                               animate=animationOptions(interval = 150, 
                                                                        loop = FALSE)
                                                                        #playButton=HTML("<span style='text-decoration:none;position:relative;top:-16px;left:22px;color:blue;hover:none;font-size:20px;'>&#9658;</span>"),
                                                                        #pauseButton=HTML("<span style='position:relative;top:-20px;left:18px;color:blue;hover:none;font-size:22px;'>&#9632;</span>"))
                                  ),
                                  div(style="font-size:18px;font-weight:bold;position:relative;top:-70px;left:50px;z-index:50;",textOutput("timeValue")),
                                  
                                  div(style="position:absolute;top:48px;left:-10px;width:320px;height:70px;opacity:0.75;",plotOutput("statusPlot",width="100%",height="100%"))

                            ),
                            column(width=5,style="white-space:nowrap;font-weight:bold;color:white;",h2(style="position:relative;top:0px;font-size:40px;","ToBike System Usage")),
                            column(width=3,align="right",
                                   div(style="white-space:nowrap;padding-top:20px;",htmlOutput("systemStats"))
                            )
                          )
                )
  ),
  absolutePanel(left=0,width="100%",bottom="0",style="background-color:black;opacity:0.5;",
    fluidPage(fluidRow(column(width=12,align="center",style="margin-top:20px",
                    div(style="font-size:12px;font-family:verdana;font-weight:bold;color:white;position:relative;top:-20px;",HTML("<span style='color:#ffc61a'>Yellow:</span> Bikes In-Use | <span  style='color:#ffc61a'>Yellow Dot:</span> Empty Center of Mass | <span style='color:#3399ff'>Blue:</span> Bikes Parked | <span style='color:#3399ff'>Blue Dot:</span> Parked Center of Mass | <span style='color:#ff1aff;'>Purple:</span>New Station?"))))
              
  ))
)

server <- function(input, output, session) {

  all_dates <- unique(by_system[,date])
  date_ticks <- seq(from=1,to=length(all_dates),by=ceiling(length(all_dates)/6))
  date_ticks <- c(date_ticks,length(all_dates))
  
  lowest_pct_free <- min(by_station$station_pct_free)
  highest_pct_free <- max(by_station$station_pct_free)
  

  output$timeValue <- renderText({
    t <- max(input$timeline)
    current_time <- times$window_mean_time[t]
    if (is.na(current_time)) current_time <- min(times$window_mean_time)
    period <- unique(times[window_mean_time==current_time,period])[1]
    paste0(format(current_time,"%d %b %Y")," - ",period)
  })
  
  output$systemStats <- renderText({
    t <- max(input$timeline)    
    current_time <- times$window_mean_time[t]
    if (is.na(current_time)) current_time <- min(times$window_mean_time)
    stats <- unique(by_system[window_mean_time==current_time,list(system_bikes_in,system_spaces_free,system_pct_free,system_capacity,system_broken_bikes)])
    str <- paste0("<div style='text-align:right;font-weight:bold;color:#ffc61a;'>",round(stats$system_pct_free*100),"% in-use of ",stats$system_capacity,"<br>","Bikes Parked: ",stats$system_bikes_in,"<br>","Bikes Out: ",stats$system_spaces_free,"<br>","Broken Bikes: ",stats$system_broken_bikes,"</div>")

    str
  })
  
  output$statusPlot <- renderPlot({
    t <- max(input$timeline)
    current_time <- times$window_mean_time[t]
    if (is.na(current_time)) current_time <- min(times$window_mean_time)

    day_stats<- (by_system[window_mean_time <= current_time,list(mean_free=mean(system_spaces_free/system_capacity)),by=list(date)])$mean_free
    day_stats<- c(day_stats,rep(NA,length(all_dates)-length(day_stats)))
    
    par(mar = c(1.2,2,0,0),col="yellow",bg="#999999")  #Very cose!
    #par(mar = c(1.2,2,0,0),col="yellow",bg=NA)
    plot(x=NULL,y=NULL,type="n",yaxt="n",xaxt="n",bty="n",ylim=c(.4,.7),xlim=c(min(all_dates),max(all_dates)))
    axis(side=2,at=c(.45,.55,.65),labels=c("45%","55%","65%") ,font=2,las=1,xpd=T, cex.axis = .9,lwd=0,mgp=c(0,0,0))
    axis(side=1,at=all_dates[date_ticks], labels=format(all_dates[date_ticks], "%b"), font=2,cex.axis = .9,lwd=0,mgp=c(-1,-1,-1))
    lines(x=all_dates,y=day_stats)
    
  })
  
  observe({
    t <- max(input$timeline)
    current_time <- times$window_mean_time[t]
    if (is.na(current_time)) current_time <- min(times$window_mean_time)
    
    current_data <- by_station[window_mean_time==current_time]
    sog_data <- sog[window_mean_time==current_time]

    leafletProxy("bikemap") %>%
      clearMarkers() %>%
      addCircleMarkers(data=current_data,~long, ~lat, 
                       fillColor=~station_pct_fill,fillOpacity=.75,radius=12,stroke=F) %>%
      ##3399ff Blue
      addCircleMarkers(data=sog_data,~long_in, ~lat_in, 
                       fillColor="#3399ff",fillOpacity=.90,radius=8,
                       stroke=T,color="black",weight=1,opacity=0.6) %>%
      ##ffc61a Yellow
      addCircleMarkers(data=sog_data,~long_out, ~lat_out, 
                       fillColor="#ffc61a",fillOpacity=.90,radius=8,
                       stroke=T,color="black",weight=1,opacity=0.6) %>%
      
      addCircleMarkers(data=toBike_stations[toBike_stations$id<0,],~long, ~lat, 
                     fillColor="purple",fillOpacity=.90,radius=5,
                     stroke=T,color="black",weight=1,opacity=0.6)
    
      
  })  
  output$bikemap <- renderLeaflet({
    
    leaflet() %>%
      setView(lat=45.09,lng=7.641354,zoom=12) %>%
      addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = TRUE))
    
    })
}



shinyApp(ui, server)

