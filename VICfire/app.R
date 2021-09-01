# Load library
library(shiny)
library(dplyr)
library(leaflet)
library(readr)
library(KernSmooth)
library(shinyWidgets)
library(plotly)
library(sp)
library(mapview)
library(leafem)
library(rgdal)
library(maptools)
library(raster)
library(DT)
library(htmlwidgets)


js <- c(
    "function(el, x){",
    "  el.on('plotly_legendclick', function(evtData) {",
    "    Shiny.setInputValue('trace', evtData.data[evtData.curveNumber].name);",
    "  });",
    "}")


# Load in training data
load("save.RData")

ystart = -39.08246
yend = -34.03690
xstart = 140.6923
xend = 149.8976
y = seq(ystart+0.1/2,yend -0.1/2 ,.1)
x = seq(xstart + 0.181/2, xend - 0.181/2, 0.181)
png <- "https://cdn.cfa.vic.gov.au/o/cfa-theme/images/cfa-logo.png"

#if (interactive()) {
    # User interface ----
    ui <- fluidPage(
        navbarPage("VICfire", id="main",

                   tabPanel("Historical fire map",
                            fluidRow(

                                column(8,align="left",
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Historical locations on fires, and ignition causes, in Victoria over 2000-2019."),

                                               sliderInput("year", label = "Choose year:",
                                                           value = c(2010,2017),
                                                           min = min(mydata$year),
                                                           max=max(mydata$year),
                                                           step = 1,
                                                           sep = ""),

                                               checkboxGroupButtons("month", label = "Choose month:",
                                                                    choices = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                                                                    individual = TRUE,justified = FALSE, selected=c("Nov","Dec","Jan"),
                                                                    width = "100%"),

                                               checkboxGroupInput("reason", label = "Choose reason:",
                                                                  choices = levels(factor(mydata$new_cause)), selected = c("lightning","arson","accident")),
                                               actionButton("showd","Show density plot"),
                                               actionButton("cleard","Clear density plot")),

                                           mainPanel(
                                               leafletOutput(outputId = "map",height = 587)))),


                                column(4,
                                       conditionalPanel(condition = "output.condition1 == 0",
                                           br(),
                                           tags$h4("About",style='color:blue'),p('This Shiny App helps visualise fires in Victoria for last two decades.
                            YAfter choose the year, month, and ignition reason. The fires match these condition will automatically shown on the map.
                                                                                 Due to the limitation of the package, the density plot cannot be refreshed automatically. Each time you change the conditions, you have to clear and re-plot the densitiy plot.
                                                                                 By clicking a fire on the map, relevant infomation will pop up and weather infomation will be shown below.')
                                       ),
                                       conditionalPanel(
                                           condition = "output.condition1 == 1",
                                           tabsetPanel(
                                               tabPanel(tags$em("Percentage",style="font-size:100%"),
                                                        plotlyOutput("p1",height = 300),
                                                        plotlyOutput("p2",height = 200)),
                                               tabPanel(tags$em("Rainfall",style="font-size:100%"),
                                                        tags$hr(style="border-color:  #d27979;"),
                                                        plotlyOutput("rain")),
                                               tabPanel(tags$em("Temperature",style="font-size:100%"),
                                                        tags$hr(style="border-color:  #ffc266;"),
                                                        plotlyOutput("temp"))
                                           )
                                       )
                                )
                            )
                   ),

                   tabPanel("2019-2020 Predicted causes",
                            fluidRow(
                                column(8,align="left",
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Predicted causes for fire ignition points over Oct 2019 through Mar 2020."),
                                               checkboxGroupButtons("month1", label = "Choose month:",
                                                                    choices = c("Oct","Nov","Dec","Jan","Feb","Mar"),
                                                                    individual = TRUE, justified = FALSE, selected = c("Dec"),
                                                                    width = "100%"),
                                               checkboxGroupInput("reason1", label = "Choose reason:",
                                                                  choices = levels(factor(prediction$new_cause)), selected = c("arson","lightning","accident"))
                                               ),
                                           mainPanel(leafletOutput(outputId = "map2",height = 587))

                                       )),
                                column(4,
                                       conditionalPanel(condition = "output.condition2 == 0",
                                                        br(),
                                                        tags$h4("About this page",style='color:blue'),
                                                        p('The fires shown on this page happened 2019.10-2020.4 located by Himawari 8. The causes of such fires are yet to be investigated.
                                                          All ignition reasons are predicted based on our model.
                                                          By clicking a fire on the map, relevant infomation will pop up and weather infomation will be shown below.')
                                       ),
                                       conditionalPanel(
                                           condition = "output.condition2 == 1",
                                           tabsetPanel(
                                               tabPanel(tags$em("Rainfall",style="font-size:100%"),
                                                        tags$hr(style="border-color:  #d27979;"),
                                                        plotlyOutput("rain1")),
                                               tabPanel(tags$em("Temperature",style="font-size:100%"),
                                                        tags$hr(style="border-color:  #ffc266;"),
                                                        plotlyOutput("temp1"))
                                           )
                                       )
                                )
                            )
                   ),


                   tabPanel("Fire risk map",
                            fluidRow(
                                column(8,align="left",
                                       sidebarLayout(
                                           sidebarPanel(
                                               helpText("Predicted fire probability maps"),
                                               radioButtons("month2", label = "Choose month:",
                                                            choices = c("Oct","Nov","Dec","Jan","Feb","Mar"),selected = c("Oct")),
                                               radioButtons("reason2", label = "Choose reason:",
                                                            choices = levels(factor(prediction$new_cause)), selected = c("arson"))
                                           ),
                                           mainPanel(leafletOutput(outputId = "map3",height = 587))

                                       )),
                                # wellPanel(
                                #     tabsetPanel(
                                #         # tabPanel(tags$em("Percentage",style="font-size:100%"),
                                #         #          tags$hr(style="border-color: #ffc266;"),
                                #         #          plotlyOutput("p3"),
                                #         #          plotlyOutput("p4")
                                #         # ),
                                #
                                #         tabPanel(tags$em("Rainfall",style="font-size:100%"),
                                #                  tags$hr(style="border-color:  #d27979;"),
                                #                  plotlyOutput("rain1")),
                                #
                                #         tabPanel(tags$em("Temperature",style="font-size:100%"),
                                #                  tags$hr(style="border-color:  #ffc266;"),
                                #                  plotlyOutput("temp1"))
                                #     )
                                # )

                            )
                   ),




                   tabPanel("Data", DT::dataTableOutput("data")),
                   tabPanel("Information",includeMarkdown("readme.md"))

        )
    )

    # Server logic ----
    server <- function(input, output) {

        pal <- colorFactor(pal = c("#E69F00","#000000","#0072B2","#009E73", "#F0E442","#CC79A7"), domain = c("arson","lightning","burningoff","accident","relight","other"))
        pal1 <- colorFactor(pal = c("#E69F00","#000000","#0072B2","#009E73"), domain = c("arson","lightning","burningoff","accident"))
        pal2 <- colorFactor(
            palette = 'red',
            domain = c(0,1)
        )



        dataselected_1 <- reactive({
            mydata <- subset(mydata,year >= input$year[1] & year <= input$year[2])
        })

        dataselected_2 <- reactive({
            mydata <- subset(dataselected_1(), month %in% input$month)
        })


        dataselected <- reactive({if(is.null(input$reason)){subset(dataselected_2(), new_cause == 1)}
            else{dataselected_2() %>% filter(new_cause %in% input$reason )}
        })


        pre_1 <- reactive({
            subset(prediction, month %in% input$month1)
        })

        pre_2 <- reactive({if(is.null(input$reason1)){subset(pre_1(), new_cause == 1)}
            else{pre_1() %>% filter(new_cause %in% input$reason1)}
        })

        d0d <- bkde2D(cbind(mydata$lon,mydata$lat),bandwidth=c(.0045, .0068), gridsize = c(50,50))

         palRaster <- colorNumeric(palette = c("yellow","Red"), domain = c(1,0), na.color = "transparent")



        #density polygons
        selectedarson <- reactive({dataselected() %>% filter(new_cause == "arson")})
        d2d_arson <- reactive({bkde2D(cbind(selectedarson()$lon, selectedarson()$lat), bandwidth=c(0.15,0.15))})
        lines_arson<- reactive({contourLines(d2d_arson()$x1,d2d_arson()$x2,d2d_arson()$fhat)})
        dd1_arson <- reactive({sapply(1:length(lines_arson()), function(i) Polygon(as.matrix(cbind(lines_arson()[[i]]$x,lines_arson()[[i]]$y))))})
        dd2_arson <- reactive({sapply(1:length(lines_arson()), function(i) Polygons(list(dd1_arson()[[i]]),i))})
        poly_data_arson <- reactive({data.frame(Value = sapply(1:length(lines_arson()), function(i) lines_arson()[[i]]$level))})
        dd3_arson <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_arson()),data = poly_data_arson())})

        selectedlightning <- reactive({dataselected() %>% filter(new_cause == "lightning")})
        d2d_lightning <- reactive({bkde2D(cbind(selectedlightning()$lon,selectedlightning()$lat),bandwidth=c(0.15,0.15))})
        lines_lightning<- reactive({contourLines(d2d_lightning()$x1,d2d_lightning()$x2,d2d_lightning()$fhat)})
        dd1_lightning <- reactive({sapply(1:length(lines_lightning()), function(i) Polygon(as.matrix(cbind(lines_lightning()[[i]]$x,lines_lightning()[[i]]$y))))})
        dd2_lightning <- reactive({sapply(1:length(lines_lightning()), function(i) Polygons(list(dd1_lightning()[[i]]),i))})
        poly_data_lightning <- reactive({data.frame(Value = sapply(1:length(lines_lightning()), function(i) lines_lightning()[[i]]$level))})
        dd3_lightning <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_lightning()),data = poly_data_lightning())})

        selectedburningoff <- reactive({dataselected() %>% filter(new_cause == "burningoff")})
        d2d_burningoff <- reactive({bkde2D(cbind(selectedburningoff()$lon, selectedburningoff()$lat), bandwidth=c(0.15,0.15))})
        lines_burningoff<- reactive({contourLines(d2d_burningoff()$x1,d2d_burningoff()$x2,d2d_burningoff()$fhat)})
        dd1_burningoff <- reactive({sapply(1:length(lines_burningoff()), function(i) Polygon(as.matrix(cbind(lines_burningoff()[[i]]$x,lines_burningoff()[[i]]$y))))})
        dd2_burningoff <- reactive({sapply(1:length(lines_burningoff()), function(i) Polygons(list(dd1_burningoff()[[i]]),i))})
        poly_data_burningoff <- reactive({data.frame(Value = sapply(1:length(lines_burningoff()), function(i) lines_burningoff()[[i]]$level))})
        dd3_burningoff <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_burningoff()),data = poly_data_burningoff())})

        selectedaccident <- reactive({dataselected() %>% filter(new_cause == "accident")})
        d2d_accident <- reactive({bkde2D(cbind(selectedaccident()$lon,selectedaccident()$lat),bandwidth=c(0.15,0.15))})
        lines_accident<- reactive({contourLines(d2d_accident()$x1,d2d_accident()$x2,d2d_accident()$fhat)})
        dd1_accident <- reactive({sapply(1:length(lines_accident()), function(i) Polygon(as.matrix(cbind(lines_accident()[[i]]$x,lines_accident()[[i]]$y))))})
        dd2_accident <- reactive({sapply(1:length(lines_accident()), function(i) Polygons(list(dd1_accident()[[i]]),i))})
        poly_data_accident <- reactive({data.frame(Value = sapply(1:length(lines_accident()), function(i) lines_accident()[[i]]$level))})
        dd3_accident <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_accident()),data = poly_data_accident())})


        selectedrelight <- reactive({dataselected() %>% filter(new_cause == "relight")})
        d2d_relight <- reactive({bkde2D(cbind(selectedrelight()$lon,selectedrelight()$lat),bandwidth=c(0.15,0.15))})
        lines_relight<- reactive({contourLines(d2d_relight()$x1,d2d_relight()$x2,d2d_relight()$fhat)})
        dd1_relight <- reactive({sapply(1:length(lines_relight()), function(i) Polygon(as.matrix(cbind(lines_relight()[[i]]$x,lines_relight()[[i]]$y))))})
        dd2_relight <- reactive({sapply(1:length(lines_relight()), function(i) Polygons(list(dd1_relight()[[i]]),i))})
        poly_data_relight <- reactive({data.frame(Value = sapply(1:length(lines_relight()), function(i) lines_relight()[[i]]$level))})
        dd3_relight <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_relight()),data = poly_data_relight())})


        selectedother <- reactive({dataselected() %>% filter(new_cause == "other")})
        d2d_other <- reactive({bkde2D(cbind(selectedother()$lon,selectedother()$lat),bandwidth=c(0.15,0.15))})
        lines_other<- reactive({contourLines(d2d_other()$x1,d2d_other()$x2,d2d_other()$fhat)})
        dd1_other <- reactive({sapply(1:length(lines_other()), function(i) Polygon(as.matrix(cbind(lines_other()[[i]]$x,lines_other()[[i]]$y))))})
        dd2_other <- reactive({sapply(1:length(lines_other()), function(i) Polygons(list(dd1_other()[[i]]),i))})
        poly_data_other <- reactive({data.frame(Value = sapply(1:length(lines_other()), function(i) lines_other()[[i]]$level))})
        dd3_other <- reactive({SpatialPolygonsDataFrame(SpatialPolygons(dd2_other()),data = poly_data_other())})

        clicked_map <- reactiveValues(clickedMarker=NULL)
        observeEvent(input$map_marker_click,{
            clicked_map$clickedMarker <- input$map_marker_click
        })


        selected_coordinates <- reactive(({
            c(clicked_map$clickedMarker$lng,clicked_map$clickedMarker$lat)
        }))


        clicked<- reactive(({
            subset(dataselected(),lon == as.numeric(selected_coordinates()[1]) & lat == as.numeric(selected_coordinates()[2]))
        }))

        condition1<-reactive({
            if(is.null(selected_coordinates())){
                result=0
            }else{
                result=1
            }
            result
        })

        output$condition1 <- renderText({
            condition1()
        })

        outputOptions(output, 'condition1', suspendWhenHidden=FALSE)


        output$rain <- renderPlotly({
            rain=clicked()
            if(is.null(rain))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(rain$avr7,rain$avr14,rain$avr28),name="20 years average",opacity=0.3,
                type = "bar") %>%
                add_trace(y= c(rain$arf7,rain$arf14,rain$arf28),name = "rain fall for that time", type = 'bar',width=0.3,opacity=1)%>%
                layout(title = '',
                       xaxis = list(title = "Period Average rain fall"),
                       yaxis = list(title = "mm"),
                       barmode = 'overlay')
        })


        output$temp <- renderPlotly({
            temp=clicked()
            if(is.null(temp))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(temp$amaxt7,temp$amaxt14,temp$amaxt28),
                type = 'scatter', mode = 'lines',name="max",line = list(color = 'rgb(205, 12, 24)')) %>%
                add_trace(y = c(temp$amint7,temp$amint14,temp$amint28),name="min",line = list(color = 'rgb(22, 96, 167)'))%>%
                add_trace(y = c(temp$avmin7,temp$avmin14,temp$avmin28),name="20 year average min",line = list(color = 'rgb(22, 96, 167)'),opacity=0.3)%>%
                add_trace(y = c(temp$avmax7,temp$avmax14,temp$avmax28),name="20 year average max",line = list(color = 'rgb(205, 12, 24)'),opacity=0.3)%>%
                layout(title = '',
                       xaxis = list(title = "Period Average Max/Min Temperature"),
                       yaxis = list(title = "Temperature (°C)")
                       )
        })




        clicked_map2 <- reactiveValues(clickedMarker=NULL)
        observeEvent(input$map2_marker_click,{
            clicked_map2$clickedMarker <- input$map2_marker_click
        })

        selected_coordinates1 <- reactive(({
            c(clicked_map2$clickedMarker$lng,clicked_map2$clickedMarker$lat)
        }))


        clicked1<- reactive(({
            subset(pre_2(),lon == as.numeric(selected_coordinates1()[1]) & lat == as.numeric(selected_coordinates1()[2]))
        }))

        condition2<-reactive({
            if(is.null(selected_coordinates1())){
                result=0
            }else{
                result=1
            }
            result
        })

        output$condition2 <- renderText({
            condition2()
        })

        outputOptions(output, 'condition2', suspendWhenHidden=FALSE)

        output$rain1 <- renderPlotly({
            rain1=clicked1()
            if(is.null(rain1))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(rain1$arf7,rain1$arf14,rain1$arf28),
                name = "rain fall",
                type = "bar") %>%
                layout(title = '',
                       xaxis = list(title = "Period Average rain fall"),
                       yaxis = list(title = "mm"))
        })


        output$temp1 <- renderPlotly({
            temp1=clicked1()
            if(is.null(temp1))
                return(NULL)
            plot_ly(
                x = c(" 7day","14day","28day"),
                y = c(temp1$amaxt7,temp1$amaxt14,temp1$amaxt28),
                type = 'scatter', mode = 'lines',name="max") %>%
                add_trace(y = c(temp1$amint7,temp1$amint14,temp1$amint28),name="min")%>%
                layout(title = '',
                       xaxis = list(title = "Period Average Max/Min Temperature"),
                       yaxis = list(title = "°C"))
        })



        # initiate a plotly object

        tx <- highlight_key(mydata2,~new_cause)

        # initiate a plotly object
        base <- plot_ly(tx, color = ~new_cause) %>% group_by(year)

        # create a time series of median house price
        t <- base %>%
            group_by(new_cause) %>%
            add_lines(x = ~year, y = ~Total,legendgroup = ~new_cause)

        hist <- add_histogram(base, x= ~Total, histnorm = "probability density")


        gig <- ggplot(mydata2, aes(x = year, y = Total, fill = new_cause)) +
            geom_bar(stat = "identity")



        fig <- plot_ly(mydata4, x = ~year, y = ~accident, type = 'bar',
                       name = 'accident',marker=list(color="#E69F00",level=1)) %>%

            add_trace(y = ~arson, name = 'arson',marker=list(color="#000000"))%>%
            add_trace(y = ~burningoff, name = 'burningoff',marker=list(color="#0072B2"))%>%
            add_trace(y = ~lightning, name = 'lightning',marker=list(color="#009E73"))%>%
            add_trace(y = ~other, name = 'other',marker=list(color="#F0E442"))%>%
            add_trace(y = ~relight, name = 'relight',marker=list(color="#CC79A7"))%>%
            layout(yaxis = list(title = 'Total number'),legend=list(traceorder="normal"), barmode = 'stack')

        output$p1 <- renderPlotly({
            fig
            fig %>% onRender(js)
        })

        output$p2 <- renderPlotly({
            d <- input$trace


             mydata2 %>%
                filter(new_cause %in% d) %>%
                 plot_ly()%>%
                 add_lines(x = ~year, y = ~Total,name="Fire trend")%>%
                 layout(yaxis = list(zeroline = FALSE, showline = FALSE,
                     autotick =TRUE, ticks = "outside", rangemode = 'tozero', title = 'Total number'),
                     showlegend = TRUE, xaxis=list(autotick =TRUE,showline = TRUE,ticks = "outside"))

        })



        output$percentage <- renderPlot({
            ggplot() +
                geom_bar(data=filter(dataselected(), new_cause==input$reason),
                         aes(x=year,y=fire,fill = new_cause,label= "fire"),stat='identity') +
                scale_fill_manual("legend", values = c("arson" = "#000000","lightning" ="#009E73", "relight" = "#CC79A7","other"="#F0E442","accident"="#E69F00","burningoff"="#0072B2"))

        })

        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addLegend(pal=pal, values=mydata$new_cause) %>%
                addLayersControl(overlayGroups =c("show all fire"))%>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6.3)%>%
                addMouseCoordinates()%>%
                leafem::addLogo(png, url = "https://www.cfa.vic.gov.au/home")%>%
                addCircles(data = mydata1, lat =  ~lat, lng =~lon,
                           radius = 100, color="#C0C0C0",
                           stroke = FALSE, fillOpacity = 0.7, group="show all fire")%>%
                hideGroup("show all fire")

        })



        observe({leafletProxy("map") %>%
                clearMarkers() %>%
                addCircleMarkers(data = dataselected(), lat =  ~lat, lng =~lon,
                                 radius = 3,
                                 color = ~pal(new_cause),
                                 stroke = FALSE, fillOpacity = 20,
                                 popup= ~paste0("Fire ID: ", EVENTID, "<br/>",
                                     "Fire starts at: ", FIRE_START, "<br/>",
                                                "Wind speed: ", round(ws,2),"  m/s","<br/>",
                                                "Fire reason: ", new_cause,"<br/>",
                                                "Forest types: ",FOR_TYPE,"<br/>",
                                                "Distance to road: ",round(dist_road),"  metres"
                                 ))

        })


        observeEvent(input$showd,
                     {
                         leafletProxy("map") %>%
                             addTiles() %>%clearGroup(group="plot density")

                         if ("accident" %in% input$reason){
                             leafletProxy("map") %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_accident(),col="#E69F00",group="plot density",stroke = FALSE)}

                         if ("arson" %in% input$reason){
                             leafletProxy("map") %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_arson(),col="#000000",group="plot density",stroke = FALSE)}

                         if ("burningoff" %in% input$reason){
                             leafletProxy("map")  %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_burningoff(),col="#0072B2",group="plot density",stroke = FALSE)}

                         if ("lightning" %in% input$reason){
                             leafletProxy("map") %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_lightning(),col="#009E73",group="plot density",stroke = FALSE)}

                         if ("other" %in% input$reason){
                             leafletProxy("map")%>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_other(),col="#F0E442",group="plot density",stroke = FALSE)}

                         if ("relight" %in% input$reason){
                             leafletProxy("map")  %>%
                                 addTiles() %>%
                                 addPolygons(data=dd3_relight(),col="#CC79A7",group="plot density",stroke = FALSE)}


                     }
        )


        observeEvent(input$cleard,{
            leafletProxy("map") %>% addTiles() %>%
                clearGroup(group="plot density")

        })


        output$map2 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addLegend(pal=pal1, values=prediction$new_cause) %>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6.3)%>%
                addMouseCoordinates()%>%
                addCircleMarkers(data = pre_2(), lat =  ~lat, lng =~lon,
                                 radius = 3,
                                 color = ~pal1(new_cause),
                                 stroke = FALSE, fillOpacity = 20)

        })




        observe({leafletProxy("map2") %>%
                clearMarkers() %>%
                addCircleMarkers(data = pre_2(), lat =  ~lat, lng =~lon,
                                 radius = 3,
                                 color = ~pal1(new_cause),
                                 stroke = FALSE, fillOpacity = 20,
                                 popup= ~paste0("Fire reason: ", new_cause,"<br/>",
                                                "Forest types: ",FOR_TYPE,"<br/>",
                                                "Distance to road: ",round(dist_road)))
        })


        KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r10a))
        KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA

        output$map3 <- renderLeaflet({
            leaflet() %>%
                addProviderTiles("CartoDB")  %>%
                setView(lng= 144.7852, lat = -36.3913 , zoom = 6.3)%>%
                addLegend(pal = palRaster, values = c(1,0),
                          title = "Fire Probability")%>%
                addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)

        })



        observe({leafletProxy("map3") %>%
                addProviderTiles("CartoDB") %>% clearShapes()

            if( "arson"%in%input$reason2 & "Oct"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r10a))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}




            if("accident"%in%input$reason2 & "Oct"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r10ac))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}




            if( "lightning"%in%input$reason2 & "Oct"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r10l))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA



                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}



            if( "burningoff"%in%input$reason2 & "Oct"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r10b))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA



                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "arson"%in%input$reason2 & "Nov"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r11a))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "accident"%in%input$reason2 & "Nov"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r11ac))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA



                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "lightning"%in%input$reason2 & "Nov"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r11l))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "burningoff"%in%input$reason2 & "Nov"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r11b))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "arson"%in%input$reason2 & "Dec"%in%input$month2){
                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r12a))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "accident"%in%input$reason2 & "Dec"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r12ac))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "lightning"%in%input$reason2 & "Dec"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r12l))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "burningoff"%in%input$reason2 & "Dec"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r12b))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "arson"%in%input$reason2 & "Jan"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r1a))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "accident"%in%input$reason2 & "Jan"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r1ac))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "lightning"%in%input$reason2 & "Jan"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r1l))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}

            if( "burningoff"%in%input$reason2 & "Jan"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r1b))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "arson"%in%input$reason2 & "Feb"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r2a))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA



                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "accident"%in%input$reason2 & "Feb"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r2ac))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA



                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "lightning"%in%input$reason2 & "Feb"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r2l))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}



            if( "burningoff"%in%input$reason2 & "Feb"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r2b))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}




            if( "arson"%in%input$reason2 & "Mar"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r3a))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}



            if( "accident"%in%input$reason2 & "Mar"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r3ac))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


            if( "lightning"%in%input$reason2 & "Mar"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r3l))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}



            if( "burningoff"%in%input$reason2input$reason2 & "Mar"%in%input$month2){

                KernelDensityRaster<-raster(list(x=d0d$x1 ,y=d0d$x2 ,z = r3b))
                KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.07)] <- NA


                leafletProxy("map3") %>%
                    addRasterImage(KernelDensityRaster,colors = palRaster,opacity = .4)}


        })


        output$data <-DT::renderDataTable(datatable(
            mydata[,c(4:5,8,10,11,13,14,65)],filter = 'top',
            colnames = c("Fire name","Fire district","Fire Start","Longitude","Latitude","Forest Type","Forest Category",
                         "Cause")
        ))





    }
    # Run app ----
    shinyApp(ui, server)

#}

