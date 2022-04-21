library(R6)

myModule <- R6Class("myModule",
                    public = list(
                      id = NULL,
                      data = NULL,
                      icons = NULL,
                      text = NULL,
                      geojson = jsonlite::fromJSON("http://polygons.openstreetmap.fr/get_geojson.py?id=347950&params=0", 
                                                   simplifyVector = FALSE),
                      
                      initialize = function(id){
                        self$id <- id
                      },
                      
                      ui_part = function(ns = NS(NULL)){
                        
                        ns <- NS(ns(self$id))
                        
                        tagList(
                          tabBox(title = self$id,
                                 tabPanel("Map", fluidRow(withSpinner(leafletOutput(ns("leaflet_map")))),
                                          br(),
                                          fluidRow(
                                            box(title = "Download data", status = "warning", 
                                                downloadButton(ns('downloader_xlsx'), 'xlsx', style = "width: 100px;"),
                                                downloadButton(ns('downloader_csv'), 'csv', style = "width: 100px;")),
                                            box(status = "primary", checkboxInput(ns("boundaries_module"), label = "Show city boundaries", value = F), width = 5, height = "97px"),
                                            box(status = "primary", actionBttn(
                                              inputId = ns("info_button"),
                                              label = NULL,
                                              style = "simple", 
                                              color = "primary",
                                              icon = icon("info")
                                            ), width = 1, height = "97px")
                                          )),
                                 tabPanel("Plots", plotlyOutput(ns("barplot1")),
                                 plotlyOutput(ns("barplot2"))),
                                 width = 9)
                        )
                        
                      },
                      
                      render_map = function(makeCluster = T){
                        callModule(private$leaflet_server, self$id, makeCluster = makeCluster)
                      },
                      
                      render_heatmap = function(){
                        callModule(private$leaflet_server_heatmap, self$id)
                      },
                      
                      render_plot1 = function(sum_var, grouped_var, xlab = "", ylab = "", title = "", 
                                              my_color = "palegreen", FUN = sum, ...){
                        callModule(private$plot_server1, self$id, data = self$data, sum_var = sum_var, grouped_var= grouped_var,
                                   xlab = xlab, ylab = ylab, title = title, 
                                   my_color = my_color, FUN = FUN, ...)
                      },
                      
                      render_plot2 = function(sum_var, grouped_var, xlab = "", ylab = "", title = "", 
                                              my_color = "palegreen", FUN = sum, ...){
                        callModule(private$plot_server2, self$id, data = self$data, sum_var = sum_var, grouped_var= grouped_var,
                                   xlab = xlab, ylab = ylab, title = title,
                                   my_color = my_color, FUN = FUN, ...)
                      }
                      
                    ),
                    private = list(
                      
                      leaflet_server = function(input, output, session, data = self$data, icons = self$icons, makeCluster = TRUE, geojson = self$geojson){
                        
                        output$leaflet_map  <- renderLeaflet({
                          
                          if(!is.data.frame(data())){
                            stop('Data argument must be a data.frame object')
                          }
                          
                          df <- data()
                          
                          if(makeCluster == TRUE){
                            
                            leaflet(df) %>%
                              addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 18)) %>%
                              setView(
                                lat=41.3991,
                                lng=2.1734,
                                zoom = 13
                              ) %>%
                              setMaxBounds(
                                lat1 = 41,
                                lat2 = 42,
                                lng1 = 2,
                                lng2 = 3
                              )  %>%
                              addAwesomeMarkers(
                                clusterOptions = markerClusterOptions(),
                                ~lon,
                                ~lat,
                                popup = ~popup,
                                icon = icons
                              )
                          }else{
                            
                            leaflet(df) %>%
                              addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 18)) %>%
                              setView(
                                lat=41.3991,
                                lng=2.1734,
                                zoom = 13
                              ) %>%
                              setMaxBounds(
                                lat1 = 41,
                                lat2 = 42,
                                lng1 = 2,
                                lng2 = 2.5
                              )  %>%
                              addAwesomeMarkers(
                                ~lon,
                                ~lat,
                                popup = ~popup,
                                icon = icons
                              )
                          }
                          
                        })
                        
                        
                        observeEvent(req(input$boundaries_module == T), {
                          leafletProxy("leaflet_map", data = geojson) %>%
                            addGeoJSON(geojson)
                        })
                        
                        observeEvent(req(input$boundaries_module == F), {
                          leafletProxy("leaflet_map") %>%
                            clearGeoJSON()
                        })
                        
                        output$downloader_xlsx <- downloadHandler(
                          filename = function() {
                            paste(self$id, "_", Sys.Date(), ".xlsx", sep = "")
                          },
                          content = function(file) {
                            
                            wb <- createWorkbook()
                            addWorksheet(wb, "Data")
                            writeData(wb = wb, sheet = "Data", x = data(), startCol = 1)
                            
                            styl <- createStyle(fontColour = "white", fgFill = "#002060", wrapText = T, textDecoration = "bold", halign = "right")
                            addStyle(wb = wb, sheet = "Data", style = styl, rows = 1, cols = c(1:(length(data()))), gridExpand = TRUE)
                            setColWidths(wb, sheet = "Data", cols = c(1:(length(data()))), widths = "auto")
                            
                            saveWorkbook(wb = wb, file = file)
                          }
                        )
                        
                        
                        output$downloader_csv <- downloadHandler(
                          filename = function() {
                            paste(self$id, "_", Sys.Date(), ".csv", sep = "")
                          },
                          content = function(file) {
                            write.csv(x = apply(data(),2,as.character), file = file, row.names = FALSE, fileEncoding = "CP1250")
                          }
                        )
                        
                        observeEvent(input$info_button, {
                          showModal(modalDialog(
                            title = "Details about the data",
                            self$text,
                            easyClose = TRUE
                          ))
                        })
                        
                      },
                      
                      plot_server1 = function(input, output, session, id = self$id, data = self$data, sum_var, grouped_var, xlab = "", ylab = "", title = "", 
                                             my_color = "palegreen", FUN = sum, ...){

                          output$barplot1 <- renderPlotly({
                            
                            if(!is.data.frame(data())){
                              stop('Data argument must be a data.frame object')
                            }
                            
                            if(is.matrix(col2rgb(my_color)) == F){
                              stop('Please provide a valid colour representation')
                            }
        
                            df <- data()
                            
                            if(nrow(df) == 0) return(ggplotly(ggplot()+theme_minimal()))
                              
                              
                            ggplotly(
                                tryCatch(expr = {
                                  df %>% 
                                    group_by(!!as.name(grouped_var)) %>% 
                                    summarize(val=FUN(!!as.name(sum_var), ...)) %>%
                                    eval},
                                         error = function(e){
                                           message("Could not group by the provided variable. Are you sure you didn't mispell it?")
                                           print(e)
                                           stop()}
                                         ) %>%
                                ggplot(aes(x = reorder(!!as.name(grouped_var), val), 
                                           y = val,
                                           text = paste0(xlab, ": ", !!as.name(grouped_var), "\n",
                                                         ylab, ": ", format(val, big.mark = " ")))) +
                                geom_col(fill = my_color) + 
                                coord_flip() +
                                theme_minimal() +
                                theme(panel.grid.major.y = element_blank(), axis.title.y = element_text(vjust=-1.5)) +
                                xlab(xlab) +
                                ylab(ylab) +
                                ggtitle(title),
                              tooltip = c("text")
                              
                            )
                            
                          })
                          
                      },
                      
                      plot_server2 = function(input, output, session, id = self$id, data = self$data, sum_var, grouped_var, xlab = "", ylab = "", title = "", 
                                              my_color = "palegreen", FUN = sum, ...){
                        
                        output$barplot2 <- renderPlotly({
                          
                          if(!is.data.frame(data())){
                            stop('Data argument must be a data.frame object')
                          }
                          
                          if(is.matrix(col2rgb(my_color)) == F){
                            stop('Please provide a valid colour representation')
                          }
                          
                          df <- data()
                          
                          if(nrow(df) == 0) return(ggplotly(ggplot()+theme_minimal()))
                          
                          ggplotly(
                            tryCatch(expr = {
                              df %>% 
                                group_by(!!as.name(grouped_var)) %>% 
                                summarize(val=FUN(!!as.name(sum_var), ...)) %>%
                                eval},
                              error = function(e){
                                message("Could not group by the provided variable. Are you sure you didn't mispell it?")
                                print(e)
                                stop()}
                            ) %>%
                              ggplot(aes(x = reorder(!!as.name(grouped_var), val), 
                                         y = val,
                                         text = paste0(xlab, ": ", !!as.name(grouped_var), "\n",
                                                       ylab, ": ", format(val, big.mark = " ")))) +
                              geom_col(fill = my_color) + 
                              coord_flip() +
                              theme_minimal() +
                              theme(panel.grid.major.y = element_blank(), axis.title.x = element_text(vjust=-0.5)) +
                              xlab(xlab) +
                              ylab(ylab) +
                              ggtitle(title),
                            tooltip = c("text")
                            
                          )
                          
                        })
                        
                      },
                      
                      
                      leaflet_server_heatmap = function(input, output, session, data = self$data, icons = self$icons, geojson = self$geojson){
                          
                          
                          output$leaflet_map <- renderLeaflet({
                            
                            if(!is.data.frame(data())){
                              stop('Data argument must be a data.frame object')
                            }
                            
                            df <- data()
                            
                            if(nrow(df) >= 500){
                            
                            leaflet(df) %>%
                              addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 18)) %>%
                              addHeatmap(lng= ~lon, lat = ~lat, intensity = 0.5,
                                         blur = 20, max = 400, radius = 15, cellSize = 3) %>%   
                              setView(
                                lat=41.3991,
                                lng=2.1734,
                                zoom = 13
                              ) %>% 
                              setMaxBounds(
                                lat1 = 41,
                                lat2 = 42,
                                lng1 = 2,
                                lng2 = 2.5
                              )
                            }else{
                              
                              leaflet(df) %>%
                                addTiles(options = providerTileOptions(minZoom = 4, maxZoom = 18)) %>%
                                setView(
                                  lat=41.3991,
                                  lng=2.1734,
                                  zoom = 13
                                ) %>%
                                setMaxBounds(
                                  lat1 = 41,
                                  lat2 = 42,
                                  lng1 = 2,
                                  lng2 = 2.5
                                )  %>%
                                addAwesomeMarkers(
                                  ~lon,
                                  ~lat,
                                  popup = ~popup,
                                  icon = icons
                                )
                              
                            }
                            
                          })
                          
                          observeEvent(req(input$boundaries_module == T), {
                            leafletProxy("leaflet_heatmap", data = geojson) %>%
                              addGeoJSON(geojson)
                          })
                          
                          observeEvent(req(input$boundaries_module == F), {
                            leafletProxy("leaflet_heatmap") %>%
                              clearGeoJSON()
                          })
                          
                          
                          output$downloader_xlsx <- downloadHandler(
                            filename = function() {
                              paste(self$id, "_", Sys.Date(), ".xlsx", sep = "")
                            },
                            content = function(file) {
                              
                              wb <- createWorkbook()
                              addWorksheet(wb, "Data")
                              writeData(wb = wb, sheet = "Data", x = data(), startCol = 1)
                              
                              styl <- createStyle(fontColour = "white", fgFill = "#002060", wrapText = T, textDecoration = "bold", halign = "right")
                              addStyle(wb = wb, sheet = "Data", style = styl, rows = 1, cols = c(1:(length(data()))), gridExpand = TRUE)
                              setColWidths(wb, sheet = "Data", cols = c(1:(length(data()))), widths = "auto")
                              
                              saveWorkbook(wb = wb, file = file)
                            }
                          )
                          
                          
                          
                          output$downloader_csv <- downloadHandler(
                            filename = function() {
                              paste(self$id, "_", Sys.Date(), ".csv", sep = "")
                            },
                            content = function(file) {
                              write.csv(x = data(), file = file, row.names = FALSE, fileEncoding = "CP1250")
                            }
                          )
                          
                          
                          
                          observeEvent(input$info_button, {
                            showModal(modalDialog(
                              title = "Details about the data",
                              self$text,
                              easyClose = TRUE
                            ))
                          })
                      }
                      
                    )
)