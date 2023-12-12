function(input, output) { 
    
    
    
    reactive_employee <- reactive({    #reactive untuk data bisa berubah ubah 
        
      df %>% 
        select(event_type,admin1,longitude,latitude) %>% 
        filter(df$admin1 == input$inputmap)
        
    })
    
   
    
    
    output$map <- renderLeaflet({
      

      peta <- df %>%
        select(event_type,admin1,admin2,longitude,latitude) %>%
        filter(df$admin1 == input$inputmap1)
        
      
      # peta1 <- peta %>% 
      #   select(event_type,admin1,admin2,longitude,latitude) %>% 
      #   #filter(peta$admin2 == "South Jakarta")
      #   filter(df$admin1 == input$inputmap2)

        map1 <- leaflet(peta)
            map1 <- addTiles(map1)
            map1 <- addMarkers(map1,
                                   label = ~event_type,
                                   popup = ~location
                )
    })
    
    output$fatalities <- renderValueBox({
      
      value_fatalities <- data_ind %>% 
        pull(fatalities) %>%   # pull itu mengganti dari (select) jadi faktor sehingga bisa di mean
        sum() %>% 
        round(2)
      
      valueBox(
        value = value_fatalities, 
        subtitle = "Fatalities",
        icon = icon("skull-crossbones"),
        color = "red", width = 3
      )    
      
    })
    
    output$event <- renderValueBox({
      
      value_event <- data_ind %>% 
        # pull(event_type) %>%   
        # sum() %>% 
        # round(2)
        # nrow()
       summarise(n())
      
      valueBox(
        value = value_event, 
        subtitle = "Violence",
        icon = icon("exclamation-triangle"),
        color = "orange", width = 3
      )    
      
    })
    
    output$avg_fatal <- renderValueBox({
      
      value_avg_fatal <- df %>% 
          filter(event_date >= input$date[1] &
                 event_date <= input$date[2]) %>%
        pull(fatalities, event_date) %>% 
        mean() %>% 
        round(2)
     
      valueBox(
        value = value_avg_fatal, 
        subtitle = "Average Victim",
        icon = icon("skull-crossbones"),
        color = "red", width = 10
      ) 
    })
    
    output$city <- renderValueBox({
      
        value_city <- tibble(selectregion) %>% 
          nrow()
        
        valueBox(
        value = value_city, 
        subtitle = "City",
        icon = icon("city"),
        color = "blue", width = 3
      )    
      
    })
    
    output$province <- renderValueBox({
      
      value_province <- tibble(selectprovince) %>% 
        nrow()
      
      valueBox(
        value = value_province, 
        subtitle = "Province",
        icon = icon("industry"),
        color = "maroon", width = 3
      )    
      
    })
    
 
    output$grafik2 <- renderPlotly({
     
      grafik2 <- df %>% 
        select(event_type,event_date) %>% 
        filter(event_date >= input$date[1] &
                 event_date <= input$date[2]) %>% 
          ggplot(aes(y  = event_type))+
          geom_bar(aes(fill = event_type )) +
          # geom_line()+
          theme(legend.position = "none") +
          labs(
            y = " ",
            x = " "
            
          )
        ggplotly(grafik2, tooltip = T)
      
      
      
    })
    
    output$data <- DT::renderDataTable(
      df, options = list(scrollX = T))
    
    
    offense_word = reactive({
      df %>%
        
        filter(event_type == input$kekerasan1, year >= input$year[1] & year <= input$year[2]) %>%
        group_by(event_type, year[1],year[2], admin2) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(input$max)
    })
    
    output$word = renderPlot({
      wordcloud(
        words = offense_word()$admin2,
        freq = offense_word()$count,
        colors = colorRampPalette(brewer.pal(9, "YlOrRd"))(32),
        random.order = F,
        rot.per = 0.3,
        scale = c(2.8, .9),
        random.color = F,
        max.words = 70
      )
    })
    
    output$hist = renderPlotly({
       offense_word() %>%
        
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE) %>%
        mutate(City = reorder(admin2, count)) %>%
        ggplot(aes(City, count)) +
        geom_bar(stat = "identity",
                 fill = "#73c6d9",
                 color = 'red') +
        ggtitle("Top Rank Violence on City" ) +
        xlab("") +
        ylab("") +
        guides(fill = guide_legend(title = "Numbers")) +
        coord_flip()
    })
      
    
    df_select = reactive({
      df %>%
        filter(event_date >= input$date[1] &
                 event_date <= input$date[2] &
                 event_type %in% input$district)
    })
    
    
    dist_ratio = reactive({
      df %>%
        filter(event_date >= input$date[1] &
                 event_date <= input$date[2]) %>%
        group_by(year, event_type) %>%
        summarise(event_count = n()) %>%
        mutate(total = sum(event_count),
               event_ratio = event_count / total)
    })
    
    df_date = reactive({
      if (input$Byother) {
        if (input$SelectBy == 'year') {
          df_select() %>%
            group_by(event_type, year) %>%
            summarise(Total = n()) 
          
        } else if (input$SelectBy == 'month') {
          df_select() %>%
            group_by(event_type, month) %>%
            summarise(Total = n()) 
          
        } 
        else{
          dist_ratio()
            }
      } else{
        df_select() %>%
          group_by(event_date, event_type) %>%
          summarise(Total = n()) 
         
        
      }
    })
    
    #plot Time series
    output$date_line = renderPlotly({
      if (input$Byother) {
        if (input$SelectBy == 'year') {
          df_date() %>%
            ggplot(aes(x = year, y = Total)) +
            geom_line(aes(color = event_type))
         
        } else if (input$SelectBy == 'month') {
          df_date() %>%
            ggplot(aes(x = month, y = Total)) +
            geom_line(aes(color = event_type))
        } 
        else{
          df_date() %>%
            ggplot(aes(x = year, y = event_ratio)) +
            geom_line(aes(color = event_type))
        }
      } else{
        df_date() %>%
          ggplot(aes(x = event_date, y = Total)) +
          geom_line(aes(color = event_type), se = F)
      }
    })
    

    #-----------------
    
    
      # output$ui <- renderUI({
      #   if (is.null(input$select_A))
      #     return()
      #   
      #   switch(input$select_A,
      #          "1" = selectInput("dynamic", label = "Select A2",
      #                            choices = choices_A),
      #          "2" = selectInput("dynamic", label = "Select B",
      #                            choices = choices_B),
      #          "3" = selectInput("dynamic", label = "Select C",
      #                            choices = choices_C)
      #   )
      # })
      # output$value <- renderPrint({ input$select_A })
      # output$dynamic_value <- renderPrint({ input$dynamic })
      # 
      # 
      # 
      # 
      #   # output$dynamicui <- renderUI({
      #   #   selectInput(inputId = "duct", label = "Select Duct", choices = selectmap$admin2 %in% input$site )
      #   # #              rownames(selectmap)  [selectmap$admin2 %in% input$site])
      #   # })
      #  
      #   output$dynamicui <- renderUI ({
      #     selectInput(inputId = "mapcity", label = "pilih city", choices = dataframe(df)[input$site %in% df$admin2])
      #   })
      # 
       
        
    
  
}