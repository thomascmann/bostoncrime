function(input, output, session) {
  
  # Have selections been changed in the "detailed" incidents tab?
  # If so, deselect everything when mode is switched back to less detailed,
  # to avoid confusion over which incident types are active.
  detailed_select_flag = reactiveVal(FALSE)
  
  ## fixers and utilities -------------------------------------------------------
  
  observeEvent(input$sq_mile_toggle, {
    session$sendCustomMessage(type = 'districtPlot_set', message = character(0))
  })
  
  observeEvent(input$incident_toggle, {
    if (input$incident_toggle == F & detailed_select_flag() == T) {
      updatePickerInput(session, "incidentType", selected = NA)
      updatePickerInput(session, "incidentTypeDetailed", selected = NA)
      detailed_select_flag(FALSE)
    }
    
  }, ignoreInit = T, ignoreNULL = F)
  
  
  observeEvent(input$incidentType, {
    if (input$incident_toggle == FALSE) 
    {
      picked_vals <- incidents_both %>%
        filter(OFFENSE_CODE_GROUP %in% input$incidentType) %>%
        pull(OFFENSE_DESCRIPTION)
      updatePickerInput(session,
                        "incidentTypeDetailed", 
                        selected = picked_vals)
    }
  }, ignoreInit = T, ignoreNULL = F)
  
  observeEvent(input$incidentTypeDetailed, {
    if (input$incident_toggle == T) {
      detailed_select_flag(TRUE)
    }
  }, ignoreInit = T, ignoreNULL = F)
  
  ## ggiraph selectors ---------------------------------------------------------
  
  selected_total <- reactive({
    input$districtPlot_selected
  })
  
  selected_heat <- reactive({
    input$heatmap_selected
  })
  
  selected_cluster <- reactive({
    input$clustermap_selected
  })
  
  selected_kmeans <- reactive({
    input$kmeansmap_selected
  })
  
  ## input validaters and debouncers --------------------------------------------
  
  timeRange <- reactive({
    input$timeRange
  }) %>% debounce(500)
  
  forecast_size <- reactive({
    if (is.null(input$forecast_size) | input$forecast_size<2) {
      updateNumericInput(session, "forecast_size", value = 2)
    }
    if (input$forecast_size>100) {
      updateNumericInput(session, "forecast_size", value = 100)
    }
    input$forecast_size
  }) %>% debounce(1000)
  
  bin_n <- reactive({
    if (is.null(input$bin_n) | input$bin_n<1) {
      updateNumericInput(session, "bin_n", value = 1)
    }
    if (input$bin_n>100) {
      updateNumericInput(session, "bin_n", value = 100)
    }
    input$bin_n
  }) %>% debounce(1000)
  
  kmeans_k <- reactive({
    if (is.null(input$kmeans_k) | input$kmeans_k<2) {
      updateNumericInput(session, "kmeans_k", value = 5)
    }
    input$kmeans_k
  }) %>% debounce(1000)
  
  n_clust_groups <- reactive({
    if (is.null(input$n_clust_groups) | input$n_clust_groups<2) {
      updateNumericInput(session, "n_clust_groups", value = 2)
    }
    if (input$n_clust_groups>10) {
      updateNumericInput(session, "n_clust_groups", value = 10)
    }
    input$n_clust_groups
  }) %>% debounce(1000)
  
  geoweight_clust <- reactive({
    if (is.null(input$geoweight_clust) | input$geoweight_clust<0) {
      updateNumericInput(session, "geoweight_clust", value = 0)
    }
    if (input$geoweight_clust>1) {
      updateNumericInput(session, "geoweight_clust", value = 1)
    }
    input$geoweight_clust
  }) %>% debounce(1000)
  
  selected_neighborhoods <- reactive({
    input$neighborhood
  }) %>% debounce(1000)
  
  selected_days <- reactive({
    input$weekdays
  }) %>% debounce(1000)
  
  selected_types <- reactive({
    input$incidentTypeDetailed
  }) %>% debounce(1000)
  
  ## data pipeline -----------------------------------------------------------
  
  cur_dat <- reactive({
    timeRange <- timeRange()
    session$sendCustomMessage(type = 'districtPlot_set', message = character(0))
    session$sendCustomMessage(type = 'heatmap_set', message = character(0))
    session$sendCustomMessage(type = 'clustermap_set', message = character(0))
    session$sendCustomMessage(type = 'kmeansmap_set', message = character(0))
    bin_n <- bin_n()
    
    cur_dat <- dat %>%
      mutate(
        x = cut_interval(Long, bin_n, labels = F),
        y = cut_interval(Lat, bin_n, labels = F),
        xunit = (max(Long, na.rm=T) - min(Long, na.rm=T))/bin_n,
        yunit = (max(Lat, na.rm=T) - min(Lat, na.rm=T))/bin_n,
        Long_cent = min(Long, na.rm=T) + x*xunit - xunit/2,
        Lat_cent = min(Lat, na.rm=T) + y*yunit - yunit/2
      ) %>%
      filter(
        OCCURRED_ON_DATE >= ymd(input$dateRange[[1]]),
        OCCURRED_ON_DATE < ymd(input$dateRange[[2]]) + days(1),
        HOUR >= timeRange[[1]],
        HOUR < timeRange[[2]],
        Name %in% selected_neighborhoods() | is.na(Name),
        DAY_OF_WEEK %in% selected_days(),
        OFFENSE_DESCRIPTION %in% selected_types()
      )
    cur_dat
  })
  
  loc_dat <- reactive({
    cur_dat <- cur_dat() %>%
      filter(!is.na(Name))
  })
  
  district_agg <- reactive({
    district_agg <- loc_dat() %>%
      group_by(Name, SqMiles) %>%
      summarise(
        crimecount = n()
      ) %>%
      mutate(
        crime_sqmi = crimecount/SqMiles
      )
  })
  
  plot_dat <- reactive({
    district_agg <- district_agg()
    plot_dat <- districts %>%
      left_join(district_agg, by = c("Name", "SqMiles"))
  })
  
  binned_loc_dat <- reactive({
    loc_dat <- loc_dat()
    #bin_n <- bin_n()
    validate(need(nrow(loc_dat) > 0, "No data available"))
    loc_dat 
  })
  
  reduced_locs <- reactive({
    binned_loc_dat() %>%
      select(Long_cent, Lat_cent, xunit, yunit) %>%
      arrange(Lat_cent, Long_cent) %>%
      unique()
  })
  
  heat_dat <- reactive({
    binned_loc_dat <- binned_loc_dat()
    heat_dat <- binned_loc_dat %>%
      group_by(Long_cent, Lat_cent, xunit, yunit) %>%
      summarize(crime = n()) %>%
      mutate(
        tooltip = paste("Latitude:", Lat_cent, "\nLongitude:", Long_cent,
                        "\nIncident count:", crime),
        dataid = paste(Long_cent, Lat_cent, sep = "_")
      ) %>%
      ungroup()
  })
  
  cluster_raw_dat <- reactive({
    validate(
      need(
        bin_n()*2 >= n_clust_groups(), "The total number of data bins must 
be larger than the number of clusters."
      )
    )
    binned_loc_dat <- binned_loc_dat()
    cluster_dat <- binned_loc_dat %>%
      arrange(Lat_cent, Long_cent) %>%
      group_by(Lat_cent, Long_cent, OFFENSE_CODE_GROUP) %>%
      summarize(crime = n()) %>%
      spread(OFFENSE_CODE_GROUP, crime, fill = 0) %>%
      ungroup() %>%
      select(-Lat_cent, -Long_cent)
  })
  
  cluster_dat_tree <- reactive({
    cluster_crime_dist <- cluster_crime_dist()
    cluster_geo_dist <- cluster_geo_dist()
    reduced_locs <- reduced_locs()
    
    validate(
      need(
        try(
          tree <- hclustgeo(cluster_crime_dist, cluster_geo_dist, 
                            alpha = geoweight_clust())
        ),
        "Insufficient data for clustering solution."
      )
    )
    tree
  })
  
  cluster_dat <- reactive({
    cluster_dat_tree <- cluster_dat_tree()
    clusters <- cutree(cluster_dat_tree, n_clust_groups())
    cluster_dat <- reduced_locs() %>%
      mutate(
        cluster = factor(clusters),
        tooltip = paste("Latitude:", Lat_cent, "\nLongitude:", Long_cent,
                        "\nCluster:", cluster),
        dataid = paste(Long_cent, Lat_cent, sep = "_")
      )
  })
  
  kmeans_dat <- reactive({
    kmeans_mod <- kmeans_mod()
    kmeans_dat <- reduced_locs() %>%
      mutate(
        cluster = factor(kmeans_mod$cluster, exclude = c(0)),
        tooltip = paste("Latitude:", Lat_cent, "\nLongitude:", Long_cent,
                        "\nCluster:", cluster),
        dataid = paste(Long_cent, Lat_cent, sep = "_")
      )
  })
  
  ## Text output --------------------------------------------------------------
  
  output$sq_mile_header <- renderText({
    if (input$sq_mile_toggle) "Incidents per Square Mile"
    else "Total Incidents"
  })
  
  ## Table output ------------------------------------------------------------
 
  output$alldata <- renderDataTable({
    if (input$nolatlong == F) cur_dat <- loc_dat()
    else cur_dat <- cur_dat()
    out <- cur_dat %>%
      select(INCIDENT_NUMBER:Location)
    if( nrow(out) < 1 ) return(NULL)
    #row.names(out) <- NULL
    out
  }, options = list(pageLength = 10, width="100%", scrollX = TRUE))
  
  output$datatab <- renderDataTable({
    district_agg <- district_agg()
    out <- district_agg %>%
      filter(Name %in% selected_total()) %>%
      rename(`Incident Count` = crimecount, 
             `Incidents per Square Mile` = crime_sqmi)
    if( nrow(out) < 1 ) return(NULL)
    #row.names(out) <- NULL
    out
  })
  
  output$clustertab <- renderDataTable({
    cluster_dat <- cluster_dat()
    out <- cluster_dat %>%
      filter(paste(Long_cent, Lat_cent, sep = "_") %in% selected_cluster()) %>%
      select(Lat_cent, Long_cent, cluster) %>%
      rename(Cluster = cluster, 
             Longitude = Long_cent,
             Latitude = Lat_cent) %>%
      mutate(Link = createMapLink(Latitude, Longitude))
    if( nrow(out) < 1 ) return(NULL)
    #row.names(out) <- NULL
    out
  }, escape = F)
  
  output$kmeanstab <- renderDataTable({
    kmeans_dat <- kmeans_dat()
    out <- kmeans_dat %>%
      filter(paste(Long_cent, Lat_cent, sep = "_") %in% selected_kmeans()) %>%
      select(Lat_cent, Long_cent, cluster) %>%
      rename(Cluster = cluster, 
             Longitude = Long_cent,
             Latitude = Lat_cent) %>%
      mutate(Link = createMapLink(Latitude, Longitude))
    if( nrow(out) < 1 ) return(NULL)
    #row.names(out) <- NULL
    out
  }, escape = F)
  
  output$heattab <- renderDataTable({
    heat_dat <- heat_dat()
    out <- heat_dat %>%
      filter(paste(Long_cent, Lat_cent, sep = "_") %in% selected_heat()) %>%
      select(Lat_cent, Long_cent, crime) %>%
      rename(`Incident Count` = crime, 
             Longitude = Long_cent,
             Latitude = Lat_cent) %>%
      mutate(Link = createMapLink(Latitude, Longitude))
    if( nrow(out) < 1 ) return(NULL)
    #row.names(out) <- NULL
    out
  }, escape = F)
  
  ## table downloads -------------------------------------------------------------
  
  output$download_all <- downloadHandler(
    filename = "bostoncrime.csv",
    content = function(file) {
      write.csv(
        if (input$nolatlong == F) loc_dat() %>% select(INCIDENT_NUMBER:Location)
        else cur_dat() %>% select(INCIDENT_NUMBER:Location), 
        file, row.names = FALSE)
    }
  )
  
  output$download_datatab <- downloadHandler(
    filename = "bostoncrime_neighborhoods.csv",
    content = function(file) {
      write.csv(
        district_agg() %>%
          filter(Name %in% selected_total()) %>%
          rename(`Incident Count` = crimecount, 
                 `Incidents per Square Mile` = crime_sqmi), 
        file, row.names = FALSE)
    }
  )
  
  output$download_clustertab <- downloadHandler(
    filename = "bostoncrime_hclust.csv",
    content = function(file) {
      write.csv(
        cluster_dat() %>%
          filter(paste(Long_cent, Lat_cent, sep = "_") %in% selected_cluster()) %>%
          select(Lat_cent, Long_cent, cluster) %>%
          rename(Cluster = cluster, 
                 Longitude = Long_cent,
                 Latitude = Lat_cent) %>%
          mutate(Link = sprintf('https://www.google.com/maps/search/?api=1&query=%s,%s',
                                Latitude, Longitude)), 
        file, row.names = FALSE)
    }
  )
  
  output$download_kmeanstab <- downloadHandler(
    filename = "bostoncrime_kmeans.csv",
    content = function(file) {
      write.csv(
        kmeans_dat() %>%
          filter(paste(Long_cent, Lat_cent, sep = "_") %in% selected_kmeans()) %>%
          select(Lat_cent, Long_cent, cluster) %>%
          rename(Cluster = cluster, 
                 Longitude = Long_cent,
                 Latitude = Lat_cent) %>%
          mutate(Link = sprintf('https://www.google.com/maps/search/?api=1&query=%s,%s',
                                Latitude, Longitude)), 
        file, row.names = FALSE)
    }
  )
  
  output$download_heattab <- downloadHandler(
    filename = "bostoncrime_heatmap.csv",
    content = function(file) {
      write.csv(
        heat_dat() %>%
          filter(paste(Long_cent, Lat_cent, sep = "_") %in% selected_heat()) %>%
          select(Lat_cent, Long_cent, crime) %>%
          rename(`Incident Count` = crime, 
                 Longitude = Long_cent,
                 Latitude = Lat_cent) %>%
          mutate(Link = sprintf('https://www.google.com/maps/search/?api=1&query=%s,%s',
                                Latitude, Longitude)), 
        file, row.names = FALSE)
    }
  )
  
  output$download_arimatab <- downloadHandler(
    filename = "bostoncrime_timeseries.csv",
    content = function(file) {
      ts_mod <- autoArimaForecast()
      write.csv(
        tibble(crime=as.matrix(ts_mod$x), 
               Date=as.character(yearmon(time(ts_mod$x)))) %>% 
          bind_rows(as_tibble(ts_mod, rownames = "Date")) %>% 
          select(Date, crime, `Point Forecast`:`Hi 95`) %>%
          rename(`Incident Count` = crime),
        file,
        row.names = FALSE
      )
    }
  )
  
  ## distances & clusterings -----------------------------------------------------
  
  cluster_crime_dist <- reactive({
    cluster_raw_dat <- cluster_raw_dat()
    
    cluster_crime_dist <- dist(scale(cluster_raw_dat))
  })
  
  cluster_geo_dist <- reactive({

    geo_dat <- binned_loc_dat() %>%
      arrange(Lat_cent, Long_cent) %>%
      select(Lat_cent, Long_cent) %>%
      unique()
    
    cluster_geo_dist <- dist(geo_dat)
  })
  
  kmeans_mod <- reactive({
    cluster_raw_dat <- cluster_raw_dat()
    kmeans_mod <- kmeans(cluster_raw_dat, centers = kmeans_k(), nstart = 25)
  })
  
  ## plots --------------------------------------------------------------------
  
  output$districtPlot <- renderggiraph({
    plot_dat <- plot_dat()
    if (input$sq_mile_toggle) plot_dat$dv <- plot_dat$crime_sqmi
    else plot_dat$dv <- plot_dat$crimecount
    x <- girafe(ggobj = ggplot(plot_dat, aes(long, lat, fill = dv)) +
                  geom_polygon_interactive(
                    aes(data_id = Name, 
                        tooltip = paste(Name, "\nIncidents: ",
                                        crimecount, "\nPer Sq Mile: ",
                                        round(crime_sqmi, 2)), 
                        group = group)
                    ) +
                  coord_map() +
                  theme_void() +
                  labs(
                    fill = NULL
                  ))
    x <- girafe_options(x, opts_selection(
      type = "multiple", css = "stroke:gold;"),
      opts_hover(css = "stroke:grey;cursor:pointer;"))
    x
  })

  output$kmeansmap <- renderggiraph({
    kmeans_dat <- kmeans_dat()
    color_n <- length(levels(kmeans_dat$cluster))
    x <- girafe(ggobj = ggplot(kmeans_dat, aes(Long_cent, Lat_cent, width = xunit, 
                                               height = yunit, fill = cluster, 
                                               tooltip = tooltip,
                                               data_id = dataid)) +
                  geom_tile_interactive() +
                  geom_path(data = districts, aes(long, lat, group = group), 
                            inherit.aes = F) +
                  coord_map() +
                  theme_void() +
                  scale_fill_manual(values = colorRampPalette(
                    brewer.pal(9,"Set1"))(color_n)
                  )
    )
    x <- girafe_options(x, opts_selection(
      type = "multiple", css = "stroke:gold;"),
      opts_hover(css = "stroke:grey;cursor:pointer;"))
    x
  })
  
  output$kmeans_sil_plot <- renderPlot({
    if (input$kmeans_silhouette == T) {
      cluster_raw_dat <- cluster_raw_dat()
      fviz_nbclust(cluster_raw_dat, kmeans, method='silhouette')
    }
  })
  
  output$clustermap <- renderggiraph({
    cluster_dat <- cluster_dat()
    color_n <- length(levels(cluster_dat$cluster))
    x <- girafe(ggobj = ggplot(cluster_dat, aes(Long_cent, Lat_cent, 
                                                width = xunit, 
                                                height = yunit, 
                                                fill = cluster, 
                                                tooltip = tooltip,
                                                data_id = dataid)) +
                  geom_tile_interactive() +
                  geom_path(data = districts, aes(long, lat, group = group), 
                            inherit.aes = F) +
                  coord_map() +
                  theme_void() +
                  scale_fill_manual(values = colorRampPalette(
                    brewer.pal(9,"Set1"))(color_n)
                  )
    )
    x <- girafe_options(x, opts_selection(
      type = "multiple", css = "stroke:gold;"),
      opts_hover(css = "stroke:grey;cursor:pointer;"))
    x
  })
  
  output$hclust_sil_plot <- renderPlot({
    if (input$hclust_silhouette == T) {
      cluster_raw_dat <- cluster_raw_dat()
      fviz_nbclust(cluster_raw_dat, hcut, method='silhouette')
    }
  })
  
  output$hclust_geo_sil_plot <- renderPlot({
    if (input$hclust_silhouette == T) {
      validate(
        need(
          bin_n()*2 >= n_clust_groups(), "The total number of data bins must 
be larger than the number of clusters."
        )
      )
      
      geo_dat <- binned_loc_dat() %>%
        arrange(Lat_cent, Long_cent) %>%
        select(Lat_cent, Long_cent) %>%
        unique()
      
      fviz_nbclust(geo_dat, hcut, method='silhouette')
    }
  })
  
  output$hclust_alpha_plot <- renderPlot({
    if (input$hclust_silhouette == T) {
      plot(choicealpha(cluster_crime_dist(), cluster_geo_dist(),
                       range.alpha = seq(0,1,0.1), K = n_clust_groups()))
    }
  })
  
  output$heatmap <- renderggiraph({
    heat_dat <- heat_dat()
    x <- girafe(ggobj = ggplot(heat_dat, 
                               aes(Long_cent, Lat_cent, width = xunit, 
                                   height = yunit, fill = crime, tooltip = tooltip,
                                   data_id = dataid)) +
                  geom_tile_interactive() +
                  geom_path(data = districts, aes(long, lat, group = group), inherit.aes = F) +
                  coord_map() +
                  theme_void() +
                  labs(fill = NULL))
    
    x <- girafe_options(x, opts_selection(
      type = "multiple", css = "stroke:gold;"),
      opts_hover(css = "stroke:grey;cursor:pointer;"))
    x
  })
  
  autoArimaForecast <- reactive({
    start_date <- ymd(input$dateRange[[1]])
    end_date <- ymd(input$dateRange[[2]])
    if (mday(start_date) == 1) {
      first_month <- month(start_date)
      first_year <- year(start_date)
    }
    else {
      first_month <- month(start_date + months(1))
      first_year <- year(start_date + months(1))
    }
    if (mday(end_date) == days_in_month(end_date)) {
      last_month <- month(end_date)
      last_year <- year(end_date)
    }
    else {
      last_month <- month(end_date - months(1))
      last_year <- year(end_date - months(1))
    }
    validate(
      need(
        ymd(paste(last_year, last_month, "01", sep = "-")) - 
          ymd(paste(first_year, first_month, "01", sep = "-")) > 60, 
        "Please specify a broader time range."
      )
    )
    if (input$nolatlong == F) cur_dat <- loc_dat()
    else cur_dat <- cur_dat()
    dat_month <- cur_dat %>%
      group_by(YEAR, MONTH) %>%
      summarise(
        crime_count = n()
      ) %>%
      filter(YEAR > first_year | 
               (YEAR == first_year & MONTH >= first_month),
             YEAR < last_year | (YEAR == last_year & MONTH <= last_month)
      ) %>%
      ungroup()
    
    # Fill in 0s for missing months
    
    dat_month <- dat_month %>%
      mutate(
        Date = as.Date(ymd(paste(YEAR, MONTH, "01", sep = "-")))
      ) %>%
      complete(Date = seq.Date(ymd(paste(first_year, first_month, "01", sep = "-")), 
                               ymd(paste(last_year, last_month, "01", sep = "-")), 
                               by="month")) %>%
      mutate(crime_count = if_else(is.na(crime_count), 0L, crime_count))
    dat_m_ts <- ts(dat_month$crime_count, 
                   start = c(first_year, first_month), 
                   end = c(last_year, last_month),
                   frequency = 12)
    validate(
      need(
        if (input$abovezero==T)
        {
          try(mod1 <- auto.arima(dat_m_ts/monthdays(dat_m_ts),
                                 lambda = 0, biasadj = T))
        }
        else {
          mod1 <- auto.arima(dat_m_ts/monthdays(dat_m_ts))
        },
        "Model could not be fit."
      )
    )
    cur_forecast <- forecast(mod1, h = forecast_size())
  })
  
  output$autoArimaMonthPlot <- renderPlot({
    autoplot(autoArimaForecast()) +
      labs(
        y = "Incident Reports per Day (Monthly Average)",
        x = "Year"
      ) +
      theme_minimal()
  })
}