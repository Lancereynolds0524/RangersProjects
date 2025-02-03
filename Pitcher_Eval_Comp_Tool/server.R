
server <- function(input, output, session) {
  
  # Reactive expression to filter data by year
  filtered_data_year <- reactive({
    req(input$YEAR)
    possible_pitchers[possible_pitchers$YEAR == input$YEAR, ]
  })
  
  # Update organization dropdown based on selected year
  observeEvent(filtered_data_year(), {
    
    req(filtered_data_year())
    
    single_orgs <- filtered_data_year() %>% filter(!grepl(",", ORGANIZATIONS_ABBREV))
    
    updateSelectInput(
      session,
      "ORGANIZATIONS_ABBREV",
      choices = sort(unique(single_orgs$ORGANIZATIONS_ABBREV))
    )
  })
  
  # Reactive expression to filter data by year and organization
  filtered_data_org <- reactive({
    req(filtered_data_year(), input$ORGANIZATIONS_ABBREV)  # Ensure organization is selected
    filtered_data_year() %>% filter(grepl(input$ORGANIZATIONS_ABBREV, ORGANIZATIONS_ABBREV))
  })
  
  observeEvent(filtered_data_org(), {
    
    req(filtered_data_org())
    
    updateSelectInput(
      session,
      "COMPETITION_LEVEL_ABBREV",
      choices = sort(unique(filtered_data_org()$COMPETITION_LEVEL_ABBREV))
    )
  })
  
  # Reactive expression to filter data by year and organization
  filtered_data_comp_level <- reactive({
    req(filtered_data_org(), input$COMPETITION_LEVEL_ABBREV)  # Ensure organization is selected
    filtered_data_org()[filtered_data_org()$COMPETITION_LEVEL_ABBREV == input$COMPETITION_LEVEL_ABBREV, ]
  })
  
  # Update player dropdown based on selected year and organization
  observeEvent(filtered_data_comp_level(), {
    
    req(filtered_data_comp_level())
    
    sorted_data <- filtered_data_comp_level()[order(filtered_data_comp_level()$PLAYER_NAME_DISPLAY_FL), ]
    
    updateSelectInput(
      session,
      "pitchers",
      choices = setNames(sorted_data$TXR_PLAYER_ID, sorted_data$PLAYER_NAME_DISPLAY_FL)
    )
  })
  
  #observeEvent(input$debug, { browser() })
  
  # updateSelectizeInput(session, "pitchers", choices = possible_pitchers_names, server = TRUE)
  
  new_pitchers <- reactive({
    req(input$pitchers, input$YEAR, input$COMPETITION_LEVEL_ABBREV, pitchers_evals_w_mlb, pitchers_evals_w_mlb_scaled, max_distance)
    find_similar_pitchers(pitchers_evals_w_mlb_scaled, pitchers_evals_w_mlb, input$pitchers, input$YEAR, input$COMPETITION_LEVEL_ABBREV, max_distance)
    })
  ten_closest <- reactive({
    req(new_pitchers(), input$pitchers)
    find_10_closest_pitchers(new_pitchers(), input$pitchers)
    })
  top_25_big_league_pct <- reactive({
    req(new_pitchers(), input$pitchers)
    find_top_25_big_league_pct(new_pitchers(), input$pitchers)
    })
  similar_big_league_pct <- reactive({
    req(new_pitchers(), input$pitchers)
    find_similar_big_league_pct(new_pitchers(), input$pitchers)
    })
  ten_closest_preprocess <- reactive({
    req(new_pitchers(), pitchers_evals_w_mlb, ten_closest(), input$pitchers)
    ten_closest_prcs(new_pitchers(), pitchers_evals_w_mlb, ten_closest(), input$pitchers)
  })
  
  output$table1 <- render_gt({
    req(new_pitchers(), input$pitchers)
    expr = focus_pitcher_table(new_pitchers(), input$pitchers)
    })
  output$role_pie_chart <- renderPlot(expr = {
    req(ten_closest())
    pie_chart(ten_closest())
    })
  output$table2 <- render_gt({
    req(new_pitchers(), top_25_big_league_pct(), similar_big_league_pct())
    expr = prob_table(new_pitchers(), top_25_big_league_pct(), similar_big_league_pct())
    })
  output$table3 <- render_gt({
    req(ten_closest_preprocess())
    expr = ten_closest_table_funct(ten_closest_preprocess())
    })
  output$table4 <- render_gt({
    req(ten_closest_preprocess())
    expr = ten_closest_summary_funct(ten_closest_preprocess())
  })
  
  observeEvent(input$go, {
    req(possible_pitchers, input$pitchers, input$YEAR, input$COMPETITION_LEVEL_ABBREV)
    screenshot(id = "screenshotdiv",
               filename = paste0(gsub(" ", "_", unique(possible_pitchers$PLAYER_NAME_DISPLAY_FL[possible_pitchers$TXR_PLAYER_ID == input$pitchers])), "_", input$YEAR, "_", input$COMPETITION_LEVEL_ABBREV))
  })
}