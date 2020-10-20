###############################################################################
# Defining Server Logic behind App to explore UBER data
#
# Author: Vivek Katial
# Created 2019-01-30 20:32:44
###############################################################################

server <- function(input, output) {
  
  picks = getPicksData()
  data = runPriceFetcher()
  
  
  # Basic Numbers Page --------------------------------------------------------------
  
  output$best_company <- renderText({
    data %>%
      filter(date == max(date)) %>%
      filter(capital == max(capital)) %>%
      select(company) %>%
      as.character()
  })
  
  output$top_analyst <- renderText({
    req(data)
  
    data %>%
      filter(date == max(date)) %>%
      group_by(analyst) %>%
      summarise(stake = sum(stake), capital = sum(capital)) %>%
      ungroup() %>%
      filter(stake != 0) %>%
      mutate(gains = capital/stake) %>%
      filter(gains == max(gains)) %>%
      select(analyst) %>%
      as.character()

  })
  
  output$num_installments = renderText({
    picks %>%
      filter(pick == "Actual") %>%
      nrow()
  })
  
  output$principal_invested = renderText({
    sum(picks$stake)
  })
  
  output$capital_gains = renderText({
    total_now = data %>%
                filter(date == max(date)) %>%
                select(capital) %>% 
                sum()
    investment = sum(picks$stake)
    
    total_now - investment
    
  })
  
  output$overall_performance = renderText({
    total_now = data %>%
      filter(date == max(date)) %>%
      select(capital) %>% 
      sum()
    investment = sum(picks$stake)
    
    round(((total_now - investment)/investment), 2) * 100
  })
  
  # Plots ---------------------------------------------------------------------------
  
  output$main_plot <- renderEcharts4r({
    req(data)
    req(input$change_date)
    req(input$analyst_pick)
    
    plot_data <- data
    
    # Filtering data
    if(input$analyst_pick != "All"){
      plot_data <- plot_data %>%
        filter(analyst %in% input$analyst_pick)
    }
    
    if(input$change_date == "1 Month"){
      plot_data <- plot_data %>%
        filter(date >= (today() - 31))
    } 
    
    # Outputs the desired plot
    if(input$change_plot == "Capital"){
      
      plot_data %>%
        filter(capital != 0) %>%
        group_by(ticker) %>%
          e_chart(x=date) %>%
          e_line(capital) %>%
          e_title("Capital by Company", left = 'center') %>%
          e_legend(orient = "horizontal", top = "bottom")
      
    } else if(input$change_plot == "Total ($)"){
      
      plot_data %>%
        group_by(date) %>%
        summarise(`Total Capital` = sum(capital, na.rm = T)) %>%
        ungroup() %>%
        e_chart(x=date) %>%
        e_line(`Total Capital`) %>%
        e_title("Total Capital ($)", left = 'center') %>%
        e_legend(show = FALSE)
    } else if(input$change_plot == "Gains"){
      
      plot_data %>%
        mutate(gains = capital - stake) %>%
        group_by(date) %>%
        summarise(`Total Gains` = sum(gains, na.rm = T)) %>%
        ungroup() %>%
        e_chart(x=date) %>%
        e_line(`Total Gains`) %>%
        e_title("Total Gains ($)", left = 'center') %>%
        e_legend(show = FALSE)
      
    } else if(input$change_plot == "Percent Change"){
      
      plot_data %>%
        mutate(gains = ((share_price/start_price)-1)*100) %>%
        group_by(company) %>%
        e_chart(x=date) %>%
        e_line(gains) %>%
        e_title("Stock Performance", left = 'center')
    }

  })


}
