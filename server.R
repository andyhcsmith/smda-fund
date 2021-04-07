###############################################################################
# Defining Server Logic behind App to explore UBER data
#
# Author: Vivek Katial
# Created 2019-01-30 20:32:44
###############################################################################

server <- function(input, output) {
  whichTable <- reactiveVal(TRUE)
  
  # Basic Numbers Page --------------------------------------------------------------
  
  output$main_quote <- renderText({
    rand_num = runif(1)
    
    if(rand_num <=0.25){
      quote = "Always remember. What goes up must continue going up."
    }else if(rand_num <= 0.5){
      quote = "Work until your bank account looks like your phone number."
    }else if (rand_num <= 0.75){
      quote = "A winner is just a loser who tried one more time."
    }else{
      quote = "Hard work beats talent every time."
    }
    
    return(quote)
  })
  
  output$best_company <- renderText({
    req(data)
    
    data %>%
      filter(pick == "Actual") %>%
      group_by(company) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      filter(gains == max(gains, na.rm=T)) %>%
      select(company) %>%
      as.character()
  })
  
  output$top_analyst <- renderText({
    req(data)
 
    data %>%     
      group_by(company) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      group_by(analyst) %>%
      summarise(total_gains = mean(gains, na.rm = T)) %>%
      ungroup() %>%
      filter(total_gains == max(total_gains, na.rm=T)) %>%
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
    data %>%
      filter(pick =="Actual") %>%
      group_by(company) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      mutate(actual_gain = capital - stake) %>%
      select(actual_gain) %>%
      sum()
  })
  
  output$overall_performance = renderText({
    temp <- data %>%
            filter(pick =="Actual") %>%
            group_by(company) %>%
            filter(date == max(date)) %>%
            ungroup()
    round((sum(temp$capital) / sum(temp$stake) - 1) * 100, 2)
    
  })

  # ValueBoxes --------------------------------------------------------------------
  
  output$top_analyst_box <- renderValueBox({
    valueBox(
      "Will", "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  
  # Table ---------------------------------------------------------------------------
  
  
  observeEvent(input$table_button, {
    whichTable(!whichTable())
  })
  
  selected_table <- reactive({
    req(data)
    
    if (whichTable()) {
      leaderboard <- data %>%
        group_by(company) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        group_by(analyst) %>%
        summarise(Performance = round(mean(gains, na.rm = T),2)) %>%
        ungroup() %>%
        rename(Analyst = analyst) %>%
        arrange(-Performance)
      
      datatable(leaderboard, 
                options = list(dom = 't',
                               columnDefs = list(list(className = 'dt-center', targets = 0:1))),
                rownames= FALSE)
    } else {

      today_change <- returnCalculator(data, "company",1) %>% rename("Todays Change" = "percent_gain") 
      week_change <- returnCalculator(data, "company", 7)
      total_change <- returnCalculator(data, "company", 1000) %>% rename("Total Change" = "percent_gain") 
      
      overview <- inner_join(today_change, total_change %>% select(company, `Total Change`), by = "company")
      overview <- overview %>% rename("Company" = "company", "Quantity" = "num_shares") %>%
        mutate(`Share Price` = signif(`Share Price`,2))
      
      datatable(overview, 
                options = list(dom = 't',
                               columnDefs = list(list(className = 'dt-center', targets = 0:4))),
                rownames= FALSE)
    }
    
  })
  
  output$leaderboard <- renderDataTable({
    selected_table()
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
    
    if(input$change_date == "Max"){
      plot_data <- returnCalculatorDaily(plot_data)
    } else if(input$change_date == "6 Months"){
      plot_data <- plot_data %>%
        filter(date >= (today() - 180))
      plot_data <- returnCalculatorDaily(plot_data)
    }  else if(input$change_date == "1 Month"){
      plot_data <- plot_data %>%
        filter(date >= (today() - 31))
      plot_data <- returnCalculatorDaily(plot_data)
    } else if(input$change_date == "1 Week"){
      plot_data <- plot_data %>%
        filter(date >= (today() - 7))
      plot_data <- returnCalculatorDaily(plot_data)
    }
    
    # Outputs the desired plot
    if(input$change_plot == "Portfolio Overview"){
      
      plot_data %>%
        group_by(pick, date) %>%
        summarise(gains = mean(relative_gain, na.rm = T)) %>%
        arrange(date) %>%
        ungroup() %>% 
        group_by(pick) %>%
        e_chart(x=date) %>%
        e_line(gains) %>%
        e_title("Portfolio Overview - Return %", left = 'center') %>%
        e_legend(orient = "horizontal", top = "bottom")
      
    } else if(input$change_plot == "PGI Manager"){
      
      plot_data %>%
        group_by(analyst, date) %>%
        summarise(gains = mean(relative_gain, na.rm = T)) %>%
        arrange(date) %>%
        ungroup() %>% 
        group_by(analyst) %>%
        e_chart(x=date) %>%
        e_line(gains) %>%
        e_title("PGI Manager - Return %", left = 'center') %>%
        e_legend(orient = "horizontal", top = "bottom")
      
    } else if(input$change_plot == "Total Gains"){
      
      plot_data %>%
        filter(pick == "Actual") %>%
        group_by(date) %>%
        summarise(`Total Gains` = sum(relative_gain, na.rm = T)) %>%
        arrange(date) %>%
        ungroup() %>%
        e_chart(x=date) %>%
        e_line(`Total Gains`) %>%
        e_title("Total Gains ($))", left = 'center') %>%
        e_legend(show = FALSE)
      
    } else if(input$change_plot == "Actual Portfolio"){
      
      plot_data %>%
        filter(pick == "Actual") %>%
        group_by(company) %>%
        arrange(date) %>%
        e_chart(x=date) %>%
        e_line(relative_gain) %>%
        e_title("PGI Manager - Return %", left = 'center') %>%
        e_legend(orient = "horizontal", top = "bottom")
      
    } else if(input$change_plot == "Virtual Portfolio"){
      
      plot_data %>%
        group_by(company) %>%
        arrange(date) %>%
        e_chart(x=date) %>%
        e_line(relative_gain) %>%
        e_title("PGI Manager - Return %", left = 'center') %>%
        e_legend(orient = "horizontal", top = "bottom", type = c("plain", "scroll"))
      
    }

  })


}
