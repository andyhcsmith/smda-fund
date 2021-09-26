###############################################################################
# UI Access for Dashboard
#
# Author: Andrew Smith
# Created 2020-10-02
###############################################################################

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",

  # Basic Numbers -------------------------------------
  main_quote_text = textOutput(
    "main_quote",
    inline = T
  ),
  
  top_analyst_text = textOutput(
    "top_analyst",
    inline = T
  ),
  
  best_company_text = textOutput(
    "best_company",
    inline = T
  ),
  
  num_installments_text = textOutput(
    "num_installments",
    inline = T
  ),
  
  principal_invested_text = textOutput(
    "principal_invested",
    inline = T
  ),
  
  capital_gains_text = textOutput(
    "capital_gains",
    inline = T
  ),
  
  overall_performance_text = textOutput(
    "overall_performance",
    inline = T
  ),
  
  # ValueBoxes ----------------------------------------
  
  top_analyst_valuebox = valueBoxOutput("top_analyst_box"),
  
  # Tables --------------------------------------------
  
  table_name = textOutput(
    "table_name_text",
    inline =T
  ),
  
  table_analyst_but = actionButton(inputId = "table_analyst_button", 
                                    label = "Leaderboard",
                                    style="color: #39b54a; background-color: #000000; border-color: #39b54a"),
  
  table_shares_but = actionButton(inputId = "table_shares_button", 
                                  label = "Holdings",
                                   style="color: #39b54a; background-color: #000000; border-color: #39b54a"),
  table_picker = selectInput(
    inputId = "table_pick",
    label = "",
    choices = c("Leaderboard", "Holdings"),
    selected = "Leaderboard"
  ),
  
  leaderboard_table = dataTableOutput(
    "leaderboard"
  ),
  

  # Plots ----------------------------------------------
  
  analyst_picker = selectInput(
    inputId = "analyst_pick",
    label = "Analyst",
    choices = c("All",unique(picks$analyst)),
    selected = "All"
  ),
  
  change_plot_picker = selectInput(
    inputId = "change_plot",
    label = "Gains",
    choices = c("Portfolio Overview", "SMDA Manager" ,"Actual Portfolio", "Virtual Portfolio", "Total Gains"),
    selected = "Actual Portfolio"
  ),
  
  change_date_picker = selectInput(
    inputId = "change_date",
    label = "Date Picker",
    choices = c("Max","6 Months", "1 Month", "1 Week"),
    selected = "1 Month"
  ),
  
  company_plot_picker = selectInput(
    inputId = "change_company",
    label = "Company",
    choices = c("All",unique(picks$company)),
    selected = "All"
  ),
  
  # The slice and dice plot on the fourth page
  plot =  echarts4rOutput("main_plot"),

  # The table showing our actual picks with daily and total change
  actual_picks_table =  dataTableOutput(
    "actual_picks_table"
  )
  
  
)
