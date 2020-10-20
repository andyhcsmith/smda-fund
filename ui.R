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
    choices = c("Percent Change","Capital", "Gains"),
    selected = "Percent Change"
  ),
  
  change_date_picker = selectInput(
    inputId = "change_date",
    label = "Date Picker",
    choices = c("6 Months", "1 Month"),
    selected = "6 Month"
  ),
  
  plot =  echarts4rOutput("main_plot"),
  
  plot_button = actionButton("andy1", "Andy"),
  
  change_plot_button = radioGroupButtons(
    inputId = "change_plot2", label = "",
    choices = c("Capital", "Gains"),
    selected = "Gains", size = "lg", width = "100%",direction = "horizontal", 
  )
  
)
