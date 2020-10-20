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
  plot =  echarts4rOutput("main_plot"),

  change_plot_button = radioGroupButtons(
    inputId = "change_plot", label = "",
    choices = c("Capital", "Gains"),
    selected = "Gains", size = "lg", width = "100%",direction = "horizontal", 
  ),
  
  # andy1 = actionButton("andy_1", label = "andy1"),
  # 
  # andy2 = actionButton("andy_2", label = "andy2"),
  
  # change_plot_button = prettyRadioButtons(
  #   inputId = "change_plot", label = "", fill = T,
  #   choices = c("Total ($)", "Share Prices", "Capital"),
  #   selected = "Total ($)",
  #  status = "primary"
  # )
  
)
