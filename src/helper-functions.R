###############################################################################
# Helper functions for the SMDA app
#
# Author: Andrew Smith
# Created 2020-10-02
###############################################################################

# Get our picks from the google sheet
getPicksData <- function(){

  # Using the googlesheets4 package to get data
  picks = read_sheet("1KjvNfNHIX8sOH5JOzAOXFdrVDZdaGrZljIxXEJ66uNk")
  picks = picks[!duplicated(picks$ticker),]
  
  return(picks)
}

# Takes a single company and gets the share prices between two dates
getSharePrices <- function(pick){
  
  if(is.na(pick$date_sold)){pick$date_sold = today()}
  share_prices = data.frame(getSymbols(pick$ticker, from = pick$date_picked, to = pick$date_sold, auto.assign = FALSE, warnings = TRUE))
  share_prices <- tibble::rownames_to_column(share_prices, "date")
  share_prices <- share_prices %>% mutate(date = gsub("X", "", date))
  share_prices <- share_prices[,c(1,2)]
  colnames(share_prices) <- c("date","share_price")
  
  # Merging with other details
  share_prices <- merge(pick, share_prices)
  share_prices <- share_prices %>% 
    mutate(date = ymd(date)) %>%
    filter(!is.na(date)) %>%
    filter(!(weekdays(date) %in% c("Saturday","Sunday")))
  
  # How many shares did we purchase
  start_price <- share_prices %>% filter(date == date_picked) %>% select(share_price) %>% as.numeric()
  share_prices <- share_prices %>%
    mutate(start_price = start_price) %>%
    mutate(num_shares = stake/start_price) %>%
    mutate(capital = num_shares * share_price) %>%
    mutate(gains = ((share_price/start_price)-1)*100)
  
  return(share_prices)
}

runPriceFetcher <- function(){
  picks <- picks %>% mutate(date_picked = ymd(date_picked),
                            date_sold = ymd(date_sold))
  
  all_prices = data.frame()
  for (i in 1:nrow(picks)){
    pick = picks %>% slice(i)
    prices = getSharePrices(pick)
    all_prices = rbind.data.frame(all_prices, prices)
  }
  
  all_prices <- all_prices %>% filter(!is.na(share_price))
  return(all_prices) 
}

# Takes a stock and works out return between two dates
returnCalculator <- function(data, group_variable, days){
  
  if(days == 1){
    companyData = data %>% group_by(analyst, get(group_variable))
    comparison_price = companyData %>% 
      filter(date != max(date)) %>% 
      filter(date == max(date)) %>%
      select(share_price) %>% rename("comp_price" = "share_price")
    
  } else{
    date_from = today() - days
    companyData = data %>% group_by(analyst, get(group_variable)) %>% filter(date >= date_from)
    comparison_price = companyData %>% filter(date == min(date)) %>% select(share_price) %>% rename("comp_price" = "share_price")
  }
  
  todays_price = companyData %>% filter(date == max(date)) %>% select(share_price, num_shares) %>% 
    rename("Share Price" = "share_price")
  comparisonData <- inner_join(todays_price, comparison_price, by = "get(group_variable)")
  comparisonData <- comparisonData %>% 
    mutate(num_shares = round(num_shares,2),
           percent_gain = round(100*((`Share Price`/comp_price)-1),2)) %>%
    select(-comp_price)
  colnames(comparisonData)[1:2] = c("analyst", group_variable)
  
  return(comparisonData)
}

# Takes a stock and works out return between two dates (and returns down to a daily level)
returnCalculatorDaily <- function(data){
  
  baseline <- data %>% group_by(company, .groups = "keep") %>% 
    filter(date == min(date)) %>% 
    select(company, share_price) %>% 
    rename("baseline" = "share_price") %>%
    ungroup()
  
  relative_data <- merge(data, baseline, by = "company")
  relative_data <- relative_data %>% mutate(relative_gain = round((share_price/baseline)-1, 2)*100)
  
  return(relative_data)
}

